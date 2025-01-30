-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2024 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Václav Paťorek <xpator00@stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_INV  : out std_logic;                      -- pozadavek na aktivaci inverzniho zobrazeni (1)
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

    -- Pomocné výstupní registry pro komunikaci s okolím
    signal output_enable : std_logic := '0';          -- Povolení výstupu dat na výstupní port
    signal output_reg : std_logic_vector(7 downto 0) := (others => '0');  -- Registr pro data k odeslání
    signal input_active : std_logic := '0';          -- Signál indikující aktivní vstupní operaci
    
    -- Řídicí signály pro práci s registry a ukazateli
    signal pc_forward : std_logic := '0';     -- Signál pro inkrementaci programového čítače
    signal pc_backward : std_logic := '0';    -- Signál pro dekrementaci programového čítače
    signal ptr_forward : std_logic := '0';    -- Signál pro posun datového ukazatele vpřed
    signal ptr_backward : std_logic := '0';   -- Signál pro posun datového ukazatele vzad
    signal boot_complete : std_logic := '0';  -- Signál pro dokončení inicializační fáze
    
    -- Definice všech možných stavů procesoru
    type machine_state is (

        phase_boot,           -- Inicializační fáze - hledání oddělovače programu a dat
        phase_fetch,          -- Načítání instrukce z paměti
        phase_process,        -- Dekódování a zpracování instrukce
        phase_ptr_right,      -- Zpracování instrukce '>' (posun ukazatele vpravo)
        phase_ptr_left,       -- Zpracování instrukce '<' (posun ukazatele vlevo)
        phase_data_fetch,     -- Načtení dat z paměti pro modifikaci    
        phase_data_write,     -- Zápis modifikovaných dat zpět do paměti   
        phase_aux_load,       -- Instrukce '$' (načtení do pomocného registru)
        phase_aux_store,      -- Instrukce '!' (uložení z pomocného registru)
        phase_out_prep,       -- Příprava pro výstupní operaci
        phase_out_send,       -- Provedení výstupní operace
        phase_in_prep,        -- Příprava pro vstupní operaci
        phase_in_start,       -- Zahájení vstupní operace
        phase_in_hold,        -- Čekání na platná vstupní data
        phase_in_save,        -- Uložení vstupních dat do paměti
        phase_halt            -- Ukončení činnosti procesoru

    );
    
    -- Registry pro řízení stavového automatu
    signal active_state : machine_state := phase_boot;   -- Aktuální stav procesoru
    signal future_state : machine_state := phase_boot;   -- Následující stav procesoru
    
    -- Registry pro správu programu a dat
    signal op_code : std_logic_vector(7 downto 0) := (others => '0');        -- Registr pro uložení aktuální instrukce
    signal prog_counter : std_logic_vector(12 downto 0) := (others => '0');  -- Programový čítač (adresa další instrukce)
    signal data_pointer : std_logic_vector(12 downto 0) := (others => '0');  -- Ukazatel do datové části paměti
    signal aux_register : std_logic_vector(7 downto 0) := (others => '0');   -- Pomocný registr pro dočasné uložení dat
    
    begin
        -- Propojení výstupních signálů s vnitřními registry
        OUT_WE <= output_enable;      -- Povolení zápisu na výstup
        OUT_DATA <= output_reg;       -- Data k odeslání na výstup
        IN_REQ <= input_active;       -- Požadavek na vstupní data
        OUT_INV <= '0';               -- Inverzní zobrazení vypnuto
        
        -- Multiplexor pro výběr adresy (přepíná mezi datovou a programovou částí paměti, pro operace s daty používáme datový ukazatel a pro načítání instrukcí používáme programový čítač)
        mem_select: DATA_ADDR <= data_pointer when (active_state = phase_data_fetch or active_state = phase_data_write or active_state = phase_aux_load or active_state = phase_aux_store or active_state = phase_out_prep or active_state = phase_out_send or active_state = phase_in_prep or active_state = phase_in_save) else prog_counter;
        
        -- Stavové signály
        READY <= boot_complete;                               -- Indikace dokončení inicializace
        DONE <= '1' when active_state = phase_halt else '0';  -- Indikace konce programu
    
        -- Hlavní výkonná jednotka (synchronní operace řízené hodinovým signálem)
        exec_unit: process(CLK, RESET)

        begin

            if RESET = '1' then

                -- Reset všech registrů na výchozí hodnoty
                active_state <= phase_boot;
                prog_counter <= (others => '0');
                data_pointer <= (others => '0');
                aux_register <= (others => '0');
                op_code <= (others => '0');
                boot_complete <= '0';
                output_enable <= '0';
                output_reg <= (others => '0');
                input_active <= '0';
                
            elsif rising_edge(CLK) then

                if EN = '1' then
                    -- Aktualizace stavu procesoru
                    active_state <= future_state;  -- Přechod do dalšího stavu
                    output_enable <= '0';   -- Reset výstupních signálů
                    input_active <= '0';    -- Reset vstupních signálů
                    
                    -- Inicializační fáze - hledání oddělovače @ v programu
                    if active_state = phase_boot and boot_complete = '0' then

                        if DATA_RDATA = X"40" then              -- Nalezen oddělovač @
                            data_pointer <= prog_counter;       -- Nastavení počátku datové oblasti
                            prog_counter <= (others => '0');    -- Reset PC na začátek programu
                            boot_complete <= '1';               -- Označení konce inicializace

                        else

                            prog_counter <= prog_counter + 1;   -- Pokračování v hledání

                        end if;

                    else
                        -- Zpracování instrukcí v běžném režimu
                        if active_state = phase_process then

                            op_code <= DATA_RDATA;  -- Uložení aktuální instrukce

                        end if;
    
                        -- Řízení programového čítače
                        if pc_forward = '1' then

                            prog_counter <= prog_counter + 1;  -- Inkrementace PC

                        elsif pc_backward = '1' then

                            prog_counter <= prog_counter - 1;  -- Dekrementace PC

                        end if;
                        
                        -- Řízení datového ukazatele s ošetřením přetečení
                        if ptr_forward = '1' then

                            if data_pointer = "1111111111111" then

                                data_pointer <= (others => '0');    -- Přetečení (návrat na začátek)

                            else

                                data_pointer <= data_pointer + 1;   -- Inkrementace

                            end if;

                        elsif ptr_backward = '1' then

                            if data_pointer = "0000000000000" then

                                data_pointer <= "1111111111111";    -- Podtečení (skok na konec)

                            else

                                data_pointer <= data_pointer - 1;   -- Dekrementace

                            end if;

                        end if;
                        
                        -- Operace s pomocným registrem
                        if active_state = phase_aux_load then

                            aux_register <= DATA_RDATA;             -- Načtení hodnoty do pomocného registru

                        end if;
                        
                        -- Řízení výstupu
                        if active_state = phase_out_send and OUT_BUSY = '0' then

                            output_enable <= '1';                   -- Aktivace výstupu
                            output_reg <= DATA_RDATA;               -- Nastavení výstupních dat

                        end if;
    
                        -- Řízení vstupu
                        if active_state = phase_in_start or active_state = phase_in_hold then

                            input_active <= '1';                    -- Aktivace požadavku na vstup

                        end if;

                    end if;

                end if;

            end if;

        end process;
    
        -- Řídící jednotka (výpočet následujícího stavu a řízení signálů)
        state_control: process(active_state, DATA_RDATA, boot_complete, op_code, OUT_BUSY, IN_VLD)

        begin

            -- Výchozí hodnoty řídících signálů
            future_state <= active_state;       -- Zachování aktuálního stavu
            pc_forward <= '0';                  -- Reset řídicích signálů pro PC
            pc_backward <= '0';
            ptr_forward <= '0';                 -- Reset řídicích signálů pro PTR
            ptr_backward <= '0';
            DATA_EN <= '0';                     -- Reset přístupu k paměti
            DATA_RDWR <= '1';                   -- Výchozí režim čtení
            DATA_WDATA <= (others => '0');      -- Reset zapisovaných dat
            
            case active_state is

                when phase_boot =>
                    DATA_EN <= '1';             -- Povolení přístupu k paměti
                    DATA_RDWR <= '1';           -- Režim čtení pro inicializaci
                    if boot_complete = '1' then

                        future_state <= phase_fetch;    -- Přechod k načítání instrukcí

                    end if;
                
                when phase_fetch =>
                    DATA_EN <= '1';             -- Povolení přístupu k paměti
                    DATA_RDWR <= '1';           -- Režim čtení instrukce
                    future_state <= phase_process;      -- Přechod ke zpracování
                
                when phase_process =>
                    DATA_EN <= '1';             -- Povolení přístupu k paměti
                    DATA_RDWR <= '1';           -- Režim čtení instrukce
                    case DATA_RDATA is

                        when X"3E" =>   -- Instrukce > (posun vpravo)
                            future_state <= phase_ptr_right;
                            pc_forward <= '1';

                        when X"3C" =>   -- Instrukce < (posun vlevo)
                            future_state <= phase_ptr_left;
                            pc_forward <= '1';

                        when X"2B" =>   -- Instrukce + (inkrementace)
                            future_state <= phase_data_fetch;
                            pc_forward <= '1';

                        when X"2D" =>   -- Instrukce - (dekrementace)
                            future_state <= phase_data_fetch;
                            pc_forward <= '1';

                        when X"40" =>   -- Instrukce @ (konec)
                            future_state <= phase_halt;

                        when X"24" =>   -- Instrukce $ (uložení do aux)
                            future_state <= phase_aux_load;
                            pc_forward <= '1';

                        when X"21" =>   -- Instrukce ! (načtení z aux)
                            future_state <= phase_aux_store;
                            pc_forward <= '1';

                        when X"2E" =>   -- Instrukce . (výstup)
                            future_state <= phase_out_prep;
                            pc_forward <= '1';

                        when X"2C" =>   -- Instrukce , (vstup)
                            future_state <= phase_in_prep;
                            pc_forward <= '1';

                        when others =>  -- Neznámá instrukce (přeskočit)
                            pc_forward <= '1';
                            future_state <= phase_fetch;

                    end case;
                
                when phase_data_fetch =>

                    DATA_EN <= '1';                     -- Povolení přístupu k paměti
                    DATA_RDWR <= '1';                   -- Režim čtení dat
                    future_state <= phase_data_write;   -- Přechod k zápisu
                    
                when phase_data_write =>

                    DATA_EN <= '1';                     -- Povolení přístupu k paměti
                    DATA_RDWR <= '0';                   -- Režim zápisu
                    if op_code = X"2B" then             -- Inkrementace hodnoty

                        DATA_WDATA <= DATA_RDATA + 1;

                    elsif op_code = X"2D" then          -- Dekrementace hodnoty

                        DATA_WDATA <= DATA_RDATA - 1;

                    end if;

                    future_state <= phase_fetch;
                
                when phase_ptr_right =>

                    ptr_forward <= '1';  -- Posun ukazatele vpravo
                    future_state <= phase_fetch;
                    
                when phase_ptr_left =>

                    ptr_backward <= '1';  -- Posun ukazatele vlevo
                    future_state <= phase_fetch;
                
                when phase_aux_load =>

                    DATA_EN <= '1';     -- Povolení přístupu k paměti
                    DATA_RDWR <= '1';   -- Režim čtení do aux registru
                    future_state <= phase_fetch;
                    
                when phase_aux_store =>

                    DATA_EN <= '1';     -- Povolení přístupu k paměti
                    DATA_RDWR <= '0';   -- Režim zápisu z aux registru
                    DATA_WDATA <= aux_register;
                    future_state <= phase_fetch;
                
                when phase_out_prep =>

                    DATA_EN <= '1';     -- Povolení přístupu k paměti
                    DATA_RDWR <= '1';   -- Režim čtení pro výstup
                    future_state <= phase_out_send;
                    
                when phase_out_send =>

                    DATA_EN <= '1';     -- Povolení přístupu k paměti
                    DATA_RDWR <= '1';   -- Udržení dat pro výstup
                    if OUT_BUSY = '0' then    -- Kontrola dostupnosti výstupu

                        future_state <= phase_fetch;

                    end if;
    
                when phase_in_prep =>

                    DATA_EN <= '1';     -- Povolení přístupu k paměti
                    DATA_RDWR <= '1';   -- Režim čtení pro přípravu vstupu
                    future_state <= phase_in_start;
                    
                when phase_in_start =>

                    future_state <= phase_in_hold;  -- Čekání na vstupní data
                    
                when phase_in_hold =>

                    if IN_VLD = '1' then            -- Kontrola platnosti vstupních dat

                        future_state <= phase_in_save;

                    end if;
                    
                when phase_in_save =>

                    DATA_EN <= '1';     -- Povolení přístupu k paměti
                    DATA_RDWR <= '0';   -- Režim zápisu vstupních dat
                    DATA_WDATA <= IN_DATA;
                    future_state <= phase_fetch;
                    
                when phase_halt =>

                    null;  -- Koncový stav
                    
            end case;

        end process;
        
end behavioral;