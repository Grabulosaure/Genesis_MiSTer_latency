--------------------------------------------------------------------------------
-- Latency Test
--------------------------------------------------------------------------------
-- DO 1/2020
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY latest IS
  GENERIC (
    FREQ  : natural;
    RGB   : unsigned(23 DOWNTO 0) :=x"FFFFFF");
  PORT (
    -- VGA IN
    i_r   : IN  unsigned(7 DOWNTO 0);
    i_g   : IN  unsigned(7 DOWNTO 0);
    i_b   : IN  unsigned(7 DOWNTO 0);
    i_hs  : IN  std_logic;
    i_vs  : IN  std_logic;
    i_fl  : IN  std_logic;
    i_de  : IN  std_logic;
    i_en  : IN  std_logic;
    i_clk : IN  std_logic;

    -- VGA_OUT
    o_r   : OUT unsigned(7 DOWNTO 0);
    o_g   : OUT unsigned(7 DOWNTO 0);
    o_b   : OUT unsigned(7 DOWNTO 0);
    o_hs  : OUT std_logic;
    o_vs  : OUT std_logic;
	 o_fl  : OUT std_logic;
    o_vl  : OUT std_logic;
    o_de  : OUT std_logic;

    ena   : IN  std_logic;
    led   : OUT std_logic;
    sense : IN  std_logic;

    refclk : IN std_logic
    );
END ENTITY latest;

--##############################################################################

ARCHITECTURE rtl OF latest IS
  
  TYPE arr_slv8 IS ARRAY (natural RANGE <>) OF unsigned(7 DOWNTO 0);
  CONSTANT CHARS : arr_slv8 :=(
    x"3E", x"63", x"73", x"7B", x"6F", x"67", x"3E", x"00",  -- 0
    x"0C", x"0E", x"0C", x"0C", x"0C", x"0C", x"3F", x"00",  -- 1
    x"1E", x"33", x"30", x"1C", x"06", x"33", x"3F", x"00",  -- 2
    x"1E", x"33", x"30", x"1C", x"30", x"33", x"1E", x"00",  -- 3
    x"38", x"3C", x"36", x"33", x"7F", x"30", x"78", x"00",  -- 4
    x"3F", x"03", x"1F", x"30", x"30", x"33", x"1E", x"00",  -- 5
    x"1C", x"06", x"03", x"1F", x"33", x"33", x"1E", x"00",  -- 6
    x"3F", x"33", x"30", x"18", x"0C", x"0C", x"0C", x"00",  -- 7
    x"1E", x"33", x"33", x"1E", x"33", x"33", x"1E", x"00",  -- 8
    x"1E", x"33", x"33", x"3E", x"30", x"18", x"0E", x"00",  -- 9
    x"00", x"00", x"00", x"00", x"00", x"00", x"00", x"00",  -- ' '
    x"00", x"00", x"33", x"7F", x"7F", x"6B", x"63", x"00",  -- m
    x"00", x"00", x"3E", x"03", x"1E", x"30", x"1F", x"00",  -- s
    x"30", x"18", x"0C", x"18", x"30", x"00", x"7E", x"00",  -- <=
    x"0C", x"18", x"30", x"18", x"0C", x"00", x"7E", x"00",  -- >=   
    x"00", x"30", x"49", x"06", x"30", x"49", x"06", x"00",  --  ~=
    x"00", x"00", x"00", x"00", x"00", x"00", x"00", x"00",  --' ' 10
    x"00", x"00", x"3F", x"00", x"00", x"3F", x"00", x"00",  -- =  11
    x"00", x"0C", x"0C", x"3F", x"0C", x"0C", x"00", x"00",  -- +  12
    x"00", x"00", x"00", x"3F", x"00", x"00", x"00", x"00",  -- -  13
    x"18", x"0C", x"06", x"03", x"06", x"0C", x"18", x"00",  -- <  14
    x"06", x"0C", x"18", x"30", x"18", x"0C", x"06", x"00",  -- >  15
    x"08", x"1C", x"36", x"63", x"41", x"00", x"00", x"00",  -- ^  16
    x"08", x"1C", x"36", x"63", x"41", x"00", x"00", x"00",  -- v  17
    x"18", x"0C", x"06", x"06", x"06", x"0C", x"18", x"00",  -- (  18
    x"06", x"0C", x"18", x"18", x"18", x"0C", x"06", x"00",  -- )  19
    x"00", x"0C", x"0C", x"00", x"00", x"0C", x"0C", x"00",  -- :  1A
    x"00", x"00", x"00", x"00", x"00", x"0C", x"0C", x"00",  -- .  1B
    x"00", x"00", x"00", x"00", x"00", x"0C", x"0C", x"06",  -- ,  1C
    x"1E", x"33", x"30", x"18", x"0C", x"00", x"0C", x"00",  -- ?  1D
    x"18", x"18", x"18", x"00", x"18", x"18", x"18", x"00",  -- |  1E
    x"36", x"36", x"7F", x"36", x"7F", x"36", x"36", x"00"); -- #  1F
  
  SIGNAL t_r,t_g,t_b : unsigned(7 DOWNTO 0);
  SIGNAL t_hs,t_vs,t_de,t_fl : std_logic;

  SUBTYPE uint20 IS natural RANGE 0 TO 65536*16-1;
  SIGNAL col : unsigned(7 DOWNTO 0);
  SIGNAL de,sense2,sense3 : std_logic;
  SIGNAL lowp : natural RANGE 0 TO 63;
  
  SIGNAL vsize,vsize5,hsize : natural RANGE 0 TO 4095;
  SIGNAL vcpt,hcpt,vcpt5,hcpt2 : natural RANGE 0 TO 4095;
  SIGNAL fcpt : natural RANGE 0 TO 31;
  SIGNAL vsize5_up : std_logic;
  
  CONSTANT COLS : natural :=20;
  SIGNAL txt,txt2 : unsigned(0 TO COLS*5-1);
  SIGNAL rect,alt,calc,calc2,pend,tog : std_logic :='0';

  SIGNAL synth : integer RANGE -65536*16384 TO 65536*16384-1;
  SIGNAL tick : std_logic :='0';
  SIGNAL timer,lat,lat2,lat3,lat4,tref,tdif : uint20;
  
  SIGNAL cpt_bcd ,bcd ,cpt_bcd2,bcd2 : unsigned(19 DOWNTO 0);
  SIGNAL cpt_bcd3,bcd3,cpt_bcd4,bcd4 : unsigned(19 DOWNTO 0);
  SIGNAL cre,cre2,cre3,cre4 : uint20;
  
  SIGNAL vligne,ligne : natural RANGE 0 TO 7;
  SIGNAL timeout : natural RANGE 0 TO 127;
  SIGNAL iter,iter2 : natural RANGE 0 TO 35;
  TYPE arr_uint20 IS ARRAY(natural RANGE <>) OF uint20;
  SIGNAL mem : arr_uint20(0 TO 31);
  SIGNAL rmem,mini,maxi : uint20;
  SIGNAL acc : natural RANGE 0 TO 65536*16384-1;
  TYPE enum_state IS (sIDLE,sACC,sACC2,sDIV);
  SIGNAL state : enum_state;
BEGIN
  
  -------------------------------------------------
  -- 1MHZ reference
  ClockGen:PROCESS (refclk)
  BEGIN
    IF rising_edge(refclk) THEN
      IF synth>0 THEN
        synth<=synth - 1000000;
        tick<='0';
      ELSE
        synth<=synth+ FREQ - 1000000;
        tick<='1';
      END IF;

      IF tick='1' THEN
        timer<=(timer+1) MOD (65536*16384);
      END IF;
    END IF;
  END PROCESS ClockGen;
  
  ----------------------------------------------------------
  Megamix:PROCESS(i_clk) IS
    VARIABLE txt_v  : unsigned(0 TO 20*5-1);
    VARIABLE char_v : unsigned(4 DOWNTO 0);
  BEGIN
    IF rising_edge(i_clk) THEN
      IF i_en='1' THEN
        ----------------------------------
        -- Propagate VGA signals. 2 cycles delay
        t_r<=i_r;
        t_g<=i_g;
        t_b<=i_b;
        t_hs<=i_hs;
        t_vs<=i_vs;
        t_fl<=i_fl;
        t_de<=i_de;
        
        o_r<=t_r;
        o_g<=t_g;
        o_b<=t_b;
        o_hs<=t_hs;
        o_vs<=t_vs;
        o_fl<=t_fl;
        o_de<=t_de;
        
        ----------------------------------
        -- Measure image size
        IF i_vs='1' AND t_vs='0' THEN
          vcpt<=0;
          vcpt5<=0;
          vligne<=0;
          vsize<=vcpt;
          vsize5_up<='0';
        ELSIF i_de='1' AND t_de='0' THEN
          vcpt<=vcpt+1;
          vcpt5<=vcpt5+1;
          IF vcpt*5-16 > vsize AND vsize5_up='0' THEN
            vsize5<=vcpt;
            vsize5_up<='1';
          END IF;
          IF vcpt5>=vsize5 THEN
            vcpt5<=0;
            vligne<=vligne+1;
          END IF;
        END IF; 
        
        IF i_hs='1' and t_hs='0' THEN
          hcpt<=0;
          IF vcpt=20 THEN
            hsize<=hcpt;
          END IF;
        ELSIF i_de='1' THEN
          hcpt<=hcpt+1;
        END IF;
        
        IF i_vs='1' AND t_vs='0' THEN
          fcpt<=fcpt+1;
          timeout<=timeout+1;
          IF fcpt>=3 THEN
            fcpt<=0;
            alt<=NOT alt;
            tog<='1';
          END IF;
        END IF;
        
        ----------------------------------
        IF sense='1' AND lowp<63 THEN
          lowp<=lowp+1;
        ELSIF sense='0' AND lowp>0 THEN
          lowp<=lowp-1;
        END IF;
        
        IF lowp=0 THEN
          sense2<='0';
        ELSIF lowp=63 THEN
          sense2<='1';
        END IF;
        
        ----------------------------------
        calc<='0';
        sense3<=sense2;
        IF sense2='1' AND sense3='0' AND pend='1' THEN
          timeout<=0;
          lat<=timer-tref;
          calc<='1';
          pend<='0';
        END IF;
        
        IF timeout>63 THEN
          timeout<=0;
          ligne<=ligne+1;
          IF ligne=4 THEN
            ligne<=0; 
          END IF;
        END IF;
        
        ----------------------------------
        IF vligne=ligne AND vcpt5<8 AND (hcpt<16 OR hcpt>=hsize-16 ) THEN
          rect<='1';
        ELSE
          rect<='0';
        END IF;
        
        IF vligne=ligne AND vcpt5=0 AND hcpt=0 AND i_de='1' THEN
          IF alt='1' and tog='1' THEN
            tref<=timer;
            pend<='1';
            tog<='0';
          END IF;
          led<=alt;
        END IF;
        
        ----------------------------------
        -- Pick characters
        IF (vcpt/8) MOD 2=0 THEN
          txt_v:=txt;
        ELSE
          txt_v:=txt2;
        END IF;
        
        IF hcpt<COLS * 8 AND vcpt<16 THEN
          char_v:=txt_v(((hcpt)/8)*5 TO ((hcpt)/8)*5+4);
        ELSE
          char_v:="10000"; -- " " : Blank character
        END IF;
        col<=CHARS(to_integer(char_v)*8+((vcpt-1) MOD 8));
        
        hcpt2<=hcpt;
        
        ----------------------------------
        -- Insert Overlay
        IF ena='1' THEN
          IF col((hcpt2) MOD 8)='1' THEN
            o_r<=rgb(23 DOWNTO 16);
            o_g<=rgb(15 DOWNTO  8);
            o_b<=rgb( 7 DOWNTO  0);
          END IF;
          IF rect='1' THEN
            o_r<=(OTHERS =>alt);
            o_g<=(OTHERS =>alt);
            o_b<=(OTHERS =>alt);
          END IF;
        END IF;
      ----------------------------------

      END IF;
    END IF;
  END PROCESS Megamix;

  ------------------------------------------------
  AVG:PROCESS(i_clk) IS
  BEGIN
    IF rising_edge(i_clk) THEN
      calc2<='0';
      rmem<=mem(iter2 MOD 16);
      
      CASE state IS
        WHEN sIDLE =>
          acc<=0;
          iter2<=0;
          mem(iter)<=lat;
          IF calc='1' THEN
            state<=sACC;
            iter<=iter+1;
            IF iter>=15 THEN
              iter<=0;
            END IF;
          END IF;
          mini<=65536*15;
          maxi<=0;
          
        WHEN sACC =>
          iter2<=iter2+1;
          state<=sACC2;
          acc<=0;
          
        WHEN sACC2 =>
          acc<=acc+rmem;
          IF rmem>maxi THEN maxi<=rmem; END IF;
          IF rmem<mini THEN mini<=rmem; END IF;
          iter2<=iter2+1;
          IF iter2=16 THEN
            state<=sDIV;
          END IF;
          
        WHEN sDIV =>
          lat2<=acc/16;
          lat3<=mini;
          lat4<=maxi;
          calc2<='1';
          state<=sIDLE;
          
      END CASE;
      
    END IF;
  END PROCESS;
  
  ------------------------------------------------
  Divi:PROCESS (i_clk) IS
    FUNCTION bcdinc(inc : std_logic;
                    a   : unsigned) return unsigned IS
      VARIABLE c : std_logic :='0';
      VARIABLE v : unsigned(19 DOWNTO 0) :=a;
    BEGIN
      IF inc='0' THEN c:='1'; END IF;
      FOR i IN 0 TO 4 LOOP
        IF i=2 AND inc='1' THEN c:='1'; END IF;
        IF c='1' THEN
          IF v(i*4+3 DOWNTO i*4)=x"A" THEN
            v(i*4+3 DOWNTO i*4):=x"1";
            c:='0';
          ELSIF v(i*4+3 DOWNTO i*4)=x"9" THEN
            v(i*4+3 DOWNTO i*4):=x"0";
            c:='1';
          ELSE
            v(i*4+3 DOWNTO i*4):=v(i*4+3 DOWNTO i*4)+1;
            c:='0';
          END IF;
        END IF;  
      END LOOP;
      RETURN v;
    END FUNCTION;
    
  BEGIN
    IF rising_edge(i_clk) THEN
      ----------------------------------
      IF calc='1' THEN
        cpt_bcd <=x"A0000";
        cre<=0;
      ELSIF cre+100<lat THEN
        cre<=cre+100;
        cpt_bcd<=bcdinc('1',cpt_bcd);
      ELSIF cre<lat THEN
        cre<=cre+1;
        cpt_bcd<=bcdinc('0',cpt_bcd);
      ELSE
        bcd<=cpt_bcd;
      END IF;

      ----------------------------------
      IF calc2='1' THEN
        cpt_bcd2 <=x"A0000";
        cre2<=0;
      ELSIF cre2+100<lat2 THEN
        cre2<=cre2+100;
        cpt_bcd2<=bcdinc('1',cpt_bcd2);
      ELSIF cre2<lat2 THEN
        cre2<=cre2+1;
        cpt_bcd2<=bcdinc('0',cpt_bcd2);
      ELSE
        bcd2<=cpt_bcd2;
      END IF;
      
      ----------------------------------
      IF calc2='1' THEN
        cpt_bcd3 <=x"A0000";
        cre3<=0;
      ELSIF cre3+100<lat3 THEN
        cre3<=cre3+100;
        cpt_bcd3<=bcdinc('1',cpt_bcd3);
      ELSIF cre3<lat3 THEN
        cre3<=cre3+1;
        cpt_bcd3<=bcdinc('0',cpt_bcd3);
      ELSE
        bcd3<=cpt_bcd3;
      END IF;
      
      ----------------------------------
      IF calc2='1' THEN
        cpt_bcd4 <=x"A0000";
        cre4<=0;
      ELSIF cre4+100<lat4 THEN
        cre4<=cre4+100;
        cpt_bcd4<=bcdinc('1',cpt_bcd4);
      ELSIF cre4<lat4 THEN
        cre4<=cre4+1;
        cpt_bcd4<=bcdinc('0',cpt_bcd4);
      ELSE
        bcd4<=cpt_bcd4;
      END IF;
    ----------------------------------
    END IF;
  END PROCESS Divi;
  
  txt<="10000" & "10000" & 
        '0' & bcd(19 DOWNTO 16) &
        '0' & bcd(15 DOWNTO 12) &
        "11011" &
        '0' & bcd(11 DOWNTO  8) &
        '0' & bcd(7 DOWNTO  4) &
        '0' & bcd(3 DOWNTO  0) &
        "01011" & "01100" & -- ms
        "10000" &
        "01111" &
        '0' & bcd2(19 DOWNTO 16) &
        '0' & bcd2(15 DOWNTO 12) &
        "11011" &
        '0' & bcd2(11 DOWNTO  8) &
        '0' & bcd2(7 DOWNTO  4) &
        '0' & bcd2(3 DOWNTO  0) &
        "01011" & "01100"; -- ms

  txt2<="10000" & "01110" & 
        '0' & bcd3(19 DOWNTO 16) &
        '0' & bcd3(15 DOWNTO 12) &
        "11011" &
        '0' & bcd3(11 DOWNTO  8) &
        '0' & bcd3(7 DOWNTO  4) &
        '0' & bcd3(3 DOWNTO  0) &
        "01011" & "01100" & -- ms
        "10000" &
        "01101" &
        '0' & bcd4(19 DOWNTO 16) &
        '0' & bcd4(15 DOWNTO 12) &
        "11011" &
        '0' & bcd4(11 DOWNTO  8) &
        '0' & bcd4(7 DOWNTO  4) &
        '0' & bcd4(3 DOWNTO  0) &
        "01011" & "01100"; -- ms
  
END ARCHITECTURE rtl;

