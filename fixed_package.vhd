LIBRARY IEEE;
USE IEEE.STD_logic_1164.ALL;
USE IEEE.STD_logic_arith.ALL;

PACKAGE FIXED_PACKAGE IS 
  CONSTANT MAX_IND : INTEGER:= +15;
  CONSTANT MIN_IND : INTEGER:= -15;
  SUBTYPE FIXED_RANGE IS  INTEGER RANGE MAX_IND  DOWNTO MIN_IND;
  SUBTYPE FIXED IS BIT_VECTOR;
 
--FUNÇÕES DE CONVERSÃO DE DADOS 
  FUNCTION TO_FIXED(ARG: INTEGER; MAX_RANGE:FIXED_RANGE:= MAX_IND; MIN_RANGE: FIXED_RANGE:= 0) RETURN FIXED;-- (MAX_RANGE DOWNTO MIN_RANGE);
  FUNCTION TO_FIXED(ARG: REAL; MAX_RANGE:FIXED_RANGE; MIN_RANGE: FIXED_RANGE) RETURN FIXED;-- (MAX_RANGE DOWNTO MIN_RANGE);
  FUNCTION TO_INTEGER(ARG: FIXED) RETURN INTEGER;
  FUNCTION TO_REAL(ARG: FIXED) RETURN REAL;
  
 --FUNÇÕES ARITMÉTICAS (SOMA)
  FUNCTION "+" (ARG_L, ARG_R: FIXED) RETURN FIXED;
  FUNCTION "+" (ARG_L: FIXED; ARG_R: INTEGER) RETURN FIXED;
  FUNCTION "+" (ARG_L: INTEGER; ARG_R: FIXED) RETURN FIXED;
  FUNCTION "+" (ARG_L: FIXED; ARG_R: REAL) RETURN FIXED;
  FUNCTION "+" (ARG_L: REAL; ARG_R: FIXED) RETURN FIXED;

 --FUNÇÕES ARITMÉTICAS (SUBTRAÇAO)
   FUNCTION "-" (ARG_L, ARG_R: FIXED) RETURN FIXED;
   FUNCTION "-" (ARG_L: FIXED; ARG_R: INTEGER) RETURN FIXED;
   FUNCTION "-" (ARG_L: INTEGER; ARG_R: FIXED) RETURN FIXED;
   FUNCTION "-" (ARG_L: FIXED; ARG_R: REAL) RETURN FIXED;
   FUNCTION "-" (ARG_L: REAL; ARG_R: FIXED) RETURN FIXED;
	
 --FUNÇÕES ARITMÉTICAS (MULTIPLICAÇÃO)
   FUNCTION "*"(ARG_L, ARG_R: FIXED) RETURN FIXED;
   FUNCTION "*"(ARG_L: FIXED; ARG_R: INTEGER) RETURN FIXED;
   FUNCTION "*"(ARG_L: INTEGER; ARG_R: FIXED) RETURN FIXED;
   FUNCTION "*"(ARG_L: FIXED; ARG_R: REAL) RETURN FIXED;
   FUNCTION "*"(ARG_L: REAL; ARG_R: FIXED) RETURN FIXED;
	  
END FIXED_PACKAGE;


PACKAGE BODY FIXED_PACKAGE IS  -- corpo do pacote

--CONVERSÃO DE DADO INTEGER PARA FIXED
    FUNCTION TO_FIXED(ARG: INTEGER; MAX_RANGE:FIXED_RANGE:= MAX_IND; MIN_RANGE: FIXED_RANGE:= 0) RETURN FIXED IS --(MAX_RANGE DOWNTO MIN_RANGE) 
	  VARIABLE ARG1: INTEGER;                             -- Argumento auxiliar que será dividido
	  VARIABLE R: INTEGER;                                -- Resto da divisão
	  VARIABLE DIV : INTEGER;                             -- Resultado da divisão
	  VARIABLE N_BIT: INTEGER;                            -- define a posição dos bits
	  VARIABLE F: FIXED(MAX_RANGE DOWNTO MIN_RANGE);      -- fixed auxiliar
	      

	  BEGIN
	   N_BIT := MIN_RANGE;                                -- a primeira posição é o mínimo (LSB)
	   ARG1 := ARG;
		
	   ABC:WHILE( DIV /= 1) OR (DIV /= 0 ) LOOP           -- divide até que o resultado da divisão seja 0 ou 1
		DIV :=ARG1/2;                                      -- divide por 2
		R := ARG REM 2;                                    -- guarda o resto da divisão
		ARG1 := DIV; 
		  IF R = 0 THEN  F(N_BIT) := '0';                  -- o resto da divisão é o valor do bit dessa posição             
		  ELSE           F(N_BIT) := '1';
		  END IF;
		N_BIT := N_BIT + 1;                                -- vai aumentando a posição
		END LOOP ABC;
		
		IF DIV = 0 THEN  F(N_BIT) := '0';                  -- o ultimo bit do numero é definido pelo resultado da ultima divisão
		ELSE             F(N_BIT) := '1';
		END IF;
		
		ASSERT N_BIT < MAX_RANGE;-- SE A CONVERSÃO PRECISAR DE MAIS CASAS QUE O DEFINIDO DA ERRO
		
		-- se a conversão dos bits tiver menos bits que o range definido 
           FOR i IN N_BIT+1 TO MAX_RANGE   LOOP  -- preenche com zero os bits restante 
	      F(i) := '0';
	   END LOOP;
         
         IF (ARG < 0) THEN                                  --  verifica se o argumento é negativo e faz o complemento de 2
	   F := F-1;
           F := NOT F;
	 END IF;
		  
	RETURN F;
	END FUNCTION;

--CONVERSÃO DE DADO REAL PARA FIXED	
	FUNCTION TO_FIXED(ARG: REAL; MAX_RANGE:FIXED_RANGE; MIN_RANGE: FIXED_RANGE) RETURN FIXED IS
	   VARIABLE R_I, R_F, DIV, INT_ARG, j : INTEGER;        -- variaveis intermediárias
	   VARIABLE F_ARG, F_ARG1, R_F_R, F_ARG2, INT_ARG_R : REAL;
	   VARIABLE F : FIXED(MAX_RANGE DOWNTO MIN_RANGE);      -- bit_vector do subtipo fixed onde será escrito o número real  
	
	   BEGIN
	      INT_ARG := INTEGER(ARG);                          -- transforma ARG no inteiro mais próximo
			INT_ARG_R := REAL(INT_ARG);                       -- transforma esse inteiro em real de novo para executar operaçoes
			IF (ARG >= INT_ARG_R) THEN                        -- separa a parte fracionária da inteira de ARG
	         F_ARG := ARG - INT_ARG_R;                      -- se ARG maior que o arredondamento basta subtrair o próprio arrendodamento e guardar a fração
			ELSE
			   F_ARG1 := INT_ARG_R - ARG;                     -- caso contrário (o arrendodamento foi pra cima), subtrai ARG do arredondamento
				F_ARG := 1.0 - F_ARG1;                         -- armazena a parte fracionária de ARG
				INT_ARG := INT_ARG - 1;                        -- como o arrendodamento foi pra cima, subtrai-se um 
			END IF;
	
	   IF (ARG < 0.0) THEN                                  -- atribui o sinal de ARG ao fixed
	      F(MAX_RANGE) := '1';
	   ELSE
	      F(MAX_RANGE) := '0';
	   END IF;
	
	   FOR i IN MIN_RANGE TO - 1 LOOP    		                 -- preenche a parte fracionária do fixed
		   F_ARG2 := F_ARG*2.0;
			R_F := INTEGER(F_ARG2);                           -- armazena a parte inteira da multiplicaçao por dois
		   R_F_R := REAL(R_F);
			F_ARG := F_ARG2 - R_F_R;                            -- separa a parte inteira da parte fracionaria
	      IF R_F = 0 THEN                                   -- preenche o bit correspondente com o valor em binário
		      F(i) := '0';
		   ELSE
		      F(i) := '1';
	      END IF;
	   END LOOP;
		
		j := 0;
	
	   WHILE( DIV /= 1) OR (DIV /= 0 ) LOOP                 -- preenche a parte inteira do fixed enquanto a parte inteira da divisão por dois não chegar a um ou zero
	      DIV := INT_ARG/2;                                 -- armazena a parte inteira da divisão por dois
	      R_I := INT_ARG REM 2;                             -- armazena o resto da divisao por dois
	      INT_ARG := DIV;                                   -- atualiza a parte inteira com o resultado da divisão
	      IF R_I = 0 THEN                                   -- preenche o bit correspondente com o valor em binário
		      F(j) := '0';                                   -- a posiçao da parte inteira se inicia depois do fim da parte fracionária
		   ELSE
		      F(j) := '1';
	      END IF;
		   j := j + 1;
	   END LOOP;
	   IF DIV = 0 THEN                                      -- preenche o último bit que representa a parte inteira de ARG
	      F(j) := '0';
      ELSE
	      F(j) := '1';
	   END IF;
	
	   FOR i IN j TO MAX_RANGE - 1 LOOP                     -- preenche com zero os bits restante que possam existir na posição parte inteira do fixed
	      F(i) := '0';
	   END LOOP;
	
	   RETURN F;                                           -- retorna fixed
	END FUNCTION;

--CONVERSÃO DE DADO FIXED PARA INTEGER
	FUNCTION TO_INTEGER(ARG: FIXED) RETURN INTEGER IS
		VARIABLE INT: INTEGER;										--valor retornado
	   VARIABLE MAX_RANGE : FIXED_RANGE:= ARG'LEFT;       -- armazena o limite esquerdo do fixed
	BEGIN		
		INT := 0;								
		
		IF ARG(MAX_RANGE) = '1' THEN                    	--se dado for negativo
			
			ARG := NOT ARG;											--complemento de dois
			ARG := ARG + 1;
			FOR I IN 0 TO ARG'HIGH - 1 LOOP						--avalia apenas a parte inteira do vetor fixed
				IF ARG(I) = '1' THEN INT := INT + 2**I;		--converte a parte inteira do vetor fixed para um valor inteiro
				END IF;
			END LOOP;
		   INT := INT*(-1);                         			-- salva inteiro negativo      
			
		ELSE																--se dado for positivo
		
			FOR I IN 0 TO ARG'HIGH - 1 LOOP						--avalia apenas a parte inteira do vetor fixed
				IF ARG(I) = '1' THEN INT := INT + 2**I;		--converte a parte inteira do vetor fixed para um valor inteiro
				END IF;
			END LOOP;
			
		END IF;

		RETURN INT;														--retorna inteiro
	END FUNCTION;

--CONVERSÃO DE DADO FIXED PARA REAL
   FUNCTION TO_REAL(ARG: FIXED) RETURN REAL IS
	   VARIABLE INT_M, F_N, R : REAL;
	   VARIABLE MIN_RANGE : FIXED_RANGE:= ARG'RIGHT;      -- armazena o limite direito do fixed
	   VARIABLE MAX_RANGE : FIXED_RANGE:= ARG'LEFT;       -- armazena o limite esquerdo do fixed
	
	   BEGIN
	      INT_M := 0;
		   F_N := 0;
		   FOR i IN MIN_RANGE TO - 1 LOOP                      -- varre-se a parte fracionaria do fixed
		      IF ARG(i) = '1' THEN             -- tranforma cada bit em seu valor na base decimal da parte fracionária
			      F_N := F_N + (-2)**i;
			   END IF;
   		END LOOP;
		
	   	FOR i IN 0 TO MAX_RANGE -1 LOOP                      -- varre-se a parte inteira do fixed
		      IF ARG(i) = '1' THEN       -- tranforma cada bit em seu valor na base decimal da parte inteira
			      INT_M := INT_M + 2**i;
			   END IF;
		   END LOOP;
		
		   IF ARG(MAX_RANGE) = '0' THEN                    -- a depender do sign bit
		      R := INT_M + F_N;                            -- simplesmente soma se for positivo
		   ELSE
		      R := (-1)*(INT_M + F_N);                     -- soma e atribui o menos se for negativo
		   END IF;

	   RETURN R;                                          -- retorna real
   END FUNCTION;
		--------------------------------------------------------------------------------------------------------------------------
		--			                                    FUNÇÕES ARITMÉTICAS 														  				   --
		--------------------------------------------------------------------------------------------------------------------------
		
  -----------------------------------------------   SOMA FIXED + FIXED   -------------------------------------------------------
	FUNCTION "+" (ARG_L, ARG_R: FIXED) RETURN FIXED IS
		
		CONSTANT MAX_SOMA : INTEGER;
		CONSTANT MIN_SOMA : INTEGER;
		VARIABLE SOMA : FIXED (MAX_SOMA DOWNTO MIN_SOMA);					--salva resultado da soma
		VARIABLE C : BIT;																--carry			
		
	   BEGIN

			IF ARG_L'HIGH > ARG_R'HIGH THEN										--procedimento para ARG_L > ARG_R
			
			--define tamanho do vetor SOMA
			MAX_SOMA := ARG_L'HIGH;
			MIN_SOMA := ARG_L'LOW;
			--alinha bit mais significativo
			
				IF ARG_R'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_L'HIGH DOWNTO ARG_R'HIGH+1 LOOP
						ARG_R(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_L'HIGH DOWNTO ARG_R'HIGH+1 LOOP				--se o dado for negativo, completa com um
						ARG_R(I) := '1';
					END LOOP;
				END IF;
				
			ELSIF ARG_R'HIGH > ARG_L'HIGH THEN									--procedimento para ARG_R > ARG_L
			
			--define tamanho do vetor SOMA
			MAX_SOMA := ARG_R'HIGH;
			MIN_SOMA := ARG_R'LOW;
			
			--alinha bit mais significativo
				IF ARG_L'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_R'HIGH DOWNTO ARG_L'HIGH+1 LOOP
						ARG_L(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_R'HIGH DOWNTO ARG_L'HIGH+1 LOOP				--se o dado for negativo, completa com um
						ARG_L(I) := '1';
					END LOOP;
				END IF;
			
			ELSE																			--procedimento para ARG_L = ARG_R
				
				--define tamanho do vetor SOMA
			MAX_SOMA := ARG_L'HIGH;												
			MIN_SOMA := ARG_L'LOW;
			
			END IF;
			
			C := '0';																	--inicia carry em zero
			FOR I IN ARG_L'LOW TO ARG_L'HIGH LOOP								--procedimento de soma
				SOMA(I)	:= ARG_L(I) XOR ARG_R(I) XOR C;
				C			:= ( ARG_L(I) AND ARG_R(I) ) OR ( ARG_L(I) AND C ) OR ( ARG_R(I) AND C );
			END LOOP;
	      
	   RETURN SOMA;                                          			-- retorna fixed
		
   END FUNCTION;
	
	--------------------------------------------   SOMA FIXED + INTEGER   --------------------------------------------------------- 
	FUNCTION "+" (ARG_L: FIXED; ARG_R: INTEGER) RETURN FIXED IS
		
		CONSTANT MAX_SOMA : INTEGER;
		CONSTANT MIN_SOMA : INTEGER;
		VARIABLE SOMA : FIXED (MAX_SOMA DOWNTO MIN_SOMA);					--salva resultado da soma
		VARIABLE C : BIT;																--carry			
		VARIABLE ARG_R_FIX : FIXED (MAX_IND DOWNTO 0);								
		
	   BEGIN
		
			--conversão de ARG_R para tipo fixed
			ARG_R_FIX := TO_FIXED (ARG_R, MAX_IND, 0);

			IF  ARG_R_FIX'HIGH > ARG_L'HIGH THEN								--procedimento para ARG_R_FIX > ARG_L

				--alinha bit mais significativo
				IF ARG_L'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_R_FIX'HIGH DOWNTO ARG_L'HIGH+1 LOOP
						ARG_L(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_R_FIX'HIGH DOWNTO ARG_L'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_L(I) := '1';
					END LOOP;
				END IF;
			
			END IF;																		--procedimento para ARG_L = ARG_R_FIX
				
			--define tamanho do vetor SOMA
			MAX_SOMA := MAX_IND;												
			MIN_SOMA := 0;
			
			C := '0';																	--inicia carry em zero
			FOR I IN 0 TO MAX_IND LOOP												--procedimento de soma
				SOMA(I)	:= ARG_L(I) XOR ARG_R_FIX(I)XOR C;
				C			:=( ARG_L(I) AND ARG_R_FIX(I) ) OR ( ARG_L(I) AND C ) OR ( ARG_R_FIX(I) AND C );
			END LOOP;
	      
	   RETURN SOMA;                                          			-- retorna fixed
		
   END FUNCTION;
	
	--------------------------------------------   SOMA INTEGER + FIXED   --------------------------------------------------------- 
	FUNCTION "+" (ARG_L: INTEGER; ARG_R: FIXED) RETURN FIXED IS
		
		CONSTANT MAX_SOMA : INTEGER;
		CONSTANT MIN_SOMA : INTEGER;
		VARIABLE SOMA : FIXED (MAX_SOMA DOWNTO MIN_SOMA);					--salva resultado da soma
		VARIABLE C : BIT;																--carry			
		VARIABLE ARG_L_FIX : FIXED (MAX_IND DOWNTO 0);								
		
	   BEGIN
		
			--conversão de ARG_R para tipo fixed
			ARG_L_FIX := TO_FIXED (ARG_L, MAX_IND, 0);

			IF ARG_L_FIX'HIGH > ARG_R'HIGH THEN									--procedimento para ARG_L_FIX > ARG_R
			
			--alinha bit mais significativo
				IF ARG_R'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_L_FIX'HIGH DOWNTO ARG_R'HIGH+1 LOOP
						ARG_R(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_L_FIX'HIGH DOWNTO ARG_R'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_R(I) := '1';
					END LOOP;
				END IF;
			
			END IF;																				--procedimento para ARG_L_FIX = ARG_R
				
			--define tamanho do vetor SOMA
			MAX_SOMA := MAX_IND;												
			MIN_SOMA := 0;
			
			C := '0';																	--inicia carry em zero
			FOR I IN 0 TO MAX_IND LOOP												--procedimento de soma
				SOMA(I)	:= ARG_L_FIX(I) XOR ARG_R (I) XOR C;	
				C			:= ( ARG_L_FIX(I) AND ARG_R(I) ) OR ( ARG_L_FIX(I) AND C ) OR ( ARG_R(I) AND C );
			END LOOP;
	      
	   RETURN SOMA;                                          			-- retorna fixed
		
   END FUNCTION;

	--------------------------------------------   SOMA FIXED + REAL   --------------------------------------------------------- 
	FUNCTION "+" (ARG_L: FIXED; ARG_R: REAL) RETURN FIXED IS
		
		CONSTANT MAX_SOMA : INTEGER;
		CONSTANT MIN_SOMA : INTEGER;
		VARIABLE SOMA : FIXED (MAX_SOMA DOWNTO MIN_SOMA);					--salva resultado da soma
		VARIABLE C : BIT;																--carry			
		VARIABLE ARG_R_FIX : FIXED (+7 DOWNTO -8);								
		
	   BEGIN
		
			--conversão de ARG_R para tipo fixed
			ARG_R_FIX := TO_FIXED (ARG_R, +7, -8);

			IF ARG_L'HIGH > ARG_R_FIX'HIGH THEN									--procedimento para ARG_L > ARG_R_FIX
			
			--define tamanho do vetor SOMA
			MAX_SOMA := ARG_L'HIGH;
			MIN_SOMA := ARG_L'LOW;
			--alinha bit mais significativo
			
				IF ARG_R_FIX'HIGH = '0' THEN										--se o dado for positivo, completa com zero
					FOR I IN ARG_L'HIGH DOWNTO ARG_R_FIX'HIGH+1 LOOP
						ARG_R_FIX(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_L'HIGH DOWNTO ARG_R_FIX'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_R_FIX(I) := '1';
					END LOOP;
				END IF;
			
			ELSIF ARG_R_FIX'HIGH > ARG_L'HIGH THEN								--procedimento para ARG_R_FIX > ARG_L
			
			--define tamanho do vetor SOMA
			MAX_SOMA := ARG_R_FIX'HIGH;
			MIN_SOMA := ARG_R_FIX'LOW;
			
			--alinha bit mais significativo
				IF ARG_L'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_R_FIX'HIGH DOWNTO ARG_L'HIGH+1 LOOP
						ARG_L(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_R_FIX'HIGH DOWNTO ARG_L'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_L(I) := '1';
					END LOOP;
				END IF;
			
			ELSE																			--procedimento para ARG_L = ARG_R_FIX
				
			--define tamanho do vetor SOMA
			MAX_SOMA := ARG_L'HIGH;												
			MIN_SOMA := ARG_L'LOW;
			
			END IF;
			
			C := '0';																	--inicia carry em zero
			FOR I IN ARG_L'LOW TO ARG_L'HIGH LOOP								--procedimento de soma
				SOMA(I)	:= ARG_L(I) XOR ARG_R_FIX(I)XOR C;
				C			:=( ARG_L(I) AND ARG_R_FIX(I) ) OR ( ARG_L(I) AND C ) OR ( ARG_R_FIX(I) AND C );
			END LOOP;
	      
	   RETURN SOMA;                                          			-- retorna fixed
		
   END FUNCTION;
	
	--------------------------------------------   SOMA REAL + FIXED   --------------------------------------------------------- 
	FUNCTION "+" (ARG_L: REAL; ARG_R: FIXED) RETURN FIXED IS
		
		CONSTANT MAX_SOMA : INTEGER;
		CONSTANT MIN_SOMA : INTEGER;
		VARIABLE SOMA : FIXED (MAX_SOMA DOWNTO MIN_SOMA);					--salva resultado da soma
		VARIABLE C : BIT;																--carry			
		VARIABLE ARG_L_FIX : FIXED (+7 DOWNTO -8);								
		
	   BEGIN
		
			--conversão de ARG_R para tipo fixed
			ARG_L_FIX := TO_FIXED (ARG_L, +7, -8);

			IF ARG_L_INT'HIGH > ARG_R'HIGH THEN									--procedimento para ARG_L_FIX > ARG_R
			
			--define tamanho do vetor SOMA
			MAX_SOMA := ARG_L_FIX'HIGH;
			MIN_SOMA := ARG_L_FIX'LOW;
			
			--alinha bit mais significativo
				IF ARG_R'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_L_FIX'HIGH DOWNTO ARG_R'HIGH+1 LOOP
						ARG_R(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_L_FIX'HIGH DOWNTO ARG_R'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_R(I) := '1';
					END LOOP;
				END IF;
			
			ELSIF ARG_R'HIGH > ARG_L_FIX'HIGH THEN								--procedimento para ARG_R > ARG_L_INT
			
			--define tamanho do vetor SOMA
			MAX_SOMA := ARG_R'HIGH;
			MIN_SOMA := ARG_R'LOW;
			
			--alinha bit mais significativo
				IF ARG_L_FIX'HIGH = '0' THEN										--se o dado for positivo, completa com zero
					FOR I IN ARG_R'HIGH DOWNTO ARG_L_FIX'HIGH+1 LOOP
						ARG_L_FIX(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_R'HIGH DOWNTO ARG_L_FIX'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_L_FIX(I) := '1';
					END LOOP;
				END IF;
			
			ELSE																			--procedimento para ARG_L_INT = ARG_R
				
			--define tamanho do vetor SOMA
			MAX_SOMA := ARG_R'HIGH;												
			MIN_SOMA := ARG_R'LOW;
			
			END IF;
			
			C := '0';																	--inicia carry em zero
			FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP								--procedimento de soma
				SOMA(I)	:= ARG_L_FIX(I) XOR ARG_R (I) XOR C;	
				C			:= ( ARG_L_FIX(I) AND ARG_R(I) ) OR ( ARG_L_FIX(I) AND C ) OR ( ARG_R(I) AND C );
			END LOOP;
	      
	   RETURN SOMA;                                          			-- retorna fixed
		
   END FUNCTION;
	
  -----------------------------------------------   SUBTRAÇÃO FIXED + FIXED   -------------------------------------------------------
	FUNCTION "-" (ARG_L, ARG_R: FIXED) RETURN FIXED IS
		
		CONSTANT MAX_SUB : INTEGER;
		CONSTANT MIN_SUB : INTEGER;
		VARIABLE SUB : FIXED (MAX_SUB DOWNTO MIN_SUB);			    		--salva resultado da subtração
		VARIABLE C, C1, M_UM : BIT;																--carry			
		
	   BEGIN

			IF ARG_L'HIGH > ARG_R'HIGH THEN										--procedimento para ARG_L > ARG_R
			
			--define tamanho do vetor SUB
			MAX_SUB := ARG_L'HIGH;
			MIN_SUB := ARG_L'LOW;
			--alinha bit mais significativo
			
				IF ARG_R'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_L'HIGH DOWNTO ARG_R'HIGH+1 LOOP
						ARG_R(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_L'HIGH DOWNTO ARG_R'HIGH+1 LOOP				--se o dado for negativo, completa com um
						ARG_R(I) := '1';
					END LOOP;
				END IF;
				
			ELSIF ARG_R'HIGH > ARG_L'HIGH THEN									--procedimento para ARG_R > ARG_L
			
			--define tamanho do vetor SUB
			MAX_SUB := ARG_R'HIGH;
			MIN_SUB := ARG_R'LOW;
			
			--alinha bit mais significativo
				IF ARG_L'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_R'HIGH DOWNTO ARG_L'HIGH+1 LOOP
						ARG_L(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_R'HIGH DOWNTO ARG_L'HIGH+1 LOOP				--se o dado for negativo, completa com um
						ARG_L(I) := '1';
					END LOOP;
				END IF;
			
			ELSE																			--procedimento para ARG_L = ARG_R
				
				--define tamanho do vetor SUB
			MAX_SUB := ARG_L'HIGH;												
			MIN_SUB := ARG_L'LOW;
			
			END IF;
			
			IF ARG_R'HIGH = '0' THEN                                     --se for positivo faz complemento de 2 direto
			   
				FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                     --complemento de 1
			      ARG_R(I):= NOT ARG_R(I);
			   END LOOP;
			
			   C1 := '1';                                                --inicia carry em um
			   FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                		--complemento de 2
				   ARG_R(I)	:= ARG_R(I) XOR C1;
				   C1			:= ARG_R(I) AND C1;
			   END LOOP;
				
			ELSE                                                         --se for negativo faz complemento de 2 inverso
			   
				M_UM := '1';                                              --define o "número" -1
				C1 := '0';                                                --inicia carry em zero
			   FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                		 --subtrai um
				   ARG_R(I)	:= ARG_R(I) XOR M_UM XOR  C1;
				   C1			:= ( ARG_R(I) AND M_UM ) XOR ( M_UM AND C1 ) XOR ( ARG_R(I) AND C1 ) ;
			   END LOOP;
				
				FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                     --complemento de 1 inverso
			      ARG_R(I):= NOT ARG_R(I);
			   END LOOP;

			END IF;
			
			C := '0';																	--inicia carry em zero
			FOR I IN ARG_L'LOW TO ARG_L'HIGH LOOP								--procedimento de soma
				SUB(I)	:= ARG_L(I) XOR ARG_R(I) XOR C;
				C			:= ( ARG_L(I) AND ARG_R(I) ) OR ( ARG_L(I) AND C ) OR ( ARG_R(I) AND C );
			END LOOP;
	      
	   RETURN SUB;                                           			-- retorna fixed
		
   END FUNCTION;
	
	--------------------------------------------   SUBTRAÇÃO FIXED + INTEGER   --------------------------------------------------------- 
	FUNCTION "-" (ARG_L: FIXED; ARG_R: INTEGER) RETURN FIXED IS
		
		CONSTANT MAX_SUB : INTEGER;
		CONSTANT MIN_SUB : INTEGER;
		VARIABLE SUB : FIXED (MAX_SUB DOWNTO MIN_SUB);				   	--salva resultado da subtração
		VARIABLE C, C1, M_UM : BIT;																--carry			
		VARIABLE ARG_R_FIX : FIXED (MAX_IND DOWNTO 0);								
		
	   BEGIN
		
			--conversão de ARG_R para tipo fixed
			ARG_R_FIX := TO_FIXED (ARG_R, MAX_IND, 0);

			IF  ARG_R_FIX'HIGH > ARG_L'HIGH THEN								--procedimento para ARG_R_FIX > ARG_L

				--alinha bit mais significativo
				IF ARG_L'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_R_FIX'HIGH DOWNTO ARG_L'HIGH+1 LOOP
						ARG_L(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_R_FIX'HIGH DOWNTO ARG_L'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_L(I) := '1';
					END LOOP;
				END IF;
			
			END IF;																		--procedimento para ARG_L = ARG_R_FIX
				
			IF ARG_R'HIGH = '0' THEN                                     --se for positivo faz complemento de 2 direto
			   
				FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                     --complemento de 1
			      ARG_R(I):= NOT ARG_R(I);
			   END LOOP;
			
			   C1 := '1';                                                --inicia carry em um
			   FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                		--complemento de 2
				   ARG_R(I)	:= ARG_R(I) XOR C1;
				   C1			:= ARG_R(I) AND C1;
			   END LOOP;
				
			ELSE                                                         --se for negativo faz complemento de 2 inverso
			   
				M_UM := '1';                                              --define o "número" -1
				C1 := '0';                                                --inicia carry em zero
			   FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                		 --subtrai um
				   ARG_R(I)	:= ARG_R(I) XOR M_UM XOR  C1;
				   C1			:= ( ARG_R(I) AND M_UM ) XOR ( M_UM AND C1 ) XOR ( ARG_R(I) AND C1 ) ;
			   END LOOP;
				
				FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                     --complemento de 1 inverso
			      ARG_R(I):= NOT ARG_R(I);
			   END LOOP;

			END IF;
			
			--define tamanho do vetor SOMA
			MAX_SUB := MAX_IND;												
			MIN_SUB := 0;
			
			C := '0';																	--inicia carry em zero
			FOR I IN 0 TO MAX_IND LOOP												--procedimento de soma
				SUB(I)	:= ARG_L(I) XOR ARG_R_FIX(I)XOR C;
				C			:=( ARG_L(I) AND ARG_R_FIX(I) ) OR ( ARG_L(I) AND C ) OR ( ARG_R_FIX(I) AND C );
			END LOOP;
	      
	   RETURN SUB;                                          			-- retorna fixed
		
   END FUNCTION;
	
	--------------------------------------------   SUBTRAÇÃO INTEGER + FIXED   --------------------------------------------------------- 
	FUNCTION "-" (ARG_L: INTEGER; ARG_R: FIXED) RETURN FIXED IS
		
		CONSTANT MAX_SUB : INTEGER;
		CONSTANT MIN_SUB : INTEGER;
		VARIABLE SUB : FIXED (MAX_SUB DOWNTO MIN_SUB);			   		--salva resultado da subtração
		VARIABLE C, C1, M_UM : BIT;																--carry			
		VARIABLE ARG_L_FIX : FIXED (MAX_IND DOWNTO 0);								
		
	   BEGIN
		
			--conversão de ARG_R para tipo fixed
			ARG_L_FIX := TO_FIXED (ARG_L, MAX_IND, 0);

			IF ARG_L_FIX'HIGH > ARG_R'HIGH THEN									--procedimento para ARG_L_FIX > ARG_R
			
			--alinha bit mais significativo
				IF ARG_R'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_L_FIX'HIGH DOWNTO ARG_R'HIGH+1 LOOP
						ARG_R(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_L_FIX'HIGH DOWNTO ARG_R'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_R(I) := '1';
					END LOOP;
				END IF;
			
			END IF;																				--procedimento para ARG_L_FIX = ARG_R
				
			IF ARG_R'HIGH = '0' THEN                                     --se for positivo faz complemento de 2 direto
			   
				FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                     --complemento de 1
			      ARG_R(I):= NOT ARG_R(I);
			   END LOOP;
			
			   C1 := '1';                                                --inicia carry em um
			   FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                		--complemento de 2
				   ARG_R(I)	:= ARG_R(I) XOR C1;
				   C1			:= ARG_R(I) AND C1;
			   END LOOP;
				
			ELSE                                                         --se for negativo faz complemento de 2 inverso
			   
				M_UM := '1';                                              --define o "número" -1
				C1 := '0';                                                --inicia carry em zero
			   FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                		 --subtrai um
				   ARG_R(I)	:= ARG_R(I) XOR M_UM XOR  C1;
				   C1			:= ( ARG_R(I) AND M_UM ) XOR ( M_UM AND C1 ) XOR ( ARG_R(I) AND C1 ) ;
			   END LOOP;
				
				FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                     --complemento de 1 inverso
			      ARG_R(I):= NOT ARG_R(I);
			   END LOOP;

			END IF;
			
			--define tamanho do vetor SUB
			MAX_SUB := MAX_IND;												
			MIN_SUB := 0;
			
			C := '0';																	--inicia carry em zero
			FOR I IN 0 TO MAX_IND LOOP												--procedimento de soma
				SUB(I)	:= ARG_L_FIX(I) XOR ARG_R (I) XOR C;	
				C			:= ( ARG_L_FIX(I) AND ARG_R(I) ) OR ( ARG_L_FIX(I) AND C ) OR ( ARG_R(I) AND C );
			END LOOP;
	      
	   RETURN SUB;                                             			-- retorna fixed
		
   END FUNCTION;

	--------------------------------------------   SUBTRAÇÃO FIXED + REAL   --------------------------------------------------------- 
	FUNCTION "-" (ARG_L: FIXED; ARG_R: REAL) RETURN FIXED IS
		
		CONSTANT MAX_SUB : INTEGER;
		CONSTANT MIN_SUB : INTEGER;
		VARIABLE SUB : FIXED (MAX_SUB DOWNTO MIN_SUB);			   		--salva resultado da subtração
		VARIABLE C, C1, M_UM : BIT;																--carry			
		VARIABLE ARG_R_FIX : FIXED (+7 DOWNTO -8);								
		
	   BEGIN
		
			--conversão de ARG_R para tipo fixed
			ARG_R_FIX := TO_FIXED (ARG_R, +7, -8);

			IF ARG_L'HIGH > ARG_R_FIX'HIGH THEN									--procedimento para ARG_L > ARG_R_FIX
			
			--define tamanho do vetor SOMA
			MAX_SUB := ARG_L'HIGH;
			MIN_SUB := ARG_L'LOW;
			--alinha bit mais significativo
			
				IF ARG_R_FIX'HIGH = '0' THEN										--se o dado for positivo, completa com zero
					FOR I IN ARG_L'HIGH DOWNTO ARG_R_FIX'HIGH+1 LOOP
						ARG_R_FIX(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_L'HIGH DOWNTO ARG_R_FIX'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_R_FIX(I) := '1';
					END LOOP;
				END IF;
			
			ELSIF ARG_R_FIX'HIGH > ARG_L'HIGH THEN								--procedimento para ARG_R_FIX > ARG_L
			
			--define tamanho do vetor SUB
			MAX_SUB := ARG_R_FIX'HIGH;
			MIN_SUB := ARG_R_FIX'LOW;
			
			--alinha bit mais significativo
				IF ARG_L'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_R_FIX'HIGH DOWNTO ARG_L'HIGH+1 LOOP
						ARG_L(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_R_FIX'HIGH DOWNTO ARG_L'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_L(I) := '1';
					END LOOP;
				END IF;
			
			ELSE																			--procedimento para ARG_L = ARG_R_FIX
				
			--define tamanho do vetor SOMA
			MAX_SUB := ARG_L'HIGH;												
			MIN_SUB := ARG_L'LOW;
			
			END IF;
			
			IF ARG_R'HIGH = '0' THEN                                     --se for positivo faz complemento de 2 direto
			   
				FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                     --complemento de 1
			      ARG_R(I):= NOT ARG_R(I);
			   END LOOP;
			
			   C1 := '1';                                                --inicia carry em um
			   FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                		--complemento de 2
				   ARG_R(I)	:= ARG_R(I) XOR C1;
				   C1			:= ARG_R(I) AND C1;
			   END LOOP;
				
			ELSE                                                         --se for negativo faz complemento de 2 inverso
			   
				M_UM := '1';                                              --define o "número" -1
				C1 := '0';                                                --inicia carry em zero
			   FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                		 --subtrai um
				   ARG_R(I)	:= ARG_R(I) XOR M_UM XOR  C1;
				   C1			:= ( ARG_R(I) AND M_UM ) XOR ( M_UM AND C1 ) XOR ( ARG_R(I) AND C1 ) ;
			   END LOOP;
				
				FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                     --complemento de 1 inverso
			      ARG_R(I):= NOT ARG_R(I);
			   END LOOP;

			END IF;
			
			C := '0';																	--inicia carry em zero
			FOR I IN ARG_L'LOW TO ARG_L'HIGH LOOP								--procedimento de soma
				SUB(I)	:= ARG_L(I) XOR ARG_R_FIX(I)XOR C;
				C			:=( ARG_L(I) AND ARG_R_FIX(I) ) OR ( ARG_L(I) AND C ) OR ( ARG_R_FIX(I) AND C );
			END LOOP;
	      
	   RETURN SUB;                                           			-- retorna fixed
		
   END FUNCTION;
	
	--------------------------------------------   SUBTRAÇÃO REAL + FIXED   --------------------------------------------------------- 
	FUNCTION "-" (ARG_L: REAL; ARG_R: FIXED) RETURN FIXED IS
		
		CONSTANT MAX_SUB : INTEGER;
		CONSTANT MIN_SUB : INTEGER;
		VARIABLE SUB : FIXED (MAX_SUB DOWNTO MIN_SUB);				   	--salva resultado da subtração
		VARIABLE C, C1, M_UM : BIT;																--carry			
		VARIABLE ARG_L_FIX : FIXED (+7 DOWNTO -8);								
		
	   BEGIN
		
			--conversão de ARG_R para tipo fixed
			ARG_L_FIX := TO_FIXED (ARG_L, +7, -8);

			IF ARG_L_INT'HIGH > ARG_R'HIGH THEN									--procedimento para ARG_L_FIX > ARG_R
			
			--define tamanho do vetor SOMA
			MAX_SUB := ARG_L_FIX'HIGH;
			MIN_SUB := ARG_L_FIX'LOW;
			
			--alinha bit mais significativo
				IF ARG_R'HIGH = '0' THEN											--se o dado for positivo, completa com zero
					FOR I IN ARG_L_FIX'HIGH DOWNTO ARG_R'HIGH+1 LOOP
						ARG_R(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_L_FIX'HIGH DOWNTO ARG_R'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_R(I) := '1';
					END LOOP;
				END IF;
			
			ELSIF ARG_R'HIGH > ARG_L_FIX'HIGH THEN								--procedimento para ARG_R > ARG_L_INT
			
			--define tamanho do vetor SUB
			MAX_SUB := ARG_R'HIGH;
			MIN_SUB := ARG_R'LOW;
			
			--alinha bit mais significativo
				IF ARG_L_FIX'HIGH = '0' THEN										--se o dado for positivo, completa com zero
					FOR I IN ARG_R'HIGH DOWNTO ARG_L_FIX'HIGH+1 LOOP
						ARG_L_FIX(I) := '0';
					END LOOP;
				ELSE
					FOR I IN ARG_R'HIGH DOWNTO ARG_L_FIX'HIGH+1 LOOP		--se o dado for negativo, completa com um
						ARG_L_FIX(I) := '1';
					END LOOP;
				END IF;
			
			ELSE																			--procedimento para ARG_L_INT = ARG_R
			
			--define tamanho do vetor SUB
			MAX_SUB := ARG_R'HIGH;												
			MIN_SUB := ARG_R'LOW;
			
			END IF;
			
			IF ARG_R'HIGH = '0' THEN                                     --se for positivo faz complemento de 2 direto
			   
				FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                     --complemento de 1
			      ARG_R(I):= NOT ARG_R(I);
			   END LOOP;
			
			   C1 := '1';                                                --inicia carry em um
			   FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                		--complemento de 2
				   ARG_R(I)	:= ARG_R(I) XOR C1;
				   C1			:= ARG_R(I) AND C1;
			   END LOOP;
				
			ELSE                                                         --se for negativo faz complemento de 2 inverso
			   
				M_UM := '1';                                              --define o "número" -1
				C1 := '0';                                                --inicia carry em zero
			   FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                		 --subtrai um
				   ARG_R(I)	:= ARG_R(I) XOR M_UM XOR  C1;
				   C1			:= ( ARG_R(I) AND M_UM ) XOR ( M_UM AND C1 ) XOR ( ARG_R(I) AND C1 ) ;
			   END LOOP;
				
				FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP                     --complemento de 1 inverso
			      ARG_R(I):= NOT ARG_R(I);
			   END LOOP;

			END IF;
			
			C := '0';																	--inicia carry em zero
			FOR I IN ARG_R'LOW TO ARG_R'HIGH LOOP								--procedimento de soma
				SUB(I)	:= ARG_L_FIX(I) XOR ARG_R (I) XOR C;	
				C			:= ( ARG_L_FIX(I) AND ARG_R(I) ) OR ( ARG_L_FIX(I) AND C ) OR ( ARG_R(I) AND C );
			END LOOP;
	      
	   RETURN SUB;                                           			-- retorna fixed
		
   END FUNCTION;

------------------------- MULTIPLICAÇÃO FIXED FIXED ---------------------

       FUNCTION "*"(ARG_L, ARG_R: FIXED) RETURN FIXED IS
	    VARIABLE S : FIXED (MAX DOWNTO MIN); -- define o tamanho do vetor de resposta
	    VARIABLE S1: FIXED (MAX DOWNTO MIN);
		 VARIABLE S2: FEXED (MAX DOWNTO MINS);
       VARIABLE N : INTEGER;
	    VARIABLE ARG_LC : FIXED ( ARG_L'RIGHT DOWNTO ARG_L'LEFT);  -- variavel auxiliar
		 
	 BEGIN
	 
	 ASSERT (ARG_L'RIGHT + ARG_R'LEFT +1) < 16;
	 
           -- limites do fixed		
	   MAX:= (ARG_L1'RIGHT + ARG_R'LEFT +1); 
	   MIN:= (ARG_R'LEFT + ARG_L'LEFT);
		MINS:= (16 -(ARG_L'RIGHT + ARG_R'LEFT +1)); -- para a saida de tamanho correto
	   S1:= 0;
		 -- faz a multiplicação dos fixed
		FOR I IN ARG_R'LEFT TO ARG_R'RIGHT LOOP -- o numero de argumentos de arg_r
		
		 IF ARG_R(I) = 0 THEN -- verifica os bits de arg_r
		  S:= 0; -- coloca zero no produto com esse bit
		 ELSE
		  N := 0;
		  IF i > ARG_R'LEFT THEN   -- coloca zero conforme a linha que ele está
		    FOR K IN (ARG_R'LEFT) TO i LOOP
		    S(S'LEFT+N) := '0';
	            N := N + 1;
		    END LOOP;
		  END IF;
		  
		  IF I = ARG_R'RIGHT THEN  -- verifica se está na ultima linha
		   IF ARG_R(ARG_R'RIGHT) = '1' THEN  -- se for negativo faz o complemento de 2
			 ARG_LC := ARG_L + 1;
			 FOR K IN ARG_L'LEFT TO ARG_L'RIGHT LOOP
		    S(S'LEFT+N) := NOT ARG_LC(K);
			 N := N+1;
		    END LOOP;
	     
		    FOR K IN N TO S'RIGHT LOOP
		    S(S'LEFT+K) := NOT ARG_LC(ARG_L'RIGHT);
		    END LOOP;
			
			ELSE -- se for positivo
			
			 FOR K IN ARG_L'LEFT TO ARG_L'RIGHT LOOP -- coloca o valor de arg_l
		    S(S'LEFT+N) := ARG_L(K);
			 N := N+1;
		    END LOOP;
			 
			 FOR K IN N TO S'RIGHT LOOP  -- coloca o valor do ultimo bit de arg_l nos bits restantes
		    S(S'LEFT+K) := ARG_L(ARG_L'RIGHT);
		    END LOOP;
			END IF;
			
		  ELSE -- se não estiver na ultima linha
			
		    FOR K IN ARG_L'LEFT TO ARG_L'RIGHT LOOP -- coloca o valor de arg_l
		    S(S'LEFT+N) := ARG_L(K);
			 N := N+1;
		    END LOOP;
			 
			 FOR K IN N TO S'RIGHT LOOP  -- coloca o valor do ultimo bit de arg_l nos bits restantes
		    S(S'LEFT+K) := ARG_L(ARG_L'RIGHT);
		    END LOOP;
			
			END IF;
		 
                 S1 := "+"(S, S1); -- SOMA DE DOIS FIXED
		  
		  END IF;
		 END LOOP;
		 
		 FOR I IN MAX DOWNTO MINS LOOP -- PASSA PARA UM VETOR DE 16 BITS
		  S2(I) := S1(I);
		 END LOOP;
		 
	 RETURN S2;
	 END FUNCTION;
	 
-------------------MULTIPLICAÇÃO FIXED INTEGER ------------------------	 
	 FUNCTION "*"(ARG_L: FIXED; ARG_R: INTEGER) RETURN FIXED IS
	  VARIABLE ARG_R1: FIEXED(ARG_L'RIGHT DOWNTO ARG_L'LEFT);
	  VARIABLE S : FIXED (MAX DOWNTO MIN); -- define o tamanho do vetor de resposta
	  VARIABLE S1: FIXED (MAX DOWNTO MIN);
	  VARIABLE S2: FIXED (MAX DOWNTO MINS);
	  VARIABLE N : INTEGER;
	  VARIABLE ARG_LC : FIXED ( ARG_L'RIGHT DOWNTO ARG_L'LEFT); -- arg_l auxiliar
		
	BEGIN
	  ARG_R1:= TO_FIXEDI(ARG_R,ARG_L'RIGHT,ARG_L'LEFT); -- FUNÇÃO DE CONVERSÃO FEITA ANTERIORMENTE
	  
	  ASSERT (ARG_L'RIGHT + ARG_R1'LEFT +1) < 16;
	  
	  -- LIMITES DOS FIXEDS DE RESULTADO DO PRODUTO
	   MAX:= (ARG_L'RIGHT + ARG_R1'LEFT +1);
		MIN:= (ARG_R1'LEFT + ARG_L'LEFT);
		MINS:= (16 -(ARG_L'RIGHT + ARG_R1'LEFT +1)); -- PARA SAIDA DE TAMANHO CORRETO
		S1:= 0;
		 -- faz a multiplicação dos fixed
		FOR I IN ARG_R1'LEFT TO ARG_R1'RIGHT LOOP
		
		 IF ARG_R1(I) = 0 THEN -- verifica se o bit é zero
		  S:= 0;
		 ELSE
		  N := 0;
		  IF i > ARG_R1'LEFT THEN -- coloca o tanto de zeros necessarios conforme a linha
		    FOR K IN (ARG_R1'LEFT) TO i LOOP
		    S(S'LEFT+N) := '0';
			 N := N + 1;
		    END LOOP;
		  END IF;
		  
		  IF I = ARG_R1'RIGHT THEN -- verifica se está na ultima linha
		   IF ARG_R(ARG_R1'RIGHT) = '1' THEN -- se for negativo faz o complemento de 2
			 ARG_LC := ARG_L + 1;
			 FOR K IN ARG_L'LEFT TO ARG_L'RIGHT LOOP -- coloca ele na posição de s correspondente
		         S(S'LEFT+N) := NOT ARG_LC(K);
			 N := N+1;
		    END LOOP;
	     
		    FOR K IN N TO S'RIGHT LOOP
		    S(S'LEFT+K) := NOT ARG_LC(ARG_L'RIGHT);
		    END LOOP;
			
		    ELSE -- não é negativo
			
			 FOR K IN ARG_L'LEFT TO ARG_L'RIGHT LOOP -- coloca o arg_l  em s na posição correspondente
		         S(S'LEFT+N) := ARG_L(K);
			 N := N+1;
		         END LOOP;
			 
			 FOR K IN N TO S'RIGHT LOOP
		         S(S'LEFT+K) := ARG_L(ARG_L'RIGHT);
		         END LOOP;
		    END IF;
			
		  ELSE -- não está na ultima linha
			
		    FOR K IN ARG_L'LEFT TO ARG_L'RIGHT LOOP -- coloca o arg_l  em s na posição correspondente
		    S(S'LEFT+N) := ARG_L(K);
			 N := N+1;
		    END LOOP;
			 
			 FOR K IN N TO S'RIGHT LOOP -- coloca o valor de positivo ou negativo nas proximas posições
		    S(S'LEFT+K) := ARG_L(ARG_L'RIGHT);
		    END LOOP;
			
			END IF;
		 
                 S1 := "+"(S, S1); -- SOMA DE DOIS FIXED
		  
		  END IF;
		 END LOOP;
		 
		 FOR I IN MAX DOWNTO MINS LOOP -- PASSA PARA UM VETOR DE 16 BITS
		  S2(I) := S1(I);
		 END LOOP;
		 
	 RETURN S2;
	 END FUNCTION;
	 
	 -----------------------MULTIPLICAÇÃO INTEGER FIXED -------------------------------
	 FUNCTION "*"(ARG_L: INTEGER; ARG_R: FIXED) RETURN FIXED IS
	  VARIABLE ARG_L1: FIEXED(ARG_R'RIGHT DOWNTO ARG_R'LEFT);
	  VARIABLE S : FIXED (MAX DOWNTO MIN); -- define o tamanho do vetor de resposta
	  VARIABLE S1: FIXED (MAX DOWNTO MIN);
	  VARIABLE S2: FIXED (MAX DOWNTO MINS);
	  VARIABLE N : INTEGER;
	  
	 BEGIN
	   ARG_L1:= TO_FIXEDI(ARG_L, ARG_R'RIGHT, ARG_R'LEFT); -- FUNÇÃO DE CONVERSÃO FEITA ANTERIORMENTE
		
		ASSERT (ARG_L1'RIGHT + ARG_R'LEFT +1) < 16;
	  
	  -- limite para os fixed
	   MAX:= (ARG_L1'RIGHT + ARG_R'LEFT +1);
		MIN:= (ARG_R'LEFT + ARG_L1'LEFT);
		MINS:= (16 -(ARG_L1'RIGHT + ARG_R'LEFT +1));-- limite para a saida com tamanho correto
		S1:= 0;
		 -- faz a multiplicação dos fixed
		FOR I IN ARG_R'LEFT TO ARG_R'RIGHT LOOP
		
		 IF ARG_R(I) = 0 THEN -- caso o bit correspondente seja 0
		  S:= 0;
		 ELSE
		  N := 0;
		  IF i > ARG_R'LEFT THEN  -- coloca zeros de acordo com a linha correspondente
		    FOR K IN (ARG_R'LEFT) TO i LOOP
		    S(S'LEFT+N) := '0';
			 N := N + 1;
		    END LOOP;
		  END IF;
		  
		  IF I = ARG_R'RIGHT THEN -- vefifica se esta na ultima linha
		   IF ARG_R(ARG_R'RIGHT) = '1' THEN      -- verifica se é negativo
			 ARG_L1 := ARG_L1 + 1;                 -- faz o compplemento de 2 
			 FOR K IN ARG_L1'LEFT TO ARG_L1'RIGHT LOOP
		    S(S'LEFT+N) := NOT ARG_L1(K);
			 N := N+1;
		    END LOOP;
	     
		    FOR K IN N TO S'RIGHT LOOP -- coloca os zeros ou uns conforme o umtilo bit de arg_l 
		    S(S'LEFT+K) := NOT ARG_L1(ARG_L1'RIGHT);
		    END LOOP;
			
			ELSE     -- é positivo
			
			 FOR K IN ARG_L1'LEFT TO ARG_L1'RIGHT LOOP -- coloca arg_l na posição correspondente
		    S(S'LEFT+N) := ARG_L1(K);
			 N := N+1;
		    END LOOP;
			 
			 FOR K IN N TO S'RIGHT LOOP  -- preenche conforme o sinal de arg_l
		    S(S'LEFT+K) := ARG_L1(ARG_L1'RIGHT);
		    END LOOP;
			END IF;
			
		  ELSE      -- não é a ultima linha
			
		    FOR K IN ARG_L1'LEFT TO ARG_L1'RIGHT LOOP -- coloca arg_l na posição correspondente
		    S(S'LEFT+N) := ARG_L1(K);
			 N := N+1;
		    END LOOP;
			 
			 FOR K IN N TO S'RIGHT LOOP              -- preenche conforme o sinal de arg_l
		    S(S'LEFT+K) := ARG_L1(ARG_L1'RIGHT);
		    END LOOP;
			
			END IF;
		 
               S1 := "+"(S, S1); -- SOMA DE DOIS FIXED
		  
		  END IF;
		 END LOOP;
		 
		 FOR I IN MAX DOWNTO MINS LOOP -- PASSA PARA UM VETOR DE 16 BITS
		  S2(I) := S1(I);
		 END LOOP;
		 
	 RETURN S2;
	 END FUNCTION;
--------------------------MULTIPLICAÇÃO FIXED REAL ------------------------------------	 
	 FUNCTION "*"(ARG_L: FIXED; ARG_R: REAL) RETURN FIXED IS
	  VARIABLE ARG_R1: FIEXED(ARG_L'RIGHT DOWNTO ARG_L'LEFT);
	  VARIABLE S : FIXED (MAX DOWNTO MIN); -- define o tamanho do vetor de resposta
	  VARIABLE S1: FIXED (MAX DOWNTO MIN);
	  VARIABLE S2: FIXED (MAX DOWNTO MINS);
	  VARIABLE N : INTEGER;
	  VARIABLE ARG_LC : FIXED ( ARG_L'RIGHT DOWNTO ARG_L'LEFT);
	  
	 BEGIN
	  ARG_R1:= TO_FIXED(ARG_R,ARG_L'RIGHT,ARG_L'LEFT); -- FUNÇÃO DE CONVERSÃO FEITA ANTERIORMENTE
	  
	  ASSERT (ARG_L'RIGHT + ARG_R1'LEFT +1) < 16; -- da erro se a parte inteira é maior que 16
	  
	  -- limites do fixed
	   MAX:= (ARG_L'RIGHT + ARG_R1'LEFT +1);
		MIN:= (ARG_R1'LEFT + ARG_L'LEFT);
		MINS:= (16 -(ARG_L'RIGHT + ARG_R1'LEFT +1));-- saida do tamanho correto
		S1:= 0;
		 -- faz a multiplicação dos fixed
		FOR I IN ARG_R1'LEFT TO ARG_R1'RIGHT LOOP
		
		 IF ARG_R1(I) = 0 THEN  -- verifica se o bit correspondete é zero
		  S:= 0;
		  
		 ELSE
		  N := 0;
		  IF i > ARG_R1'LEFT THEN        -- preenche com zeros conforme a linha
		    FOR K IN (ARG_R1'LEFT) TO i LOOP
		    S(S'LEFT+N) := '0';
			 N := N + 1;
		    END LOOP;
		  END IF;
		  
		  IF I = ARG_R1'RIGHT THEN  -- verifica se está na ultima linha
		   IF ARG_R(ARG_R1'RIGHT) = '1' THEN -- verifica se é negativo
			 ARG_LC := ARG_L + 1;             -- faz o complemento de 2
			 FOR K IN ARG_L'LEFT TO ARG_L'RIGHT LOOP
		    S(S'LEFT+N) := NOT ARG_LC(K);      -- coloca o complemento na posição corespondente
			 N := N+1;
		    END LOOP;
	     
		    FOR K IN N TO S'RIGHT LOOP            -- preenche conforme o sinal do complemento
		    S(S'LEFT+K) := NOT ARG_LC(ARG_L'RIGHT);
		    END LOOP;
			
			ELSE -- é positivo
			
			 FOR K IN ARG_L'LEFT TO ARG_L'RIGHT LOOP -- coloca arg_l na posição correspondente
		    S(S'LEFT+N) := ARG_L(K);
			 N := N+1;
		    END LOOP;
			 
			 FOR K IN N TO S'RIGHT LOOP
		    S(S'LEFT+K) := ARG_L(ARG_L'RIGHT); -- preenche conforme o sinal de arg_l
		    END LOOP;
			END IF;
			
		  ELSE
			
		    FOR K IN ARG_L'LEFT TO ARG_L'RIGHT LOOP -- coloca arg_l na posição correspondente
		    S(S'LEFT+N) := ARG_L(K);
			 N := N+1;
		    END LOOP;
			 
			 FOR K IN N TO S'RIGHT LOOP       -- preenche conforme o sinal de arg_l
		    S(S'LEFT+K) := ARG_L(ARG_L'RIGHT);
		    END LOOP;
			
			END IF;
		 
        S1 := "+"(S, S1); -- SOMA DE DOIS FIXED
		  
		  END IF;
		 END LOOP;
		 
		 FOR I IN MAX DOWNTO MINS LOOP -- PASSA PARA UM VETOR DE 16 BITS
		  S2(I) := S1(I);
		 END LOOP;
		 
	 RETURN S2;
	 END FUNCTION;
	 
----------------------- MULTIPLICAÇÃO REAL FIXED --------------------------------------	 
	 FUNCTION "*"(ARG_L: REAL; ARG_R: FIXED) RETURN FIXED IS
	  VARIABLE ARG_L1: FIEXED(ARG_R'RIGHT DOWNTO ARG_R'LEFT);
	  VARIABLE S : FIXED (MAX DOWNTO MIN); -- define o tamanho do vetor de resposta
	  VARIABLE S1: FIXED (MAX DOWNTO MIN);
	  VARIABLE S2: FIXED (MAX DOWNTO MINS);
	  VARIABLE N : INTEGER;
	  
	 BEGIN
	  ARG_L1:= TO_FIXED(ARG_L,ARG_R'RIGHT,ARG_R'LEFT); -- FUNÇÃO DE CONVERSÃO FEITA ANTERIORMENTE
	  
	  ASSERT (ARG_L1'RIGHT + ARG_R'LEFT +1) < 16; -- da erro se a parte inteira é maior que 16
	  
	  -- limites do fixed
	   MAX:= (ARG_L1'RIGHT + ARG_R'LEFT +1);
		MIN:= (ARG_R'LEFT + ARG_L1'LEFT);
		MINS:= (16 -(ARG_L1'RIGHT + ARG_R'LEFT +1));-- saida do tamanho correto
		S1:= 0;
		 -- faz a multiplicação dos fixed
		FOR I IN ARG_R'LEFT TO ARG_R'RIGHT LOOP
		
		 IF ARG_R(I) = 0 THEN  -- verifica se o bit correspondete é zero
		  S:= 0;
		  
		 ELSE
		  N := 0;
		  IF i > ARG_R'LEFT THEN        -- preenche com zeros conforme a linha
		    FOR K IN (ARG_R'LEFT) TO i LOOP
		    S(S'LEFT+N) := '0';
			 N := N + 1;
		    END LOOP;
		  END IF;
		  
		  IF I = ARG_R'RIGHT THEN  -- verifica se está na ultima linha
		   IF ARG_R(ARG_R'RIGHT) = '1' THEN -- verifica se é negativo
			 ARG_L1 := ARG_L1 + 1;             -- faz o complemento de 2
			 FOR K IN ARG_L1'LEFT TO ARG_L1'RIGHT LOOP
		    S(S'LEFT+N) := NOT ARG_L1(K);      -- coloca o complemento na posição corespondente
			 N := N+1;
		    END LOOP;
	     
		    FOR K IN N TO S'RIGHT LOOP            -- preenche conforme o sinal do complemento
		    S(S'LEFT+K) := NOT ARG_L1(ARG_L1'RIGHT);
		    END LOOP;
			
			ELSE -- é positivo
			
			 FOR K IN ARG_L1'LEFT TO ARG_L1'RIGHT LOOP -- coloca arg_l na posição correspondente
		    S(S'LEFT+N) := ARG_L1(K);
			 N := N+1;
		    END LOOP;
			 
			 FOR K IN N TO S'RIGHT LOOP
		    S(S'LEFT+K) := ARG_L1(ARG_L1'RIGHT); -- preenche conforme o sinal de arg_l
		    END LOOP;
			END IF;
			
		  ELSE
			
		    FOR K IN ARG_L1'LEFT TO ARG_L1'RIGHT LOOP -- coloca arg_l na posição correspondente
		    S(S'LEFT+N) := ARG_L1(K);
			 N := N+1;
		    END LOOP;
			 
			 FOR K IN N TO S'RIGHT LOOP       -- preenche conforme o sinal de arg_l
		    S(S'LEFT+K) := ARG_L1(ARG_L1'RIGHT);
		    END LOOP;
			
			END IF;
		 
        S1 := "+"(S, S1); -- SOMA DE DOIS FIXED
		  
		  END IF;
		 END LOOP;
		 
		 FOR I IN MAX DOWNTO MINS LOOP -- PASSA PARA UM VETOR DE 16 BITS
		  S2(I) := S1(I);
		 END LOOP;
		 
	 RETURN S2;
	 END FUNCTION;
	
END FIXED_PACKAGE;
