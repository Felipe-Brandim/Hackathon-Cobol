******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP00007
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 27/06/2026
      * OBJETIVO..: IDENTIFICAR TIPO DE CONTA (EVALUATE)
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 27.06.26 Felipe Brandim  IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************
 
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
 
       PROGRAM-ID. BHCP00007.
 
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
 
      *----------------------------------------
       CONFIGURATION                   SECTION.
      *----------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
 
      ******************************************************************
       DATA DIVISION. 
      ******************************************************************
 
      *----------------------------------------
       WORKING-STORAGE                 SECTION.
      *----------------------------------------
      
      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01 IDENTIFICA-CONTA.
          03 GDA-TX-DESCRICAO  PIC X(20) VALUE SPACES.
          03 GDA-NR-TIPO       PIC 9(1)  VALUE 2.
          
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
      * Seleção Múltipla equivalente ao switch/case do C
      * Vantagem interessante: O Cobol não precisa de break
      * pois o EVALUATE encerra a execução do bloco.
           EVALUATE GDA-NR-TIPO
           WHEN 1
                MOVE "CONTA CORRENTE" TO GDA-TX-DESCRICAO
           WHEN 2
                MOVE "CONTA POUPANCA" TO GDA-TX-DESCRICAO
           WHEN 3
                MOVE "CONTA SALARIO" TO GDA-TX-DESCRICAO
           WHEN 4
                MOVE "CONTA EMPRESARIAL" TO GDA-TX-DESCRICAO
           WHEN OTHER
                MOVE "TIPO INVALIDO" TO GDA-TX-DESCRICAO
           END-EVALUATE.

           DISPLAY "CODIGO DO TIPO: " GDA-NR-TIPO.
           DISPLAY "DESCRICAO: " FUNCTION TRIM(GDA-TX-DESCRICAO).

           GOBACK.
      ******************************************************************