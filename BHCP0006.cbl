******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP00006
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 27/06/2026
      * OBJETIVO..: Classificar saldo do cliente (if, else)
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
 
       PROGRAM-ID. BHCP00006.
 
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
       01 CLASSIFICA-SALDO.
          03 GDA-TX-NOME      PIC X(20) VALUE "ANA PAULA".
          03 GDA-NR-SALDO     PIC S9(4) VALUE 500.
          03 GDA-TX-SITUACAO  PIC X(10) VALUE SPACES.
 
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
      * Optei por usar operadores aritméticos (>, =, <), mas sei que 
      * poderia ter escrito por extenso.
           IF GDA-NR-SALDO > 0 THEN 
              MOVE "POSITIVO" TO GDA-TX-SITUACAO
           ELSE
              IF GDA-NR-SALDO = 0 THEN
                 MOVE "ZERADO" TO GDA-TX-SITUACAO
              ELSE
                 MOVE "NEGATIVO" TO GDA-TX-SITUACAO
              END-IF
           END-IF.              
                                      
           DISPLAY "NOME: " GDA-TX-NOME.
           DISPLAY "SALDO: " GDA-NR-SALDO.
           DISPLAY "SITUACAO: " GDA-TX-SITUACAO.

           GOBACK.
      ******************************************************************