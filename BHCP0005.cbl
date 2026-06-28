******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP00005
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 27/06/2026
      * OBJETIVO..: Cálculo de movimentação bancária
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
 
       PROGRAM-ID. BHCP00005.
 
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
       01 MOVIMENTACAO.
          03 GDA-NR-SALDO-INICIAL   PIC 9(4) VALUE 1000.
          03 GDA-NR-VALOR-DEPOSITO  PIC 9(3) VALUE 500.
          03 GDA-NR-VALOR-SAQUE     PIC 9(3) VALUE 200.
          03 GDA-NR-SALDO-FINAL     PIC 9(4) VALUE 0.
 
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
           COMPUTE GDA-NR-SALDO-FINAL = GDA-NR-SALDO-INICIAL
              + GDA-NR-VALOR-DEPOSITO
              - GDA-NR-VALOR-SAQUE.                
                                      
           DISPLAY "SALDO INICIAL: " GDA-NR-SALDO-INICIAL.
           DISPLAY "DEPÓSITO: " GDA-NR-VALOR-DEPOSITO.
           DISPLAY "SAQUE: " GDA-NR-VALOR-SAQUE.
           DISPLAY "SALDO FINAL: " GDA-NR-SALDO-FINAL.

           GOBACK.
      ******************************************************************