******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP00004
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 26/06/2026
      * OBJETIVO..: Cadastro simples de cliente
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 26.06.26 Felipe Brandim  IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************
 
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
 
       PROGRAM-ID. BHCP00004.
 
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
       01 CAD-CLIENTE.
          03 GDA-NR-CODIGO   PIC 9(4).
          03 GDA-TX-NOME     PIC X(20).
          03 GDA-NR-AGENCIA  PIC 9(4).
          03 GDA-NR-CONTA    PIC 9(6).
          03 GDA-NR-SALDO    PIC 9(4).
  
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
           MOVE "1001" TO GDA-NR-CODIGO.
           MOVE "JOÃO SILVA" TO GDA-TX-NOME.
           MOVE "1234" TO GDA-NR-AGENCIA.
           MOVE "987654" TO GDA-NR-CONTA.
           MOVE "1500" TO GDA-NR-SALDO.

           DISPLAY "CADASTRO DE CLIENTE".
           DISPLAY "CODIGO: " GDA-NR-CODIGO.
           DISPLAY "NOME: " GDA-TX-NOME.
           DISPLAY "AGENCIA: " GDA-NR-AGENCIA.
           DISPLAY "CONTA: " GDA-NR-CONTA.
           DISPLAY "SALDO INICIAL: " GDA-NR-SALDO.

           GOBACK.
      ******************************************************************