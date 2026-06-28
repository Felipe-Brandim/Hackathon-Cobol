******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP00002
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 25/06/2026
      * OBJETIVO..: Realizar soma simples e imprimir o resultado
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 25.06.26 Felipe Brandim  IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************
 
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
 
       PROGRAM-ID. BHCP00002.
 
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
       77 GDA-NR-1          PIC 9(02).
       77 GDA-NR-2          PIC 9(02).
       77 GDA-TX-RESULTADO  PIC 9(02). 
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
           MOVE 10 TO GDA-NR-1.
           MOVE 25 TO GDA-NR-2.

           ADD GDA-NR-1 TO GDA-NR-2 GIVING GDA-TX-RESULTADO.

           DISPLAY "Primeiro numero: " GDA-NR-1.
           DISPLAY "Segundo numero : " GDA-NR-2.
           DISPLAY "Resultado      : " GDA-TX-RESULTADO.

           
           GOBACK.
      ******************************************************************