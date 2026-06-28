******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP00003
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 26/06/2026
      * OBJETIVO..: Movimentação simples
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
 
       PROGRAM-ID. BHCP00003.
 
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
       77 GDA-TX-ALUNO     PIC X(30).
       77 GDA-TX-CURSO     PIC X(30).
       77 GDA-TX-MENSAGEM  PIC X(30). 
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
           MOVE "MARIA" TO GDA-TX-ALUNO.
           MOVE "BOOTCAMP COBOL" TO GDA-TX-CURSO.
           MOVE "MEU PROGRAMA COBOL" TO GDA-TX-MENSAGEM.  

           DISPLAY "ALUNO: " GDA-TX-ALUNO.
           DISPLAY "CURSO: " GDA-TX-CURSO.
           DISPLAY "MENSAGEM: " GDA-TX-MENSAGEM.

           GOBACK.
      ******************************************************************