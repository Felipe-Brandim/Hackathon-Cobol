******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCS0001
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 03/07/2026
      * OBJETIVO..: Subprograma de Emissao de Relatorio Bancario
      * EXECUCAO..: CHAMADO POR BHCP0011
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 002 03.07.26 Felipe Brandim  CORRECAO DE UNREACHABLE CODE
      * ----------------------------------------------------------------
******************************************************************
 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BHCS0001.
 
       ENVIRONMENT DIVISION.
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
 
       DATA DIVISION.
      *----------------------------------------
       WORKING-STORAGE                 SECTION.
      *----------------------------------------
      * Subprogramas nao guardam a estrutura original aqui.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
      * Atributos locais de montagem e loop do relatorio
       01 LK-CONTROLE.
          03 LK-IDX              PIC 9(02) VALUE 1.
          03 LK-TX-LINHA         PIC X(40) VALUE SPACES.

      *----------------------------------------
       LINKAGE SECTION.
      *----------------------------------------
      * Mapeamento exato da estrutura recebida do programa chamador
       01 LK-SISTEMA-BANCARIO.
          03 LK-QTDE-CONTAS      PIC 9(02).
          03 LK-TAB-CONTAS OCCURS 1 TO 50 TIMES
                DEPENDING ON LK-QTDE-CONTAS.
             05 LK-CTA-NUMERO    PIC 9(05).
             05 LK-CTA-CLIENTE   PIC X(10).
             05 LK-CTA-TIPO      PIC X(01).
             05 LK-CTA-TIPO-TXT  PIC X(08).
             05 LK-CTA-SALDO     PIC 9(05).
             05 LK-CTA-STATUS    PIC X(08).
  
******************************************************************
      * Vinculando o recebimento da estrutura na Procedure Division
       PROCEDURE DIVISION USING LK-SISTEMA-BANCARIO.
******************************************************************
 
       0000-SUBPRINCIPAL SECTION.
       0000-INICIO.
           DISPLAY "************************************".
           DISPLAY "RELATORIO DE CLIENTE".

      * FIX: Chamando a SECTION para englobar o paragrafo FIM[cite: 3]
           PERFORM 1000-RELATORIO
              VARYING LK-IDX FROM 1 BY 1 UNTIL LK-IDX > LK-QTDE-CONTAS.

           DISPLAY "****************".
           
      * Retorna o controle para o programa principal de forma segura
           GOBACK.

       1000-RELATORIO SECTION.
       1000-IMPRESSAO.
      * Linha CLIENTE
           MOVE SPACES TO LK-TX-LINHA.
           STRING "CLIENTE: " DELIMITED BY SIZE
                  FUNCTION TRIM(LK-CTA-CLIENTE(LK-IDX))
              DELIMITED BY SIZE
              INTO LK-TX-LINHA.
           DISPLAY LK-TX-LINHA.

      * Linha TIPO (Mantendo o alinhamento de 8 posicoes do label)
           MOVE SPACES TO LK-TX-LINHA.
           STRING "TIPO    : " DELIMITED BY SIZE
                  FUNCTION TRIM(LK-CTA-TIPO-TXT(LK-IDX))
              DELIMITED BY SIZE
              INTO LK-TX-LINHA.
           DISPLAY LK-TX-LINHA.

      * Linha SALDO
           MOVE SPACES TO LK-TX-LINHA.
           STRING "SALDO   : " DELIMITED BY SIZE
                  LK-CTA-SALDO(LK-IDX) DELIMITED BY SIZE
              INTO LK-TX-LINHA.
           DISPLAY LK-TX-LINHA.

      * Linha STATUS
           MOVE SPACES TO LK-TX-LINHA.
           STRING "STATUS  : " DELIMITED BY SIZE
                  FUNCTION TRIM(LK-CTA-STATUS(LK-IDX))
              DELIMITED BY SIZE
              INTO LK-TX-LINHA.
           DISPLAY LK-TX-LINHA.

      * Separador entre registros
           DISPLAY "*******".
       1000-FIM.
           EXIT.
******************************************************************