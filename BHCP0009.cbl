******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0009
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 01/06/2026
      * OBJETIVO..: Cadastro e relatorio de funcionarios (Array Fixo)
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 01.06.26 Felipe Brandim  IMPLANTACAO
      * ----------------------------------------------------------------
******************************************************************
 
******************************************************************
       IDENTIFICATION DIVISION.
******************************************************************
 
       PROGRAM-ID. BHCP0009.
 
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
      * Massa de dados bruta com FILLER
       01 DADOS-PESSOAS.
          03 FILLER           PIC X(18) VALUE "1001JOAO      3200".
          03 FILLER           PIC X(18) VALUE "1002MARIA     5000".
          03 FILLER           PIC X(18) VALUE "1003CARLOS    8200".
          03 FILLER           PIC X(18) VALUE "1004ANA       4100".
          03 FILLER           PIC X(18) VALUE "1005PEDRO     5100".
          03 FILLER           PIC X(18) VALUE "1006LUCAS     2800".
          03 FILLER           PIC X(18) VALUE "1007FERNANDA  5000".
          03 FILLER           PIC X(18) VALUE "1008PAULO     9100".
          03 FILLER           PIC X(18) VALUE "1009MARTA     3900".
          03 FILLER           PIC X(18) VALUE "1010JULIANA   6000".

      
      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      * Mapeamento da tabela com REDEFINES e OCCURS
       01 TABELA-PESSOAS REDEFINES DADOS-PESSOAS.
          03 GDA-FUNCIONARIO OCCURS 10 TIMES.
             05 FUNC-CODIGO   PIC 9(04).
             05 FUNC-NOME     PIC X(10).
             05 FUNC-SALARIO  PIC 9(04).

      * Variáveis de controle de iteração e montagem do relatório
       01 GDA-CONTROLE.
          03 GDA-IDX          PIC 9(02) VALUE 1.
          03 GDA-TX-SITUACAO  PIC X(07) VALUE SPACES.
          03 GDA-TX-LINHA     PIC X(50) VALUE SPACES.
      *----------------------------------------
  
******************************************************************
       PROCEDURE DIVISION.
******************************************************************
 
       0000-PRINCIPAL SECTION.
       0000-INICIO.
           DISPLAY "RELATORIO DE FUNCIONARIOS".
           DISPLAY " ".

      * O PERFORM dispara a section de relatório
           PERFORM 1000-RELATORIO
              VARYING GDA-IDX FROM 1 BY 1 UNTIL GDA-IDX > 10.
           GOBACK.

       1000-RELATORIO SECTION.
       1000-PROCESSAR-TABELA.
      * Lógica de classificação
           IF FUNC-SALARIO(GDA-IDX) > 5000 THEN
              MOVE "ALTO" TO GDA-TX-SITUACAO
           ELSE
              IF FUNC-SALARIO(GDA-IDX) = 5000 THEN
                 MOVE "LIMITE" TO GDA-TX-SITUACAO
              ELSE
                 MOVE "NORMAL" TO GDA-TX-SITUACAO
              END-IF
           END-IF.

           MOVE SPACES TO GDA-TX-LINHA.

      * Montagem da linha usando STRING
           STRING FUNC-CODIGO(GDA-IDX) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNC-NOME(GDA-IDX) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNC-SALARIO(GDA-IDX) DELIMITED BY SIZE
                  ",00 " DELIMITED BY SIZE
                  GDA-TX-SITUACAO DELIMITED BY SIZE
              INTO GDA-TX-LINHA.

           DISPLAY GDA-TX-LINHA.

       1000-FIM.
           EXIT.
******************************************************************