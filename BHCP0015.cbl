******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0015
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 03/07/2026
      * OBJETIVO..: Processamento de Lancamentos de Conta Corrente
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 002 03.07.26 Felipe Brandim  CORRECAO DE MATEMATICA E SYSOUT
      * ----------------------------------------------------------------
******************************************************************
 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BHCP0015.
 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BHCF015E ASSIGN TO "BHCF015E.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FS-BHCF015E.

           SELECT BHCF015S ASSIGN TO "BHCF015S.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FS-BHCF015S.

           SELECT BHCF015L ASSIGN TO "BHCF015L.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FS-BHCF015L.
 
       DATA DIVISION.
       FILE SECTION.
       FD  BHCF015E
           RECORDING MODE IS F
           RECORD CONTAINS 53 CHARACTERS.
       01 REG-BHCF015E.
          05 IN-COD-CONTA          PIC 9(005).
          05 IN-DATA-LANC          PIC 9(008).
          05 IN-TIPO-LANC          PIC X(001).
          05 IN-VALOR-LANC         PIC 9(007)V99.
          05 IN-HISTORICO          PIC X(030).

       FD  BHCF015S
           RECORDING MODE IS F
           RECORD CONTAINS 62 CHARACTERS.
       01 REG-BHCF015S.
          05 OUT-COD-CONTA         PIC 9(005).
          05 OUT-DATA-LANC         PIC 9(008).
          05 OUT-TIPO-LANC         PIC X(001).
          05 OUT-VALOR-LANC        PIC 9(007)V99.
          05 OUT-HISTORICO         PIC X(030).
          05 OUT-SALDO-APOS        PIC 9(007)V99.

       FD  BHCF015L
           RECORDING MODE IS F
           RECORD CONTAINS 67 CHARACTERS.
       01 REG-BHCF015L.
          05 LOG-COD-CONTA         PIC 9(005).
          05 LOG-DATA-LANC         PIC 9(008).
          05 LOG-TIPO-LANC         PIC X(001).
          05 LOG-VALOR-LANC        PIC 9(007)V99.
          05 LOG-COD-ERRO          PIC X(004).
          05 LOG-MENSAGEM          PIC X(040).
 
      *----------------------------------------
       WORKING-STORAGE                 SECTION.
      *----------------------------------------
       01 GDA-VALORES-FIXOS.
          05 GDA-SALDO-INICIAL     PIC 9(007)V99 VALUE 1000.00.
          05 GDA-SALDO-ATUAL       PIC 9(007)V99 VALUE 1000.00.

       01 WS-CONSTANTES.
          05 WS-FS-OK              PIC X(002)    VALUE '00'.
          05 WS-FS-EOF             PIC X(002)    VALUE '10'.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01 WS-FILE-STATUS.
          05 WS-FS-BHCF015E        PIC X(002)    VALUE SPACES.
          05 WS-FS-BHCF015S        PIC X(002)    VALUE SPACES.
          05 WS-FS-BHCF015L        PIC X(002)    VALUE SPACES.

       01 GDA-CONTROLES.
          05 GDA-FIM-ARQ           PIC X(001)    VALUE 'N'.
             88 GDA-FIM-SIM                      VALUE 'S'.
             88 GDA-FIM-NAO                      VALUE 'N'.
          05 GDA-STATUS-REG        PIC X(001)    VALUE SPACE.
             88 GDA-REG-VALIDO                   VALUE 'V'.
             88 GDA-REG-REJEITADO                VALUE 'R'.
          05 GDA-COD-ERRO          PIC X(004)    VALUE SPACES.
          05 GDA-MENSAGEM          PIC X(040)    VALUE SPACES.

       01 GDA-TOTALIZADORES.
          05 GDA-TOT-LIDOS         PIC 9(005)    VALUE ZEROS.
          05 GDA-TOT-VALIDOS       PIC 9(005)    VALUE ZEROS.
          05 GDA-TOT-REJEITADOS    PIC 9(005)    VALUE ZEROS.
          05 GDA-TOT-ERROS         PIC 9(005)    VALUE ZEROS.
          05 GDA-QTDE-DEP          PIC 9(005)    VALUE ZEROS.
          05 GDA-QTDE-SAQ          PIC 9(005)    VALUE ZEROS.
          05 GDA-QTDE-TRA          PIC 9(005)    VALUE ZEROS.

       01 GDA-ACUMULADORES.
          05 GDA-VAL-DEP           PIC 9(009)V99 VALUE ZEROS.
          05 GDA-VAL-SAQ           PIC 9(009)V99 VALUE ZEROS.
          05 GDA-VAL-TRA           PIC 9(009)V99 VALUE ZEROS.

      * Variáveis de formatação exclusivas para saída limpa da SYSOUT
       01 WS-SINO-DISPLAY.
          05 DISP-COUNT            PIC 9(005).
          05 DISP-VAL-11           PIC 9(011).
          05 DISP-VAL-12           PIC 9(012).
  
******************************************************************
       PROCEDURE DIVISION.
******************************************************************
 
       0000-PRINCIPAL SECTION.
       0000-MAIN.
           PERFORM 1000-INICIALIZAR.
           PERFORM 2000-ABRIR-ARQUIVOS.
           PERFORM 3000-PROCESSAR-ARQUIVO.
           PERFORM 8000-FECHAR-ARQUIVOS.
           PERFORM 9000-EXIBIR-RESUMO.
           GOBACK.

       1000-INICIALIZAR.
           SET GDA-FIM-NAO TO TRUE.
           DISPLAY 'BHCP0015 - PROCESSAMENTO DE LANCAMENTOS'.

       2000-ABRIR-ARQUIVOS.
           OPEN INPUT BHCF015E.
           IF WS-FS-BHCF015E NOT = WS-FS-OK
              DISPLAY 'ERRO OPEN BHCF015E: ' WS-FS-BHCF015E
              ADD 1 TO GDA-TOT-ERROS
              SET GDA-FIM-SIM TO TRUE
           END-IF.

           OPEN OUTPUT BHCF015S.
           IF WS-FS-BHCF015S NOT = WS-FS-OK
              DISPLAY 'ERRO OPEN BHCF015S: ' WS-FS-BHCF015S
              ADD 1 TO GDA-TOT-ERROS
              SET GDA-FIM-SIM TO TRUE
           END-IF.

           OPEN OUTPUT BHCF015L.
           IF WS-FS-BHCF015L NOT = WS-FS-OK
              DISPLAY 'ERRO OPEN BHCF015L: ' WS-FS-BHCF015L
              ADD 1 TO GDA-TOT-ERROS
              SET GDA-FIM-SIM TO TRUE
           END-IF.

       3000-PROCESSAR-ARQUIVO.
           PERFORM UNTIL GDA-FIM-SIM
                   PERFORM 3100-LER-ENTRADA
                   IF GDA-FIM-NAO
                      PERFORM 3200-VALIDAR-LANCAMENTO
                      IF GDA-REG-VALIDO
                         PERFORM 3300-PROCESSAR-LANCAMENTO
                         PERFORM 3400-GRAVAR-SAIDA
                      ELSE
                         PERFORM 3500-GRAVAR-LOG
                      END-IF
                   END-IF
           END-PERFORM.

       3100-LER-ENTRADA.
           READ BHCF015E
           AT END
              SET GDA-FIM-SIM TO TRUE
           NOT AT END
               ADD 1 TO GDA-TOT-LIDOS
           END-READ.
           IF WS-FS-BHCF015E NOT = WS-FS-OK AND
              WS-FS-BHCF015E NOT = WS-FS-EOF
              DISPLAY 'ERRO READ BHCF015E: ' WS-FS-BHCF015E
              ADD 1 TO GDA-TOT-ERROS
              SET GDA-FIM-SIM TO TRUE
           END-IF.

       3200-VALIDAR-LANCAMENTO.
           SET GDA-REG-VALIDO TO TRUE.
           MOVE SPACES TO GDA-COD-ERRO GDA-MENSAGEM.

           IF IN-COD-CONTA = ZEROS THEN
              SET GDA-REG-REJEITADO TO TRUE
              MOVE 'E001' TO GDA-COD-ERRO
              MOVE 'CODIGO DA CONTA ZERADO' TO GDA-MENSAGEM
           ELSE
              IF IN-DATA-LANC = ZEROS THEN
                 SET GDA-REG-REJEITADO TO TRUE
                 MOVE 'E002' TO GDA-COD-ERRO
                 MOVE 'DATA DO LANCAMENTO ZERADA' TO GDA-MENSAGEM
              ELSE
                 IF IN-TIPO-LANC NOT = 'D' AND IN-TIPO-LANC NOT = 'S'
                    AND IN-TIPO-LANC NOT = 'T' THEN
                    SET GDA-REG-REJEITADO TO TRUE
                    MOVE 'E003' TO GDA-COD-ERRO
                    MOVE 'TIPO DE OPERACAO INVALIDO' TO GDA-MENSAGEM
                 ELSE
                    IF IN-VALOR-LANC <= ZERO THEN
                       SET GDA-REG-REJEITADO TO TRUE
                       MOVE 'E004' TO GDA-COD-ERRO
                       MOVE 'VALOR DO LANCAMENTO INVALIDO' TO
                          GDA-MENSAGEM
                    ELSE
                       IF IN-HISTORICO = SPACES THEN
                          SET GDA-REG-REJEITADO TO TRUE
                          MOVE 'E005' TO GDA-COD-ERRO
                          MOVE 'HISTORICO NAO PREENCHIDO' TO
                             GDA-MENSAGEM
                       ELSE
                          IF (IN-TIPO-LANC = 'S' OR IN-TIPO-LANC = 'T')
                             AND
                             IN-VALOR-LANC > GDA-SALDO-ATUAL THEN
                             SET GDA-REG-REJEITADO TO TRUE
                             MOVE 'E006' TO GDA-COD-ERRO
                             MOVE 'SALDO INSUFICIENTE' TO GDA-MENSAGEM
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.

       3300-PROCESSAR-LANCAMENTO.
           ADD 1 TO GDA-TOT-VALIDOS.
           EVALUATE IN-TIPO-LANC
           WHEN 'D'
                ADD IN-VALOR-LANC TO GDA-SALDO-ATUAL
                ADD 1 TO GDA-QTDE-DEP
                ADD IN-VALOR-LANC TO GDA-VAL-DEP
           WHEN 'S'
                SUBTRACT IN-VALOR-LANC FROM GDA-SALDO-ATUAL
                ADD 1 TO GDA-QTDE-SAQ
                ADD IN-VALOR-LANC TO GDA-VAL-SAQ
           WHEN 'T'
                SUBTRACT IN-VALOR-LANC FROM GDA-SALDO-ATUAL
                ADD 1 TO GDA-QTDE-TRA
                ADD IN-VALOR-LANC TO GDA-VAL-TRA
           END-EVALUATE.

       3400-GRAVAR-SAIDA.
           MOVE IN-COD-CONTA TO OUT-COD-CONTA.
           MOVE IN-DATA-LANC TO OUT-DATA-LANC.
           MOVE IN-TIPO-LANC TO OUT-TIPO-LANC.
           MOVE IN-VALOR-LANC TO OUT-VALOR-LANC.
           MOVE IN-HISTORICO TO OUT-HISTORICO.
           MOVE GDA-SALDO-ATUAL TO OUT-SALDO-APOS.

           WRITE REG-BHCF015S.
           IF WS-FS-BHCF015S NOT = WS-FS-OK
              DISPLAY 'ERRO WRITE BHCF015S: ' WS-FS-BHCF015S
              ADD 1 TO GDA-TOT-ERROS
           END-IF.

       3500-GRAVAR-LOG.
           ADD 1 TO GDA-TOT-REJEITADOS.
           MOVE IN-COD-CONTA TO LOG-COD-CONTA.
           MOVE IN-DATA-LANC TO LOG-DATA-LANC.
           MOVE IN-TIPO-LANC TO LOG-TIPO-LANC.
           MOVE IN-VALOR-LANC TO LOG-VALOR-LANC.
           MOVE GDA-COD-ERRO TO LOG-COD-ERRO.
           MOVE GDA-MENSAGEM TO LOG-MENSAGEM.

           WRITE REG-BHCF015L.
           IF WS-FS-BHCF015L NOT = WS-FS-OK
              DISPLAY 'ERRO WRITE BHCF015L: ' WS-FS-BHCF015L
              ADD 1 TO GDA-TOT-ERROS
           END-IF.

       8000-FECHAR-ARQUIVOS.
           CLOSE BHCF015E BHCF015S BHCF015L.
           DISPLAY 'BHCP0015 - FIM DO PROCESSAMENTO'.

       9000-EXIBIR-RESUMO.
           DISPLAY '---------------------------------------'.
           MOVE GDA-TOT-LIDOS TO DISP-COUNT.
           DISPLAY 'TOTAL DE REGISTROS LIDOS.......: ' DISP-COUNT.
           MOVE GDA-TOT-VALIDOS TO DISP-COUNT.
           DISPLAY 'TOTAL DE REGISTROS VALIDOS.....: ' DISP-COUNT.
           MOVE GDA-TOT-REJEITADOS TO DISP-COUNT.
           DISPLAY 'TOTAL DE REGISTROS REJEITADOS..: ' DISP-COUNT.
           MOVE GDA-QTDE-DEP TO DISP-COUNT.
           DISPLAY 'TOTAL DE DEPOSITOS.............: ' DISP-COUNT.
           MOVE GDA-QTDE-SAQ TO DISP-COUNT.
           DISPLAY 'TOTAL DE SAQUES................: ' DISP-COUNT.
           MOVE GDA-QTDE-TRA TO DISP-COUNT.
           DISPLAY 'TOTAL DE TRANSFERENCIAS........: ' DISP-COUNT.
           
           COMPUTE DISP-VAL-11 = GDA-VAL-DEP * 100.
           DISPLAY 'VALOR TOTAL DEPOSITOS..........: ' DISP-VAL-11.
           COMPUTE DISP-VAL-11 = GDA-VAL-SAQ * 100.
           DISPLAY 'VALOR TOTAL SAQUES.............: ' DISP-VAL-11.
           COMPUTE DISP-VAL-11 = GDA-VAL-TRA * 100.
           DISPLAY 'VALOR TOTAL TRANSFERENCIAS.....: ' DISP-VAL-11.
           
           COMPUTE DISP-VAL-12 = GDA-SALDO-INICIAL * 100.
           DISPLAY 'SALDO INICIAL..................: ' DISP-VAL-12.
           COMPUTE DISP-VAL-12 = GDA-SALDO-ATUAL * 100.
           DISPLAY 'SALDO FINAL....................: ' DISP-VAL-12.
           MOVE GDA-TOT-ERROS TO DISP-COUNT.
           DISPLAY 'TOTAL DE ERROS DE ARQUIVO......: ' DISP-COUNT.
           DISPLAY '---------------------------------------'.