******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0013
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 03/07/2026
      * OBJETIVO..: Ler participantes e gerar log de rejeicoes
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 03.07.26 Felipe Brandim  IMPLANTACAO COM READ INTO
      * ----------------------------------------------------------------
******************************************************************
 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BHCP0013.
 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BHCF012S ASSIGN TO "BHCF012S.txt"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-FS-BHCF012S.

           SELECT BHCF013L ASSIGN TO "BHCF013L.txt"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-FS-BHCF013L.
 
       DATA DIVISION.
       FILE SECTION.
       FD  BHCF012S
           RECORDING MODE IS F
           RECORD CONTAINS 65 CHARACTERS.
       01 REG-BHCF012S             PIC X(065).

       FD  BHCF013L
           RECORDING MODE IS F
           RECORD CONTAINS 79 CHARACTERS.
       01 REG-BHCF013L             PIC X(079).
 
      *----------------------------------------
       WORKING-STORAGE                 SECTION.
      *----------------------------------------
      * ESTRUTURA ESTÁTICA: Constantes do sistema
       01 WS-CONSTANTES.
          05 WS-FS-OK              PIC X(002) VALUE '00'.
          05 WS-FS-EOF             PIC X(002) VALUE '10'.

      * Area de manobra obrigatoria para o READ INTO
       01 WS-AREA-LEITURA.
          05 WS-CODIGO             PIC 9(005).
          05 WS-NOME               PIC X(030).
          05 WS-UF                 PIC X(002).
          05 WS-TRILHA             PIC X(010).
          05 WS-SITUACAO           PIC X(010).
          05 WS-DATA               PIC 9(008).

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
      * ESTRUTURA DINÂMICA: Status de arquivos, Niveis 88 e Contadores
       01 WS-FILE-STATUS.
          05 WS-FS-BHCF012S        PIC X(002) VALUE SPACES.
          05 WS-FS-BHCF013L        PIC X(002) VALUE SPACES.

       01 GDA-CONTROLES.
          05 GDA-FIM-ARQ           PIC X(001) VALUE 'N'.
             88 GDA-FIM-SIM                   VALUE 'S'.
             88 GDA-FIM-NAO                   VALUE 'N'.
          05 GDA-STATUS-REG        PIC X(001) VALUE SPACE.
             88 GDA-REG-VALIDO                VALUE 'V'.
             88 GDA-REG-REJEITADO             VALUE 'R'.
          05 GDA-COD-ERRO          PIC X(004) VALUE SPACES.
          05 GDA-MENSAGEM          PIC X(040) VALUE SPACES.

       01 GDA-TOTALIZADORES.
          05 GDA-TOT-LIDOS         PIC 9(005) VALUE ZEROS.
          05 GDA-TOT-VALIDOS       PIC 9(005) VALUE ZEROS.
          05 GDA-TOT-REJEITADOS    PIC 9(005) VALUE ZEROS.
          05 GDA-TOT-ERROS         PIC 9(005) VALUE ZEROS.

       01 GDA-DATA-HORA.
          05 GDA-CURRENT-DATE      PIC X(021).

       01 LOG-ESTRUTURA.
          05 LOG-CODIGO            PIC 9(005).
          05 LOG-NOME              PIC X(030).
          05 LOG-COD-ERRO          PIC X(004).
          05 LOG-MENSAGEM          PIC X(040).
  
******************************************************************
       PROCEDURE DIVISION.
******************************************************************
 
       000000-ROTINA-PRINCIPAL SECTION.
       0000-MAIN.
           PERFORM 100000-INICIALIZAR.
           PERFORM 200000-ABRIR-ARQUIVOS.
           PERFORM 300000-PROCESSAR-ARQUIVO.
           PERFORM 900000-FECHAR-ARQUIVOS.
           PERFORM 910000-EXIBIR-TOTALIZADORES.
           GOBACK.

       100000-INICIALIZAR.
           MOVE ZEROS TO GDA-TOT-LIDOS
                         GDA-TOT-VALIDOS
                         GDA-TOT-REJEITADOS
                         GDA-TOT-ERROS.
           SET GDA-FIM-NAO TO TRUE.
           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.
           DISPLAY 'BHCP0013 INICIO DO PROCESSAMENTO'.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.

       200000-ABRIR-ARQUIVOS.
           OPEN INPUT BHCF012S.
           IF WS-FS-BHCF012S NOT = WS-FS-OK
              DISPLAY 'ERRO OPEN INPUT BHCF012S: ' WS-FS-BHCF012S
              ADD 1 TO GDA-TOT-ERROS
              SET GDA-FIM-SIM TO TRUE
           END-IF.

           OPEN OUTPUT BHCF013L.
           IF WS-FS-BHCF013L NOT = WS-FS-OK
              DISPLAY 'ERRO OPEN OUTPUT BHCF013L: ' WS-FS-BHCF013L
              ADD 1 TO GDA-TOT-ERROS
              SET GDA-FIM-SIM TO TRUE
           END-IF.

       300000-PROCESSAR-ARQUIVO.
           PERFORM UNTIL GDA-FIM-SIM
                   PERFORM 310000-LER-PARTICIPANTE
                   IF GDA-FIM-NAO
                      PERFORM 400000-VALIDAR-PARTICIPANTE
                      IF GDA-REG-VALIDO
                         ADD 1 TO GDA-TOT-VALIDOS
                      ELSE
                         ADD 1 TO GDA-TOT-REJEITADOS
                         PERFORM 500000-GRAVAR-LOG
                      END-IF
                   END-IF
           END-PERFORM.

       310000-LER-PARTICIPANTE.
      * Executando o READ INTO obrigatorio para a area de manobra
           READ BHCF012S INTO WS-AREA-LEITURA
           AT END
              SET GDA-FIM-SIM TO TRUE
           NOT AT END
               ADD 1 TO GDA-TOT-LIDOS
           END-READ.

           IF WS-FS-BHCF012S NOT = WS-FS-OK AND
              WS-FS-BHCF012S NOT = WS-FS-EOF
              DISPLAY 'ERRO READ BHCF012S: ' WS-FS-BHCF012S
              ADD 1 TO GDA-TOT-ERROS
              SET GDA-FIM-SIM TO TRUE
           END-IF.

       400000-VALIDAR-PARTICIPANTE.
           SET GDA-REG-VALIDO TO TRUE.
           MOVE '0000' TO GDA-COD-ERRO.
           MOVE 'REGISTRO VALIDO' TO GDA-MENSAGEM.

      * Cascata de validacoes parando no primeiro erro encontrado
           IF WS-CODIGO = ZEROS THEN
              SET GDA-REG-REJEITADO TO TRUE
              MOVE 'E001' TO GDA-COD-ERRO
              MOVE 'CODIGO NAO INFORMADO' TO GDA-MENSAGEM
           ELSE
              IF WS-NOME = SPACES THEN
                 SET GDA-REG-REJEITADO TO TRUE
                 MOVE 'E002' TO GDA-COD-ERRO
                 MOVE 'NOME NAO INFORMADO' TO GDA-MENSAGEM
              ELSE
                 IF WS-UF = SPACES THEN
                    SET GDA-REG-REJEITADO TO TRUE
                    MOVE 'E003' TO GDA-COD-ERRO
                    MOVE 'UF NAO INFORMADA' TO GDA-MENSAGEM
                 ELSE
                    IF WS-TRILHA = SPACES THEN
                       SET GDA-REG-REJEITADO TO TRUE
                       MOVE 'E004' TO GDA-COD-ERRO
                       MOVE 'TRILHA NAO INFORMADA' TO GDA-MENSAGEM
                    ELSE
                       IF WS-SITUACAO = SPACES THEN
                          SET GDA-REG-REJEITADO TO TRUE
                          MOVE 'E005' TO GDA-COD-ERRO
                          MOVE 'SITUACAO NAO INFORMADA' TO GDA-MENSAGEM
                       ELSE
                          IF WS-DATA = ZEROS THEN
                             SET GDA-REG-REJEITADO TO TRUE
                             MOVE 'E006' TO GDA-COD-ERRO
                             MOVE 'DATA NAO INFORMADA' TO GDA-MENSAGEM
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.

       500000-GRAVAR-LOG.
           MOVE WS-CODIGO TO LOG-CODIGO.
           MOVE WS-NOME TO LOG-NOME.
           MOVE GDA-COD-ERRO TO LOG-COD-ERRO.
           MOVE GDA-MENSAGEM TO LOG-MENSAGEM.

           WRITE REG-BHCF013L FROM LOG-ESTRUTURA.
           IF WS-FS-BHCF013L NOT = WS-FS-OK
              DISPLAY 'ERRO WRITE BHCF013L: ' WS-FS-BHCF013L
              ADD 1 TO GDA-TOT-ERROS
           END-IF.

       900000-FECHAR-ARQUIVOS.
           CLOSE BHCF012S BHCF013L.
           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.
           DISPLAY 'BHCP0013 FIM DO PROCESSAMENTO'.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.

       910000-EXIBIR-TOTALIZADORES.
           DISPLAY 'TOTAL DE REGISTROS LIDOS......: ' GDA-TOT-LIDOS.
           DISPLAY 'TOTAL DE REGISTROS VALIDOS....: ' GDA-TOT-VALIDOS.
           DISPLAY 'TOTAL DE REGISTROS REJEITADOS.: '
                   GDA-TOT-REJEITADOS.
           DISPLAY 'TOTAL DE ERROS DE ARQUIVO.....: ' GDA-TOT-ERROS.