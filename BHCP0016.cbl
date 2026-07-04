******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0016
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 03/07/2026
      * OBJETIVO..: Geracao de Arquivo JSON a partir de Sequencial
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 003 03.07.26 Felipe Brandim  FIX DE LEITURA SEQUENTIAL E SYSOUT
      * ----------------------------------------------------------------
******************************************************************
 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BHCP0016.
 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * FIX: Alterado para SEQUENTIAL puro para ler o arquivo do Ex 12
           SELECT BHCF012S ASSIGN TO "BHCF012S.txt"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS GDA-FS-BHCF012S.

           SELECT BHCF016J ASSIGN TO "BHCF016J.json"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS GDA-FS-BHCF016J.
 
       DATA DIVISION.
       FILE SECTION.
       FD  BHCF012S
           RECORDING MODE IS F
           RECORD CONTAINS 65 CHARACTERS.
       01 REG-BHCF012S         PIC X(065).

       FD  BHCF016J.
       01 REG-BHCF016J         PIC X(200).
 
      *----------------------------------------
       WORKING-STORAGE                 SECTION.
      *----------------------------------------
       01 GDA-GR-BHCF0125.
          03 FD-CODIGO         PIC X(005).
          03 FD-NOME           PIC X(030).
          03 FD-UF             PIC X(002).
          03 FD-TRILHA         PIC X(010).
          03 FD-SITUACAO       PIC X(010).
          03 FD-DATA           PIC X(008).

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01 GDA-FILE-STATUS.
          03 GDA-FS-BHCF012S   PIC X(002) VALUE SPACES.
          03 GDA-FS-BHCF016J   PIC X(002) VALUE SPACES.
          03 GDA-FS-OK         PIC X(002) VALUE '00'.

       01 WS-FIM-ARQUIVO       PIC X(001) VALUE 'N'.
          88 WS-FIM-SIM                   VALUE 'S'.
          88 WS-FIM-NAO                   VALUE 'N'.

       01 GDA-TOTALIZADORES.
          03 GDA-TOT-LIDOS     PIC 9(005) VALUE ZEROS.
          03 GDA-TOT-JSON      PIC 9(005) VALUE ZEROS.
          03 GDA-TOT-ERROS     PIC 9(005) VALUE ZEROS.

       01 GDA-DATA-HORA.
          03 GDA-CURRENT-DATE  PIC X(021).
  
******************************************************************
       PROCEDURE DIVISION.
******************************************************************
 
       000000-ROTINA-PRINCIPAL SECTION.
       0000-MAIN.
           PERFORM 100000-INICIALIZAR.
           PERFORM 200000-ABRIR-ARQUIVOS.
           PERFORM 300000-GRAVAR-CABECALHO-JSON.
           PERFORM 400000-LER-ENTRADA.
           PERFORM 800000-GRAVAR-RODAPE-JSON.
           PERFORM 900000-FECHAR-ARQUIVOS.
           PERFORM 910000-EXIBIR-TOTALIZADORES.
           GOBACK.

       100000-INICIALIZAR.
           MOVE ZEROS TO GDA-TOT-LIDOS GDA-TOT-JSON GDA-TOT-ERROS.
           SET WS-FIM-NAO TO TRUE.
           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.
           DISPLAY 'BHCP0016 - INICIO DO PROCESSAMENTO'.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.

       200000-ABRIR-ARQUIVOS.
           OPEN INPUT BHCF012S.
           IF GDA-FS-BHCF012S = GDA-FS-OK THEN
              DISPLAY 'ARQUIVO BHCF012S ABERTO COM SUCESSO'
           ELSE
              DISPLAY 'ERRO OPEN BHCF012S FS: ' GDA-FS-BHCF012S
              ADD 1 TO GDA-TOT-ERROS
              SET WS-FIM-SIM TO TRUE
           END-IF.

           OPEN OUTPUT BHCF016J.
           IF GDA-FS-BHCF016J = GDA-FS-OK THEN
              DISPLAY 'ARQUIVO BHCF016J ABERTO COM SUCESSO'
           ELSE
              DISPLAY 'ERRO OPEN BHCF016J FS: ' GDA-FS-BHCF016J
              ADD 1 TO GDA-TOT-ERROS
              SET WS-FIM-SIM TO TRUE
           END-IF.

       300000-GRAVAR-CABECALHO-JSON.
           IF GDA-TOT-ERROS = ZEROS THEN
              MOVE '{' TO REG-BHCF016J
              WRITE REG-BHCF016J
              MOVE '  "participantes": [' TO REG-BHCF016J
              WRITE REG-BHCF016J
           END-IF.

       400000-LER-ENTRADA.
           PERFORM UNTIL WS-FIM-SIM
                   READ BHCF012S INTO GDA-GR-BHCF0125
                   AT END
                      SET WS-FIM-SIM TO TRUE
                   NOT AT END
                       ADD 1 TO GDA-TOT-LIDOS
                       PERFORM 500000-PROCESSAR-ARQUIVO
                   END-READ
           END-PERFORM.

       500000-PROCESSAR-ARQUIVO.
           IF GDA-TOT-LIDOS > 1 THEN
              MOVE '    ,' TO REG-BHCF016J
              WRITE REG-BHCF016J
           END-IF.
           PERFORM 510000-GRAVAR-OBJETO-JSON.

       510000-GRAVAR-OBJETO-JSON.
           MOVE '    {' TO REG-BHCF016J.
           WRITE REG-BHCF016J.

           MOVE SPACES TO REG-BHCF016J.
           STRING '      "codigo": "' DELIMITED BY SIZE
                  FUNCTION TRIM(FD-CODIGO) DELIMITED BY SIZE
                  '",' DELIMITED BY SIZE
              INTO REG-BHCF016J.
           WRITE REG-BHCF016J.

           MOVE SPACES TO REG-BHCF016J.
           STRING '      "nome": "' DELIMITED BY SIZE
                  FUNCTION TRIM(FD-NOME) DELIMITED BY SIZE
                  '",' DELIMITED BY SIZE
              INTO REG-BHCF016J.
           WRITE REG-BHCF016J.

           MOVE SPACES TO REG-BHCF016J.
           STRING '      "uf": "' DELIMITED BY SIZE
                  FUNCTION TRIM(FD-UF) DELIMITED BY SIZE
                  '",' DELIMITED BY SIZE
              INTO REG-BHCF016J.
           WRITE REG-BHCF016J.

           MOVE SPACES TO REG-BHCF016J.
           STRING '      "trilha": "' DELIMITED BY SIZE
                  FUNCTION TRIM(FD-TRILHA) DELIMITED BY SIZE
                  '",' DELIMITED BY SIZE
              INTO REG-BHCF016J.
           WRITE REG-BHCF016J.

           MOVE SPACES TO REG-BHCF016J.
           STRING '      "situacao": "' DELIMITED BY SIZE
                  FUNCTION TRIM(FD-SITUACAO) DELIMITED BY SIZE
                  '",' DELIMITED BY SIZE
              INTO REG-BHCF016J.
           WRITE REG-BHCF016J.

           MOVE SPACES TO REG-BHCF016J.
           STRING '      "data": "' DELIMITED BY SIZE
                  FUNCTION TRIM(FD-DATA) DELIMITED BY SIZE
                  '"' DELIMITED BY SIZE
              INTO REG-BHCF016J.
           WRITE REG-BHCF016J.

           MOVE '    }' TO REG-BHCF016J.
           WRITE REG-BHCF016J.
           ADD 1 TO GDA-TOT-JSON.

       800000-GRAVAR-RODAPE-JSON.
           IF GDA-TOT-ERROS = ZEROS THEN
              MOVE '  ]' TO REG-BHCF016J
              WRITE REG-BHCF016J
              MOVE '}' TO REG-BHCF016J
              WRITE REG-BHCF016J
           END-IF.

       900000-FECHAR-ARQUIVOS.
           CLOSE BHCF012S BHCF016J.

       910000-EXIBIR-TOTALIZADORES.
      * FIX: Reordenado para exibir os totais antes da mensagem de fim
           DISPLAY 'TOTAL DE REGISTROS LIDOS......: ' GDA-TOT-LIDOS.
           DISPLAY 'TOTAL DE OBJETOS JSON GERADOS.: ' GDA-TOT-JSON.
           DISPLAY 'TOTAL DE ERROS DE ARQUIVO.....: ' GDA-TOT-ERROS.
           DISPLAY 'BHCP0016 - FIM DO PROCESSAMENTO'.
           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.