******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP00008
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 27/06/2026
      * OBJETIVO..: Processamento de vendas (paragrafos e perform)
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
 
       PROGRAM-ID. BHCP00008.
 
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
      * Valores das vendas fixados na memória
       01 GDA-VENDAS.
          03 GDA-NR-VENDA-1   PIC 9(03) VALUE 100.
          03 GDA-NR-VENDA-2   PIC 9(03) VALUE 200.
          03 GDA-NR-VENDA-3   PIC 9(03) VALUE 150.
          03 GDA-NR-VENDA-4   PIC 9(03) VALUE 300.
          03 GDA-NR-VENDA-5   PIC 9(03) VALUE 400.

      * Variáveis com tamanho exato dos resultados para evitar poluição
       01 GDA-CONTROLE.
          03 GDA-NR-TOTAL     PIC 9(04) VALUE ZERO.
          03 GDA-NR-QTDE      PIC 9(01) VALUE ZERO.
          03 GDA-NR-MEDIA     PIC 9(03) VALUE ZERO.
          03 GDA-TX-SITUACAO  PIC X(13) VALUE SPACES.
      
      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
  
******************************************************************
       PROCEDURE DIVISION.
******************************************************************

       0000-PRINCIPAL.
      * O fluxo do programa começa obrigatoriamente aqui no topo
           PERFORM 1000-PROCESSA-VENDAS.
           PERFORM 2000-AVALIA-META.

           DISPLAY "RESUMO DE VENDAS".
           DISPLAY "QUANTIDADE: " GDA-NR-QTDE.
           DISPLAY "TOTAL: " GDA-NR-TOTAL.
           DISPLAY "MEDIA: " GDA-NR-MEDIA.
           DISPLAY "SITUACAO: " GDA-TX-SITUACAO.

           GOBACK.

       1000-PROCESSA-VENDAS.
           COMPUTE GDA-NR-TOTAL = GDA-NR-VENDA-1 + GDA-NR-VENDA-2
              + GDA-NR-VENDA-3 + GDA-NR-VENDA-4
              + GDA-NR-VENDA-5.
           ADD 5 TO GDA-NR-QTDE.

       2000-AVALIA-META.
           COMPUTE GDA-NR-MEDIA = GDA-NR-TOTAL / GDA-NR-QTDE.

           IF GDA-NR-TOTAL >= 1000 THEN
              MOVE "META ATINGIDA" TO GDA-TX-SITUACAO
           ELSE
              MOVE "META NAO ATINGIDA" TO GDA-TX-SITUACAO
           END-IF.
******************************************************************