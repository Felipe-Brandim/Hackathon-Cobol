******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0010
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 03/07/2026
      * OBJETIVO..: Cadastro de produtos com Array Dinamico (EVALUATE)
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 03.07.26 Felipe Brandim  IMPLANTACAO COM ACERTO DE ESCOPO
      * ----------------------------------------------------------------
******************************************************************
 
******************************************************************
       IDENTIFICATION DIVISION.
******************************************************************
 
       PROGRAM-ID. BHCP0010.
 
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
      * ESTRUTURA ESTÁTICA: O esqueleto e a capacidade maxima do array
       01 GDA-ESTOQUE.
          03 GDA-QTDE-PROD     PIC 9(03) VALUE ZERO.
          03 GDA-TAB-PRODUTOS OCCURS 1 TO 100 TIMES
                DEPENDING ON GDA-QTDE-PROD.
             05 PROD-CODIGO    PIC 9(03).
             05 PROD-DESC      PIC X(12).
             05 PROD-PRECO     PIC 9(04).

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
      * ESTRUTURA DINÂMICA: Variaveis transientes de controle e buffers de tela
       01 GDA-CONTROLE.
          03 GDA-IDX           PIC 9(03) VALUE 1.
          03 GDA-TX-CLASSIF    PIC X(10) VALUE SPACES.
          03 GDA-TX-LINHA      PIC X(40) VALUE SPACES.
  
******************************************************************
       PROCEDURE DIVISION.
******************************************************************
 
       0000-PRINCIPAL SECTION.
       0000-INICIO.
      * CORREÇÃO: Chamando a SECTION inteira para englobar o parágrafo FIM
           PERFORM 1000-CADASTRO.

      * CORREÇÃO: Chamando a SECTION no laço de repetição dinâmico
           PERFORM 2000-RELATORIO
              VARYING GDA-IDX FROM 1 BY 1 UNTIL GDA-IDX > GDA-QTDE-PROD.

           GOBACK.

       1000-CADASTRO SECTION.
       1000-CADASTRAR-PRODUTOS.
      * Definindo a quantidade real de elementos ativos
           MOVE 10 TO GDA-QTDE-PROD.

      * Populando o array posicao por posicao com a massa sugerida
           MOVE 101 TO PROD-CODIGO(1). 
           MOVE "MOUSE" TO PROD-DESC(1).  
           MOVE 80 TO PROD-PRECO(1).
           MOVE 102 TO PROD-CODIGO(2).  
           MOVE "MONITOR" TO PROD-DESC(2).  
           MOVE 900 TO PROD-PRECO(2).
           MOVE 103 TO PROD-CODIGO(3).  
           MOVE "TECLADO" TO PROD-DESC(3).  
           MOVE 150 TO PROD-PRECO(3).
           MOVE 104 TO PROD-CODIGO(4).  
           MOVE "NOTEBOOK" TO PROD-DESC(4).  
           MOVE 4200 TO PROD-PRECO(4).
           MOVE 105 TO PROD-CODIGO(5).  
           MOVE "HEADSET" TO PROD-DESC(5).  
           MOVE 280 TO PROD-PRECO(5).
           MOVE 106 TO PROD-CODIGO(6).  
           MOVE "WEBCAM" TO PROD-DESC(6).  
           MOVE 320 TO PROD-PRECO(6).
           MOVE 107 TO PROD-CODIGO(7).  
           MOVE "SSD" TO PROD-DESC(7).  
           MOVE 550 TO PROD-PRECO(7).
           MOVE 108 TO PROD-CODIGO(8).  
           MOVE "PENDRIVE" TO PROD-DESC(8).  
           MOVE 60 TO PROD-PRECO(8).
           MOVE 109 TO PROD-CODIGO(9).  
           MOVE "HD EXTERNO" TO PROD-DESC(9).  
           MOVE 480 TO PROD-PRECO(9).
           MOVE 110 TO PROD-CODIGO(10). 
           MOVE "IMPRESSAORA" TO PROD-DESC(10). 
           MOVE 1100 TO PROD-PRECO(10).
       1000-FIM.
           EXIT.

       2000-RELATORIO SECTION.
       2000-PROCESSAR-ESTOQUE.
      * Classificacao atendendo obrigatoriamente os desafios de sintaxe 
           EVALUATE TRUE
           WHEN PROD-PRECO(GDA-IDX) LESS 100
                MOVE "BARATO" TO GDA-TX-CLASSIF
           WHEN PROD-PRECO(GDA-IDX) NOT LESS 100 AND
              PROD-PRECO(GDA-IDX) NOT GREATER 500
                MOVE "NORMAL" TO GDA-TX-CLASSIF
           WHEN PROD-PRECO(GDA-IDX) GREATER 500
                MOVE "CARO" TO GDA-TX-CLASSIF
           WHEN OTHER
                MOVE "INVALIDO" TO GDA-TX-CLASSIF
           END-EVALUATE.

      * Atendendo ao desafio de usar o operador NOT EQUAL no EVALUATE
           EVALUATE TRUE
           WHEN GDA-TX-CLASSIF NOT EQUAL "INVALIDO"
                MOVE SPACES TO GDA-TX-LINHA
                   
      * Alinhamento preservado ao manter o tamanho nativo de PROD-DESC
                STRING PROD-CODIGO(GDA-IDX) DELIMITED BY SIZE
                       " " DELIMITED BY SIZE
                       PROD-DESC(GDA-IDX) DELIMITED BY SIZE
                       " " DELIMITED BY SIZE
                       GDA-TX-CLASSIF DELIMITED BY SIZE
                   INTO GDA-TX-LINHA
                          
                DISPLAY GDA-TX-LINHA
           END-EVALUATE.
       2000-FIM.
           EXIT.
******************************************************************