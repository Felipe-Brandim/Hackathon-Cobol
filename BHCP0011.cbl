******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0011
      * ANALISTA..: José Hilario Veras Leite Junior
      * AUTOR.....: Felipe Fernandes Brandim   
      * DATA......: 03/07/2026
      * OBJETIVO..: Sistema Bancario - Programa Principal (CALL)
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 002 03.07.26 Felipe Brandim  CORRECAO DE UNREACHABLE CODE
      * ----------------------------------------------------------------
******************************************************************
 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BHCP0011.
 
       ENVIRONMENT DIVISION.
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
 
       DATA DIVISION. 
      *----------------------------------------
       WORKING-STORAGE                 SECTION.
      *----------------------------------------
      * Layout compartilhado que vai para o Subprograma
       01 GDA-SISTEMA-BANCARIO.
          03 GDA-QTDE-CONTAS    PIC 9(02) VALUE ZERO.
          03 GDA-TAB-CONTAS OCCURS 1 TO 50 TIMES
                DEPENDING ON GDA-QTDE-CONTAS.
             05 CTA-NUMERO      PIC 9(05).
             05 CTA-CLIENTE     PIC X(10).
             05 CTA-TIPO        PIC X(01).
             05 CTA-TIPO-TXT    PIC X(08).
             05 CTA-SALDO       PIC 9(05).
             05 CTA-STATUS      PIC X(08).

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
      * ESTRUTURA DINÂMICA: Variavel transiente para o laco local
       01 GDA-CONTROLE.
          03 GDA-IDX            PIC 9(02) VALUE 1.
  
******************************************************************
       PROCEDURE DIVISION.
******************************************************************
 
       0000-PRINCIPAL SECTION.
       0000-INICIO.
      * Popula a matriz dinamica de contas chamando a SECTION
           PERFORM 1000-CADASTRO.

      * FIX: Chamando a SECTION para alcancar o paragrafo FIM
           PERFORM 2000-LOGICA
              VARYING GDA-IDX FROM 1 BY 1 UNTIL
              GDA-IDX > GDA-QTDE-CONTAS.

      * Transfere o controle e os dados para o subprograma de relatorio
           CALL 'BHCS0001' USING GDA-SISTEMA-BANCARIO.

           GOBACK.

       1000-CADASTRO SECTION.
       1000-CADASTRAR-CONTAS.
           MOVE 10 TO GDA-QTDE-CONTAS.

           MOVE 10001 TO CTA-NUMERO(1)
           MOVE "JOAO" TO CTA-CLIENTE(1)
           MOVE "C" TO CTA-TIPO(1)
           MOVE 15000 TO CTA-SALDO(1)

           MOVE 10002 TO CTA-NUMERO(2)
           MOVE "MARIA" TO CTA-CLIENTE(2)
           MOVE "P" TO CTA-TIPO(2)
           MOVE 10000 TO CTA-SALDO(2)

           MOVE 10003 TO CTA-NUMERO(3)
           MOVE "PEDRO" TO CTA-CLIENTE(3)
           MOVE "C" TO CTA-TIPO(3)
           MOVE 8200 TO CTA-SALDO(3)

           MOVE 10004 TO CTA-NUMERO(4)
           MOVE "ANA" TO CTA-CLIENTE(4)
           MOVE "P" TO CTA-TIPO(4)
           MOVE 30000 TO CTA-SALDO(4)

           MOVE 10005 TO CTA-NUMERO(5)
           MOVE "LUCAS" TO CTA-CLIENTE(5)
           MOVE "C" TO CTA-TIPO(5)
           MOVE 9900 TO CTA-SALDO(5)

           MOVE 10006 TO CTA-NUMERO(6)
           MOVE "FERNANDA" TO CTA-CLIENTE(6)
           MOVE "P" TO CTA-TIPO(6)
           MOVE 4500 TO CTA-SALDO(6)

           MOVE 10007 TO CTA-NUMERO(7)
           MOVE "PAULO" TO CTA-CLIENTE(7)
           MOVE "C" TO CTA-TIPO(7)
           MOVE 12000 TO CTA-SALDO(7)

           MOVE 10008 TO CTA-NUMERO(8)
           MOVE "MARTA" TO CTA-CLIENTE(8)
           MOVE "C" TO CTA-TIPO(8)
           MOVE 10000 TO CTA-SALDO(8)

           MOVE 10009 TO CTA-NUMERO(9)
           MOVE "JULIANA" TO CTA-CLIENTE(9)
           MOVE "C" TO CTA-TIPO(9)
           MOVE 7800 TO CTA-SALDO(9)

           MOVE 10010 TO CTA-NUMERO(10)
           MOVE "ROBERTO" TO CTA-CLIENTE(10)
           MOVE "P" TO CTA-TIPO(10)
           MOVE 25000 TO CTA-SALDO(10).
       1000-FIM.
           EXIT.

       2000-LOGICA SECTION.
       2000-CLASSIFICACAO.
      * Classificacao do Status pelo Saldo
           EVALUATE TRUE
           WHEN CTA-SALDO(GDA-IDX) GREATER 10000
                MOVE "VIP" TO CTA-STATUS(GDA-IDX)
           WHEN CTA-SALDO(GDA-IDX) EQUAL 10000
                MOVE "ESPECIAL" TO CTA-STATUS(GDA-IDX)
           WHEN OTHER
                MOVE "PADRAO" TO CTA-STATUS(GDA-IDX)
           END-EVALUATE.

      * Traducao do Tipo de Conta
           EVALUATE CTA-TIPO(GDA-IDX)
           WHEN "C"
                MOVE "CORRENTE" TO CTA-TIPO-TXT(GDA-IDX)
           WHEN "P"
                MOVE "POUPANCA" TO CTA-TIPO-TXT(GDA-IDX)
           WHEN OTHER
                MOVE "IGNORADO" TO CTA-TIPO-TXT(GDA-IDX)
           END-EVALUATE.
       2000-FIM.
           EXIT.
******************************************************************