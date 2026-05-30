      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. DCOM3500.
       AUTHOR.     LEVI BARCIELA DA SILVA
      *================================================================*
      *                    C A P G E M I N I                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM3500                                    *
      *    PROGRAMADOR.:   LEVI BARCIELA DA SILVA - CAP                *
      *    ANALISTA....:   RICARDO JAMMAL         - BRADESCO           *
      *    DATA........:   10/05/2021                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ROTEADOR CICS X IMS PARA O MODULO DCOM3501  *
      *----------------------------------------------------------------*
      *    MODULOS.....:   MODULO PARA OBTER CHEQUES ATRAVES DA CUSTO  *
      *                    DIA E AS TABELAS DE CHEQUE MODIFICACAO DO   *
      *                    DCOM.                                       *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  INICIO DA WORKING DCOM3500  *'.
      *----------------------------------------------------------------*

       01 WRK-GU                       PIC  X(04)          VALUE 'GU'.
       01 WRK-ISRT                     PIC  X(04)          VALUE 'ISRT'.
       01 WRK-MODULO                   PIC  X(08)          VALUE SPACES.
       01 IND-1                        PIC  9(09) COMP-3   VALUE ZEROS.
       01 WRK-SQLCA                    PIC  X(136)         VALUE SPACES.

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE MENSAGEM ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '***  AREA DECLARACAO DE BOOK  ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-DCOM3500.
           COPY DCOMWE1C.
           COPY DCOMW350.
           COPY DCOMWE97.

       01  WRK-AREA-MENSAGEM.
           05  WRK-LL                  PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-ZZ                  PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-MENSAGEM            PIC  X(1996)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM3501 ***'.
      *----------------------------------------------------------------*

          COPY I#DCOMJ3.

      *----------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '* FIM DA WORKING DCOM3500 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           03  LNK-IO-LTERM            PIC  X(08).
           03  FILLER                  PIC  X(02).
           03  LNK-IO-STATUS           PIC  X(02).
           03  FILLER                  PIC  X(12).
           03  LNK-IO-MODNAME          PIC  X(08).

       01  LNK-ALT-PCB.
           03  LNK-ALT-LTERM           PIC  X(08).
           03  FILLER                  PIC  X(02).
           03  LNK-ALT-STATUS          PIC  X(02).
           03  FILLER                  PIC  X(20).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIO                     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE LNK-IO-PCB
                      LNK-ALT-PCB.

           ENTRY 'DLITCBL'             USING LNK-IO-PCB LNK-ALT-PCB.

           PERFORM 1000-LER-MSG-INPUT.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-LER-MSG-INPUT              SECTION.
      *----------------------------------------------------------------*

           CALL 'CBLTDLI'              USING WRK-GU
                                             LNK-IO-PCB
                                             WRK-AREA-MENSAGEM.

           IF LNK-IO-STATUS            EQUAL 'QC'
              PERFORM 9000-PROCEDIMENTOS-FINAIS
           END-IF.

           MOVE WRK-MENSAGEM           TO WRK-AREA-DCOM3500.

           PERFORM 1100-PROCESSAMENTO-PRINCIPAL.

           PERFORM 1200-FINALIZAR.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-PROCESSAMENTO-PRINCIPAL    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-CONSISTENCIA-FISICA.

           PERFORM 1560-ACESSAR-DCOM3501.

           PERFORM 1570-MOVIMENTAR-DADOS.

           MOVE 00                     TO W350-COD-RETORNO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTIR FISICAMENTE OS CAMPOS DE ENTRADA      *
      *----------------------------------------------------------------*
       1110-CONSISTENCIA-FISICA        SECTION.
      *----------------------------------------------------------------*

           IF W350-DANO-OPER-DESC      NOT NUMERIC OR
              W350-DANO-OPER-DESC      EQUAL ZEROS
              MOVE 08                  TO W350-COD-RETORNO
              MOVE '0030'              TO W350-LOCAL
              MOVE 'ANO BOOK W350 INVALIDO'
                                       TO W350-MENSAGEM
              PERFORM 1200-FINALIZAR
           END-IF.

           IF W350-NSEQ-OPER-DESC      NOT NUMERIC OR
              W350-NSEQ-OPER-DESC      EQUAL ZEROS
              MOVE 08                  TO W350-COD-RETORNO
              MOVE '0040'              TO W350-LOCAL
              MOVE 'SEQUENCIA BOOK W350 INVALIDA'
                                       TO W350-MENSAGEM
              PERFORM 1200-FINALIZAR
           END-IF.

           IF W350-NPCELA-DESC-COML NOT NUMERIC
              MOVE 08                  TO W350-COD-RETORNO
              MOVE '0050'              TO W350-LOCAL
              MOVE 'PARCELA BOOK W350 INVALIDA'
                                       TO W350-MENSAGEM
              PERFORM 1200-FINALIZAR
           END-IF.

           IF W350-QTDE-A-DESPREZAR NOT NUMERIC
              MOVE 08                  TO W350-COD-RETORNO
              MOVE '0060'              TO W350-LOCAL
              MOVE 'QTDE DESPREZAR BOOK W350 INVALIDA'
                                       TO W350-MENSAGEM
              PERFORM 1200-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AREA-DCOM3500      TO WRK-MENSAGEM

           MOVE LENGTH                 OF WRK-AREA-MENSAGEM
                                       TO WRK-LL
           MOVE ZEROS                  TO WRK-ZZ

           CALL 'CBLTDLI'              USING WRK-ISRT
                                             LNK-IO-PCB
                                             WRK-AREA-MENSAGEM.

           IF  LNK-IO-STATUS           NOT EQUAL SPACES
               PERFORM 9000-PROCEDIMENTOS-FINAIS
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1560-ACESSAR-DCOM3501           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  J3-ARGUMENTOS-ENTRADA
                       J3-RETORNO
                       ERRO-AREA
                       WRK-SQLCA.

           MOVE W350-DANO-OPER-DESC    TO J3-DANO-OPER-DESC
           MOVE W350-NSEQ-OPER-DESC    TO J3-NSEQ-OPER-DESC
           MOVE W350-NPCELA-DESC-COML  TO J3-NPCELA-DESC-COML
           MOVE W350-TIPO-OPCAO        TO J3-TIPO-OPCAO
           MOVE W350-QTDE-A-DESPREZAR  TO J3-QTDE-A-DESPREZAR
           MOVE 'DCOM3501'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING J3-ARGUMENTOS-ENTRADA
                                             J3-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  J3-COD-RETORNO          NOT EQUAL ZEROS
               MOVE J3-COD-RETORNO     TO W350-COD-RETORNO
               MOVE J3-MENSAGEM        TO W350-MENSAGEM
               MOVE '0070'             TO W350-LOCAL
               PERFORM 1200-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1560-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1570-MOVIMENTAR-DADOS           SECTION.
      *----------------------------------------------------------------*

           MOVE J3-COD-RETORNO         TO W350-COD-RETORNO
           MOVE J3-MENSAGEM            TO W350-MENSAGEM
           MOVE J3-QTDE-RETORNADA      TO W350-QTDE-RETORNADA

           PERFORM VARYING IND-1       FROM 1 BY 1
             UNTIL IND-1 GREATER 10
                OR IND-1 GREATER J3-QTDE-TOTAL
               MOVE J3RT-BANCO(IND-1)   TO W350-BANCO(IND-1)
               MOVE J3RT-AGENCIA(IND-1) TO W350-AGENCIA(IND-1)
               MOVE J3RT-CONTA(IND-1)   TO W350-CONTA(IND-1)
               MOVE J3RT-NUMERO(IND-1)  TO W350-NUMERO(IND-1)
               MOVE J3RT-VALOR(IND-1)   TO W350-VALOR(IND-1)
               MOVE J3RT-SITUACAO(IND-1)
                                      TO W350-SITUACAO(IND-1)
               MOVE J3RT-NRO-CNPJ-CPF-CHEQ(IND-1)
                                      TO W350-NRO-CNPJ-CPF-CHEQ(IND-1)
               MOVE J3RT-FLIAL-CPNJ-CPF-CHEQ(IND-1)
                                      TO W350-FLIAL-CPNJ-CPF-CHEQ(IND-1)
               MOVE J3RT-CTRL-CNPJ-CPF-CHEQ(IND-1)
                                      TO W350-CTRL-CNPJ-CPF-CHEQ(IND-1)
           END-PERFORM.

      *----------------------------------------------------------------*
       1570-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
