      *================================================================*
       IDENTIFICATION                   DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE1953.
       AUTHOR.     DOUGLAS LIRA.
      *================================================================*
      *                        P R O C W O R K                         *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : BVVE1953                                     *
      *    PROGRAMADOR  : DOUGLAS LIRA                                 *
      *    ANALISTA     : JUNIOR                                       *
      *    DATA         : SETEMBRO/2010                                *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      EMITIR RELATORIO COM AS PROPOSTAS NAO APROVADAS PELA      *
      *      VISA-VALE                                                 *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *     DDNAME                                 INCLUDE/BOOK        *
      *      PEDCTNAC(INPUT)     SAM                 I#BVVEPN          *
      *      RELATO  (OUTPUT)    SAM                                   *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.

           SELECT PEDCTNAC ASSIGN      TO PEDCTNAC
                  FILE     STATUS      IS WRK-FS-PEDCTNAC.

           SELECT RELATO   ASSIGN      TO RELATO
                  FILE     STATUS      IS WRK-FS-RELATO.

      *================================================================*
       DATA DIVISION.
      *================================================================*
       FILE SECTION.

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA  - PEDCTNAC                      *
      *            ORG. SEQUENCIAL     - LRECL = 1600                  *
      *----------------------------------------------------------------*
       FD  PEDCTNAC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-PEDCTNAC                 PIC X(1600).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE SAIDA    - RELATO                        *
      *            ORG. SEQUENCIAL     - LRECL = 133                   *
      *----------------------------------------------------------------*
       FD  RELATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-RELATO                   PIC X(133).

      *================================================================*
       WORKING-STORAGE SECTION.
      *================================================================*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  TRATAMENTO DE FILE-STATUS   *'.
      *----------------------------------------------------------------*
       01  WRK-FS-PEDCTNAC              PIC  X(02) VALUE '00'.
       01  WRK-FS-RELATO               PIC  X(02) VALUE '00'.

       01  WRK-OPERACAO                PIC  X(13) VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  CAMPOS AUXILIARES           *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           03  WRK-BATCH               PIC  X(008)       VALUE 'BATCH'.

           03  WRK-ED-LID-PEDCTNAC     PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.
           03  WRK-ED-GRA-RELATO       PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  CAMPOS AUXILIARES           *'.
      *----------------------------------------------------------------*
       01  WRK-INDEXADORES.
           03  WRK-IND                 PIC  9(03) COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  ACUMULADORES                *'.
      *----------------------------------------------------------------*
       01  WRK-ACUMULADORES.
           03  WRK-AC-LID-PEDCTNAC     PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-AC-GRA-RELATO       PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-AC-GRA-HEADER       PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-AC-GRA-DETALHE      PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-AC-GRA-TRAILLER     PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-AC-GRA-PROPOSTAS    PIC  9(09) COMP-3 VALUE ZEROS.

           03  WRK-AC-PAGINA           PIC  9(03) COMP-3 VALUE ZEROS.
           03  WRK-AC-LINHA            PIC  9(03) COMP-3 VALUE 99.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL7600 *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC 9(05) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC 9(07) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC 9(07) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP           PIC X(20)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  LINHAS DO RELATORIO         *'.
      *----------------------------------------------------------------*
       01  CABEC1.
           03  FILLER                  PIC X(01)         VALUE '1'.
           03  FILLER                  PIC X(55)         VALUE
              'BVVE1953                                         B A N '.
           03  FILLER                  PIC X(55)         VALUE
              'C O  B R A D E S C O  S / A                            '.
           03  FILLER                  PIC X(19)         VALUE
              '              PAG. '.
           03  CB1-PAGINA              PIC ZZ9           VALUE ZEROS.

       01  CABEC2.
           03  FILLER                  PIC X(01)         VALUE SPACES.
           03  CB2-DATA                PIC X(10)         VALUE SPACES.
           03  FILLER                  PIC X(55)         VALUE
              '                                      ALELO BENEFICIOS '.
           03  FILLER                  PIC X(55)         VALUE
              '- INCONSISTENCIAS                                      '.
           03  FILLER                  PIC X(04)         VALUE SPACES.
           03  CB2-HORA                PIC X(08)         VALUE SPACES.

       01  CABEC3.
           03  FILLER                  PIC X(01)         VALUE '0'.
           03  FILLER                  PIC X(56)         VALUE SPACES.
           03  CB3-TIPO-REGISTRO       PIC X(17)         VALUE SPACES.

       01  CABEC4.
           03  FILLER                  PIC X(01)         VALUE '0'.
           03  FILLER                  PIC X(55)         VALUE
              '  DT REFERENCIA   NUMERO REMESSA   INCONSISTENCIAS     '.

       01  CABEC5.
           03  FILLER                  PIC X(01)         VALUE '0'.
           03  FILLER                  PIC X(55)         VALUE
              '       CNPJ        EMPRESA                             '.
           03  FILLER                  PIC X(55)         VALUE
              '       CONTRATO  PRODUTO     LIMITE      FORMA PAGAMENT'.
           03  FILLER                  PIC X(22)         VALUE
              'O   MEIO PAGAMENTO    '.

       01  CABEC6.
           03  FILLER                  PIC X(03)         VALUE SPACES.
           03  FILLER                  PIC X(15)         VALUE
              'INCONSISTENCIAS'.

       01  LINDET1.
           03  FILLER                  PIC X(01)         VALUE SPACES.
           03  FILLER                  PIC X(03)         VALUE SPACES.
           03  LD1-DATA-REFERENCIA     PIC X(10)         VALUE SPACES.
           03  FILLER                  PIC X(09)         VALUE SPACES.
           03  LD1-NUMERO-REMESSA      PIC ZZZZZZ        VALUE ZEROS.
           03  FILLER                  PIC X(07)         VALUE SPACES.
           03  LD1-COD-INC             PIC X(04)         VALUE ZEROS.
           03  FILLER                  PIC X(03)         VALUE
              ' - '.
           03  LD1-DESC-INC            PIC X(90)         VALUE SPACES.

       01  LINDET2.
           03  FILLER                  PIC X(01)         VALUE '0'.
7C2511*    03  LD2-CNPJ                PIC 99.999.999    VALUE ZEROS.
7C2511     03  LD2-CNPJ                PIC X(08)         VALUE ZEROS.
           03  FILLER                  PIC X(01)         VALUE '/'.
7C2511*    03  LD2-FILIAL              PIC 9(04)         VALUE ZEROS.
7C2511     03  LD2-FILIAL              PIC X(04)         VALUE ZEROS.
           03  FILLER                  PIC X(01)         VALUE '-'.
           03  LD2-CONTROLE            PIC 9(02)         VALUE ZEROS.
           03  FILLER                  PIC X(01)         VALUE SPACES.
           03  LD2-EMPRESA             PIC X(40)         VALUE SPACES.
           03  FILLER                  PIC X(01)         VALUE SPACES.
           03  LD2-CONTRATO            PIC ZZZZZZZZZZZ   VALUE ZEROS.
           03  FILLER                  PIC X(03)         VALUE SPACES.
           03  LD2-COD-PRODUTO         PIC 9(03)         VALUE ZEROS.
           03  FILLER                  PIC X(01)         VALUE SPACES.
           03  LD2-VLR-LIMITE          PIC ZZZZ.ZZZ.ZZZ.ZZ9
                                                         VALUE ZEROS.
           03  FILLER                  PIC X(05)         VALUE SPACES.
           03  LD2-FORMA-PAGAMENTO     PIC X(10)         VALUE SPACES.
           03  FILLER                  PIC X(04)         VALUE SPACES.
           03  LD2-MEIO-PAGAMENTO      PIC X(17)         VALUE SPACES.

       01  LINDET3.
           03  FILLER                  PIC X(03)         VALUE SPACES.
           03  LD3-COD-INC             PIC X(04)         VALUE ZEROS.
           03  FILLER                  PIC X(03)         VALUE
              ' - '.
           03  LD3-DESC-INC            PIC X(100)        VALUE SPACES.

       01  LINTOT1.
           03  FILLER                  PIC X(01)         VALUE '-'.
           03  FILLER                  PIC X(33)         VALUE
              '  TOTAL DE INCONSISTENCIAS     : '.
           03  LT1-QTD-INC             PIC ZZZ.ZZZ.ZZ9   VALUE ZEROS.

       01  LINTOT2.
           03  FILLER                  PIC X(01)         VALUE '0'.
           03  FILLER                  PIC X(33)         VALUE
              '  TOTAL DE PROPOSTAS REJEITADAS: '.
           03  LT2-QTD-PROP-REJ        PIC ZZZ.ZZZ.ZZ9   VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  BOOK DO ARQUIVO PEDCTNAC    *'.
      *----------------------------------------------------------------*
           COPY 'I#BVVEPN'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  TABELA DE ERROS VISA VALE   *'.
      *----------------------------------------------------------------*
           COPY 'I#BVVEVE'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  BOOK DE TRATAMENTO DE ABEND *'.
      *----------------------------------------------------------------*
           COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   FIM DA WORKING BVVE1953   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR
                        UNTIL WRK-FS-PEDCTNAC EQUAL '10'

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN    INPUT  PEDCTNAC
                   OUTPUT RELATO

           MOVE    WRK-ABERTURA        TO WRK-OPERACAO
           PERFORM 1100-TESTAR-FILE-STATUS

           PERFORM 1200-OBTER-DATA-HORA

           PERFORM 1300-LER-PEDCTNAC.

       1000-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-PEDCTNAC

           PERFORM 1120-TESTAR-FS-RELATO.

       1100-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1110-TESTAR-FS-PEDCTNAC         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-PEDCTNAC          NOT EQUAL ZEROS AND '10'
              DISPLAY '****************** BVVE1953 ******************'
              DISPLAY '*                                            *'
              DISPLAY '*   ERRO ' WRK-OPERACAO
                                            ' DO ARQUIVO PEDCTNAC   *'
              DISPLAY '*   FILE STATUS: ' WRK-FS-PEDCTNAC
                                         '                          *'
              DISPLAY '*                                            *'
              DISPLAY '****************** BVVE1953 ******************'
              PERFORM 9999-CANCELA-PROGRAMA
           END-IF.

       1110-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1120-TESTAR-FS-RELATO           SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-RELATO            NOT EQUAL ZEROS
              DISPLAY '****************** BVVE1953 ******************'
              DISPLAY '*                                            *'
              DISPLAY '*   ERRO ' WRK-OPERACAO
                                            ' DO ARQUIVO RELATO     *'
              DISPLAY '*   FILE STATUS: ' WRK-FS-RELATO
                                         '                          *'
              DISPLAY '*                                            *'
              DISPLAY '****************** BVVE1953 ******************'
              PERFORM 9999-CANCELA-PROGRAMA
           END-IF.

       1120-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1200-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           CALL  'POOL7600'            USING WRK-DATA-HORA

           STRING WRK-TIMESTAMP(7:2) '/'
                  WRK-TIMESTAMP(5:2) '/'
                  WRK-TIMESTAMP(1:4)
                  DELIMITED BY SIZE    INTO CB2-DATA
           END-STRING

           STRING WRK-TIMESTAMP(9:2)  ':'
                  WRK-TIMESTAMP(11:2) ':'
                  WRK-TIMESTAMP(13:2)
                  DELIMITED BY SIZE    INTO CB2-HORA
           END-STRING.

       1200-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1300-LER-PEDCTNAC               SECTION.
      *----------------------------------------------------------------*

           READ    PEDCTNAC            INTO REG-PEDCTNAC

           MOVE    WRK-LEITURA         TO WRK-OPERACAO
           PERFORM 1110-TESTAR-FS-PEDCTNAC

           IF WRK-FS-PEDCTNAC          EQUAL '00'
              ADD     1                TO WRK-AC-LID-PEDCTNAC
           END-IF.

       1300-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-EMITIR-RELATORIO

           PERFORM 1300-LER-PEDCTNAC.

       2000-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2100-EMITIR-RELATORIO           SECTION.
      *----------------------------------------------------------------*

           IF WRK-AC-LINHA             GREATER 60
              PERFORM 2110-GRAVAR-CABECALHO
           END-IF.

           PERFORM 2120-GRAVAR-DETALHE.

       2100-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2110-GRAVAR-CABECALHO           SECTION.
      *----------------------------------------------------------------*

           ADD     1                   TO WRK-AC-PAGINA
           MOVE    WRK-AC-PAGINA       TO CB1-PAGINA

           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO

           WRITE   FD-RELATO           FROM CABEC1
           PERFORM 1120-TESTAR-FS-RELATO

           WRITE   FD-RELATO           FROM CABEC2
           PERFORM 1120-TESTAR-FS-RELATO

           EVALUATE PEDCTNAC-TIPO-REG
               WHEN 1
                    MOVE   ' REGISTRO HEADER'
                                       TO CB3-TIPO-REGISTRO

               WHEN 2
                    MOVE   'REGISTROS DETALHE'
                                       TO CB3-TIPO-REGISTRO

               WHEN 3
                    MOVE   'REGISTRO TRAILLER'
                                       TO CB3-TIPO-REGISTRO
           END-EVALUATE

           WRITE   FD-RELATO           FROM CABEC3
           PERFORM 1120-TESTAR-FS-RELATO

           MOVE    4                   TO WRK-AC-LINHA

           EVALUATE PEDCTNAC-TIPO-REG
               WHEN 1
                    WRITE   FD-RELATO  FROM CABEC4
                    PERFORM 1120-TESTAR-FS-RELATO

                    ADD     2          TO WRK-AC-LINHA

               WHEN 2
                    WRITE   FD-RELATO  FROM CABEC5
                    PERFORM 1120-TESTAR-FS-RELATO

                    WRITE   FD-RELATO  FROM CABEC6
                    PERFORM 1120-TESTAR-FS-RELATO

                    ADD     3          TO WRK-AC-LINHA

               WHEN 3
                    WRITE   FD-RELATO  FROM CABEC6
                    PERFORM 1120-TESTAR-FS-RELATO

                    ADD     2          TO WRK-AC-LINHA
           END-EVALUATE.

       2110-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2120-GRAVAR-DETALHE             SECTION.
      *----------------------------------------------------------------*

           EVALUATE PEDCTNAC-TIPO-REG
               WHEN 1
                    PERFORM 2130-GRAVAR-DETALHE-H0
                    PERFORM 2200-GRAVAR-TOTAL-H0

               WHEN 2
                    PERFORM 2140-GRAVAR-DETALHE-D1

               WHEN 3
                    PERFORM 2150-GRAVAR-DETALHE-T0
                    PERFORM 2220-GRAVAR-TOTAL-T0
           END-EVALUATE.

       2120-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2130-GRAVAR-DETALHE-H0          SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO

           STRING PEDCTNAC-DATA-REFERENCIA(1:2) '/'
                  PEDCTNAC-DATA-REFERENCIA(3:2) '/'
                  PEDCTNAC-DATA-REFERENCIA(5:4)
                  DELIMITED BY SIZE    INTO LD1-DATA-REFERENCIA
           END-STRING

           MOVE    PEDCTNAC-NUM-REMESSA
                                       TO LD1-NUMERO-REMESSA

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                   UNTIL   WRK-IND     GREATER PEDCTNAC-QTD-ERROS

                IF WRK-IND             GREATER 1
                   MOVE SPACES         TO LD1-DATA-REFERENCIA
                   MOVE ZEROS          TO LD1-NUMERO-REMESSA
                END-IF

                MOVE PEDCTNAC-TAB-COD-ERRO(WRK-IND)
                                       TO LD1-COD-INC

                SEARCH ALL TAB-ERROS-VV
                       AT END
                          MOVE SPACES  TO LD1-DESC-INC

                       WHEN TAB-COD-ERRO-VV (IND-ERR-VV)
                                       EQUAL PEDCTNAC-TAB-COD-ERRO
                                            (WRK-IND)
                          MOVE TAB-DESC-ERRO-VV(IND-ERR-VV)
                                       TO LD1-DESC-INC
                END-SEARCH

                IF WRK-AC-LINHA        GREATER 60
                   PERFORM 2110-GRAVAR-CABECALHO
                END-IF

                WRITE   FD-RELATO      FROM LINDET1
                PERFORM 1120-TESTAR-FS-RELATO

                ADD     1              TO WRK-AC-LINHA
           END-PERFORM

           MOVE    70                  TO WRK-AC-LINHA
           ADD     1                   TO WRK-AC-GRA-HEADER.

       2130-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2140-GRAVAR-DETALHE-D1          SECTION.
      *----------------------------------------------------------------*

           MOVE    PEDCTNAC-CNPJ       TO LD2-CNPJ
           MOVE    PEDCTNAC-FILIAL     TO LD2-FILIAL
           MOVE    PEDCTNAC-CONTROLE   TO LD2-CONTROLE
           MOVE    PEDCTNAC-RAZAO-SOCIAL
                                       TO LD2-EMPRESA
           MOVE    PEDCTNAC-NUM-CONTRATO
                                       TO LD2-CONTRATO
           MOVE    PEDCTNAC-COD-PRODUTO
                                       TO LD2-COD-PRODUTO
           MOVE    PEDCTNAC-VLR-LIMITE
                                       TO LD2-VLR-LIMITE

           IF PEDCTNAC-FORMA-PGTO      EQUAL 1
              MOVE 'ANTECIPADO'        TO LD2-FORMA-PAGAMENTO

           ELSE
              MOVE SPACES              TO LD2-FORMA-PAGAMENTO
           END-IF

           EVALUATE PEDCTNAC-MEIO-PGTO
               WHEN 1
                    MOVE 'CREDITO EM CONTA'
                                       TO LD2-MEIO-PAGAMENTO
               WHEN 2
                    MOVE 'DEBITO EM CONTA'
                                       TO LD2-MEIO-PAGAMENTO
               WHEN 3
                    MOVE 'COBRANCA BANCARIA'
                                       TO LD2-MEIO-PAGAMENTO
               WHEN OTHER
                    MOVE SPACES        TO LD2-MEIO-PAGAMENTO
           END-EVALUATE

           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO

           WRITE   FD-RELATO           FROM LINDET2
           PERFORM 1120-TESTAR-FS-RELATO

           ADD     1                   TO WRK-AC-LINHA
                                          WRK-AC-GRA-PROPOSTAS
                                          WRK-AC-GRA-RELATO

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                   UNTIL   WRK-IND     GREATER PEDCTNAC-QTD-ERROS

                MOVE PEDCTNAC-TAB-COD-ERRO(WRK-IND)
                                       TO LD3-COD-INC

                SEARCH ALL TAB-ERROS-VV
                       AT  END
                           MOVE SPACES TO LD3-DESC-INC

                       WHEN TAB-COD-ERRO-VV (IND-ERR-VV)
                                       EQUAL PEDCTNAC-TAB-COD-ERRO
                                            (WRK-IND)
                          MOVE TAB-DESC-ERRO-VV(IND-ERR-VV)
                                       TO LD3-DESC-INC
                END-SEARCH

                IF WRK-AC-LINHA        GREATER 60
                   PERFORM 2110-GRAVAR-CABECALHO
                END-IF

                WRITE   FD-RELATO      FROM LINDET3
                PERFORM 1120-TESTAR-FS-RELATO

                ADD     1              TO WRK-AC-LINHA
                                          WRK-AC-GRA-DETALHE
                                          WRK-AC-GRA-RELATO
           END-PERFORM.

       2140-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2150-GRAVAR-DETALHE-T0          SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO

           IF WRK-AC-GRA-DETALHE       GREATER ZEROS
              PERFORM 2210-GRAVAR-TOTAL-D1
           END-IF

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                   UNTIL   WRK-IND     GREATER PEDCTNAC-QTD-ERROS

                MOVE PEDCTNAC-TAB-COD-ERRO(WRK-IND)
                                       TO LD3-COD-INC

                SEARCH ALL TAB-ERROS-VV
                       AT  END
                           MOVE SPACES TO LD3-DESC-INC

                       WHEN TAB-COD-ERRO-VV(IND-ERR-VV)
                                       EQUAL PEDCTNAC-TAB-COD-ERRO
                                            (WRK-IND)
                          MOVE TAB-DESC-ERRO-VV(IND-ERR-VV)
                                       TO LD3-DESC-INC
                END-SEARCH

                IF WRK-AC-LINHA        GREATER 60
                   PERFORM 2110-GRAVAR-CABECALHO
                END-IF

                WRITE   FD-RELATO      FROM LINDET3
                PERFORM 1120-TESTAR-FS-RELATO

                ADD     1              TO WRK-AC-LINHA
                                          WRK-AC-GRA-RELATO
           END-PERFORM

           ADD     1                   TO WRK-AC-GRA-TRAILLER.

       2150-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2200-GRAVAR-TOTAL-H0            SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-AC-GRA-HEADER   TO LT1-QTD-INC

           WRITE   FD-RELATO           FROM LINTOT1
           PERFORM 1120-TESTAR-FS-RELATO.

       2200-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2210-GRAVAR-TOTAL-D1            SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-AC-GRA-DETALHE  TO LT1-QTD-INC

           WRITE   FD-RELATO           FROM LINTOT1
           PERFORM 1120-TESTAR-FS-RELATO

           MOVE    WRK-AC-GRA-PROPOSTAS
                                       TO LT2-QTD-PROP-REJ

           WRITE   FD-RELATO           FROM LINTOT2
           PERFORM 1120-TESTAR-FS-RELATO.

       2210-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2220-GRAVAR-TOTAL-T0            SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-AC-GRA-TRAILLER TO LT1-QTD-INC

           WRITE   FD-RELATO           FROM LINTOT1
           PERFORM 1120-TESTAR-FS-RELATO.

       2220-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 9010-DISPLAY-TOTAIS

           IF WRK-AC-GRA-DETALHE       GREATER ZEROS AND
              WRK-AC-GRA-TRAILLER      EQUAL ZEROS
              PERFORM 2210-GRAVAR-TOTAL-D1
           END-IF

           CLOSE   PEDCTNAC
                   RELATO

           MOVE    WRK-FECHAMENTO      TO WRK-OPERACAO
           PERFORM 1100-TESTAR-FILE-STATUS

           STOP    RUN.

       9000-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       9010-DISPLAY-TOTAIS             SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-AC-LID-PEDCTNAC TO WRK-ED-LID-PEDCTNAC
           MOVE    WRK-AC-GRA-RELATO   TO WRK-ED-GRA-RELATO

           DISPLAY '******************** BVVE1953 ********************'
           DISPLAY '*                                                *'
           DISPLAY '* TOTAL DE REGISTROS LIDOS PEDCTNAC: '
                      WRK-ED-LID-PEDCTNAC                          ' *'
           DISPLAY '* TOTAL DE REGISTROS IMPRESSOS     : '
                      WRK-ED-GRA-RELATO                            ' *'
           DISPLAY '*                                                *'
           DISPLAY '******************** BVVE1953 ********************'.

       9010-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       9999-CANCELA-PROGRAMA           SECTION.
      *----------------------------------------------------------------*

           MOVE   'BVVE1953'           TO ERR-PGM
           MOVE   'APL'                TO ERR-TIPO-ACESSO
           MOVE   'PROGRAMA CANCELADO' TO ERR-TEXTO

           CALL   'BRAD7100'           USING WRK-BATCH
                                             ERRO-AREA

           GOBACK.

       9999-99-FIM.                    EXIT.
