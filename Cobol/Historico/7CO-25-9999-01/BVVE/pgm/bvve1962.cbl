      *================================================================*
       IDENTIFICATION                   DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE1962.
       AUTHOR.     DOUGLAS LIRA.
      *================================================================*
      *                        P R O C W O R K                         *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : BVVE1962                                     *
      *    PROGRAMADOR  : DOUGLAS LIRA                                 *
      *    ANALISTA     : JUNIOR                                       *
      *    DATA         : SETEMBRO/2010                                *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      A PARTIR DO RETORNO DOS PEDIDOS DE CARTAO ENVIADOS A      *
      *      VISA-VALE, ATUALIZAR O ARQUIVO VSAM VVPEDICT E GERAR      *
      *      ARQUIVOS CONTENDO AS PROPOSTAS ACATADAS E NAO ACATADAS    *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *     DDNAME                                 INCLUDE/BOOK        *
      *      RETPEDCT(INPUT)     SAM                 I#BVVEVV          *
      *      PEDCTAC (OUTPUT)    SAM                 I#BVVEPA          *
      *      PEDCTNAC(OUTPUT)    SAM                 I#VBVVPN          *
      *      VVPEDICT(I-O)      VSAM                 I#BVVEPC          *
      *                                                                *
4SYSFB*================================================================*
.     *                       A L T E R A C A O                        *
.     *================================================================*
.     *    PROGRAMADOR.: FABRICA            - FOURSYS                  *
.     *    DATA........: 14/09/2018                                    *
.     *----------------------------------------------------------------*
.     *    PROJETO.....: PROPOSTA ELETRONICA FASE II PAGAMENTO A PRAZO *
.     *    OBJETIVO....: GRAVAR O CONTRATO                             *
4SYSFB*----------------------------------------------------------------*
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.

           SELECT RETPEDCT ASSIGN      TO RETPEDCT
                  FILE     STATUS      IS WRK-FS-RETPEDCT.

           SELECT PEDCTAC  ASSIGN      TO PEDCTAC
                  FILE     STATUS      IS WRK-FS-PEDCTAC.

           SELECT PEDCTNAC ASSIGN      TO PEDCTNAC
                  FILE     STATUS      IS WRK-FS-PEDCTNAC.

           SELECT VVPEDICT  ASSIGN     TO VVPEDICT
                  ORGANIZATION         IS INDEXED
                  ACCESS               IS RANDOM
                  RECORD   KEY         IS CADA-PDCT-CH
                  FILE     STATUS      IS WRK-FS-VVPEDICT.

4SYSFB     SELECT VVCONTRT  ASSIGN     TO VVCONTRT
.                 ORGANIZATION         IS INDEXED
.                 ACCESS               IS RANDOM
.                 RECORD   KEY         IS CONTRATO-CH
4SYSFB            FILE     STATUS      IS WRK-FS-VVCONTRT.

      *================================================================*
       DATA DIVISION.
      *================================================================*
       FILE SECTION.

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA  - RETPEDCT                      *
      *            ORG. SEQUENCIAL     - LRECL = 350                   *
      *----------------------------------------------------------------*
       FD  RETPEDCT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-RETPEDCT                 PIC X(350).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE SAIDA    - PEDCTAC                       *
      *            ORG. SEQUENCIAL     - LRECL = 200                   *
      *----------------------------------------------------------------*
       FD  PEDCTAC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-PEDCTAC                  PIC X(200).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE SAIDA    - PEDCTNAC                      *
      *            ORG. SEQUENCIAL     - LRECL = 1600                  *
      *----------------------------------------------------------------*
       FD  PEDCTNAC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-PEDCTNAC                 PIC X(1600).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE ENT/SAI  - VVPEDICT                      *
      *            ORG. INDEXADA       - LRECL = 1500                  *
      *----------------------------------------------------------------*
       FD  VVPEDICT.
           COPY 'I#BVVEPC'.

4SYSFB*----------------------------------------------------------------*
.     *    OUTPUT: ARQUIVO SAIDA       - VVCONTRT                      *
.     *            ORG. INDEXADA       - LRECL = 1500                  *
.     *----------------------------------------------------------------*
.      FD  VVCONTRT.
.          COPY 'I#BVVECO'.
.
4SYSFB*================================================================*
       WORKING-STORAGE SECTION.
      *================================================================*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  TRATAMENTO DE FILE-STATUS   *'.
      *----------------------------------------------------------------*
       01  WRK-FS-RETPEDCT             PIC  X(02) VALUE '00'.
       01  WRK-FS-PEDCTAC              PIC  X(02) VALUE '00'.
       01  WRK-FS-PEDCTNAC             PIC  X(02) VALUE '00'.
       01  WRK-FS-VVPEDICT             PIC  X(02) VALUE '00'.
4SYSFB 01  WRK-FS-VVCONTRT             PIC  X(02) VALUE '00'.

       01  WRK-OPERACAO                PIC  X(13) VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.
       77  WRK-REGRAVACAO              PIC  X(13) VALUE 'NA REGRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  CAMPOS AUXILIARES           *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           03  WRK-BATCH               PIC  X(08)        VALUE 'BATCH'.
           03  WRK-ERRO-HEADER         PIC  X(01)        VALUE 'N'.

           03  WRK-ED-LID-RETPEDCT     PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.
           03  WRK-ED-GRA-PEDCTAC      PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.
           03  WRK-ED-GRA-PEDCTNAC     PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.
           03  WRK-ED-ATU-VVPEDICT     PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.
           03  WRK-ED-REG-AC           PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.
           03  WRK-ED-REG-NAC          PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.
4SYSFB     03  WRK-ED-GRA-VVCONTRT     PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  INDEXADORES                 *'.
      *----------------------------------------------------------------*
       01  WRK-INDEXADORES.
           03  WRK-IND                 PIC  9(03) COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  ACUMULADORES                *'.
      *----------------------------------------------------------------*
       01  WRK-ACUMULADORES.
           03  WRK-AC-LID-RETPEDCT     PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-AC-GRA-PEDCTAC      PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-AC-GRA-PEDCTNAC     PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-AC-ATU-VVPEDICT     PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-AC-REG-AC           PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-AC-REG-NAC          PIC  9(09) COMP-3 VALUE ZEROS.
4SYSFB     03  WRK-AC-GRA-VVCONTRT     PIC  9(09) COMP-3 VALUE ZEROS.
4SYSFB     03  WRK-ACU-INCONSISTENTE   PIC  9(09) COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  BOOK DA TABELA DE ERROS     *'.
      *----------------------------------------------------------------*
           COPY 'I#BVVETE'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  BOOK DO ARQUIVO RETPEDCT    *'.
      *----------------------------------------------------------------*
           COPY 'I#BVVEVV'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  BOOK DO ARQUIVO PEDCTAC     *'.
      *----------------------------------------------------------------*
           COPY 'I#BVVEPA'.

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
           '*   FIM DA WORKING BVVE1962   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR
                        UNTIL WRK-FS-RETPEDCT EQUAL '10'

           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN    INPUT  RETPEDCT
                   OUTPUT PEDCTAC
                          PEDCTNAC
4SYSFB             I-O    VVCONTRT
                          VVPEDICT


           MOVE    WRK-ABERTURA        TO WRK-OPERACAO
           PERFORM 1100-TESTAR-FILE-STATUS

           PERFORM 1200-LER-RETPEDCT

           IF ENV-TIPO-H0              NOT EQUAL 'H0'
              DISPLAY '**************** BVVE1962 ****************'
              DISPLAY '*                                        *'
              DISPLAY '*   ARQUIVO RETPEDCT NAO POSSUI HEADER   *'
              DISPLAY '*        PROCESSAMENTO CANCELADO         *'
              DISPLAY '*                                        *'
              DISPLAY '**************** BVVE1962 ****************'
              PERFORM 9999-CANCELA-PROGRAMA
           END-IF.

       1000-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-RETPEDCT

           PERFORM 1120-TESTAR-FS-PEDCTAC

           PERFORM 1130-TESTAR-FS-PEDCTNAC

           PERFORM 1140-TESTAR-FS-VVPEDICT

4SYSFB     PERFORM 1150-TESTAR-FS-VVCONTRT.

       1100-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1110-TESTAR-FS-RETPEDCT         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-RETPEDCT          NOT EQUAL ZEROS AND '10'
              DISPLAY '****************** BVVE1962 ******************'
              DISPLAY '*                                            *'
              DISPLAY '*   ERRO ' WRK-OPERACAO
                                            ' DO ARQUIVO RETPEDCT   *'
              DISPLAY '*   FILE STATUS: ' WRK-FS-RETPEDCT
                                         '                          *'
              DISPLAY '*                                            *'
              DISPLAY '****************** BVVE1962 ******************'
              PERFORM 9999-CANCELA-PROGRAMA
           END-IF.

       1110-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1120-TESTAR-FS-PEDCTAC          SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-PEDCTAC           NOT EQUAL ZEROS
              DISPLAY '****************** BVVE1962 ******************'
              DISPLAY '*                                            *'
              DISPLAY '*   ERRO ' WRK-OPERACAO
                                            ' DO ARQUIVO PEDCTAC    *'
              DISPLAY '*   FILE STATUS: ' WRK-FS-PEDCTAC
                                         '                          *'
              DISPLAY '*                                            *'
              DISPLAY '****************** BVVE1962 ******************'
              PERFORM 9999-CANCELA-PROGRAMA
           END-IF.

       1120-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1130-TESTAR-FS-PEDCTNAC         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-PEDCTNAC          NOT EQUAL ZEROS
              DISPLAY '****************** BVVE1962 ******************'
              DISPLAY '*                                            *'
              DISPLAY '*   ERRO ' WRK-OPERACAO
                                            ' DO ARQUIVO PEDCTNAC   *'
              DISPLAY '*   FILE STATUS: ' WRK-FS-PEDCTNAC
                                         '                          *'
              DISPLAY '*                                            *'
              DISPLAY '****************** BVVE1962 ******************'
              PERFORM 9999-CANCELA-PROGRAMA
           END-IF.

       1130-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       1140-TESTAR-FS-VVPEDICT         SECTION.
      *----------------------------------------------------------------*

170614     IF WRK-FS-VVPEDICT          NOT EQUAL ZEROS AND '23'
              DISPLAY '****************** BVVE1962 ******************'
              DISPLAY '*                                            *'
              DISPLAY '*   ERRO ' WRK-OPERACAO
                                            ' DO ARQUIVO VVPEDICT   *'
              DISPLAY '*   FILE STATUS: ' WRK-FS-VVPEDICT
                                         '                          *'
              DISPLAY '*                                            *'
              DISPLAY '****************** BVVE1962 ******************'
              PERFORM 9999-CANCELA-PROGRAMA
           END-IF.

       1140-99-FIM.                    EXIT.

4SYSFB*----------------------------------------------------------------*
.      1150-TESTAR-FS-VVCONTRT         SECTION.
.     *----------------------------------------------------------------*
.
.          IF WRK-FS-VVCONTRT          NOT EQUAL ZEROS AND '23'
.             DISPLAY '****************** BVVE1962 ******************'
.             DISPLAY '*                                            *'
.             DISPLAY '*   ERRO ' WRK-OPERACAO
.                                           ' DO ARQUIVO VVCONTRT   *'
.             DISPLAY '*   FILE STATUS: ' WRK-FS-VVCONTRT
.                                        '                          *'
.             DISPLAY '*                                            *'
.             DISPLAY '****************** BVVE1962 ******************'
.             PERFORM 9999-CANCELA-PROGRAMA
.          ELSE
.             IF WRK-FS-VVCONTRT       EQUAL '14'
.                ADD 1                 TO WRK-ACU-INCONSISTENTE
.             END-IF
.          END-IF.
.
.     *----------------------------------------------------------------*
.      1150-99-FIM.                    EXIT.
4SYSFB*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-LER-RETPEDCT               SECTION.
      *----------------------------------------------------------------*

           READ    RETPEDCT          INTO ENVIO-PEDIDO-CARTAO-H0

           MOVE    WRK-LEITURA         TO WRK-OPERACAO
           PERFORM 1110-TESTAR-FS-RETPEDCT

           IF WRK-FS-RETPEDCT          EQUAL '00'
              ADD     1                TO WRK-AC-LID-RETPEDCT
           END-IF.

       1200-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE ENV-TIPO-H0
               WHEN 'H0'
                     PERFORM 2010-PROCESSAR-H0

               WHEN 'D1'
                     PERFORM 2020-PROCESSAR-D1

               WHEN 'D2'
                     PERFORM 2030-PROCESSAR-D2

               WHEN 'D3'
                     PERFORM 2040-PROCESSAR-D3

               WHEN 'D4'
                     PERFORM 2050-PROCESSAR-D4

               WHEN 'D5'
                     PERFORM 2060-PROCESSAR-D5

               WHEN 'T1'
                     PERFORM 2070-PROCESSAR-T1
           END-EVALUATE

           PERFORM 1200-LER-RETPEDCT.

       2000-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2010-PROCESSAR-H0               SECTION.
      *----------------------------------------------------------------*

      *    SE HOUVER ERRO NO REGISTRO HEADER DESPREZA TODOS OS REGISTROS
      *    DETALHE E CONSISTE TRAILLER

           IF ENV-COD-RETORNO-H0       NOT EQUAL SPACES AND LOW-VALUES
                                       AND '0000;'
              INITIALIZE               REG-PEDCTNAC
                                       WRK-TABELA-ERROS

              MOVE   'S'               TO WRK-ERRO-HEADER

              MOVE    1                TO PEDCTNAC-TIPO-REG
              MOVE    ENV-DT-REF-INTERFACE-H0
                                       TO PEDCTNAC-DATA-REFERENCIA
              MOVE    ENV-NUM-REMESSA-DIA-H0
                                       TO PEDCTNAC-NUM-REMESSA
              MOVE    ENV-COD-RETORNO-H0
                                       TO WRK-TABELA-ERROS

              PERFORM 2080-MOVER-ERROS

              PERFORM 2090-GRAVAR-PEDCTNAC

              PERFORM 1200-LER-RETPEDCT

              PERFORM UNTIL
                      ENV-TIPO-H0      EQUAL 'T1' OR
                      WRK-FS-RETPEDCT  EQUAL '10'

                      ADD 1            TO WRK-AC-REG-NAC

                      IF ENV-TIPO-H0   EQUAL 'D1' AND
                         ENV-SUBTIPO-H0
                                       EQUAL 1
                         ADD 1         TO WRK-AC-GRA-PEDCTNAC
                      END-IF

                      PERFORM 1200-LER-RETPEDCT
              END-PERFORM

              IF WRK-FS-RETPEDCT  EQUAL '10'
                 DISPLAY '**************** BVVE1962 ******************'
                 DISPLAY '*                                          *'
                 DISPLAY '*   ARQUIVO RETPEDCT NAO POSSUI TRAILLER   *'
                 DISPLAY '*        PROCESSAMENTO CANCELADO           *'
                 DISPLAY '*                                          *'
                 DISPLAY '**************** BVVE1962 ******************'
                 PERFORM 9999-CANCELA-PROGRAMA

              ELSE
                 PERFORM 2070-PROCESSAR-T1
              END-IF

           ELSE
              ADD     1                TO WRK-AC-REG-AC
           END-IF.

       2010-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2020-PROCESSAR-D1               SECTION.
      *----------------------------------------------------------------*

           IF WRK-AC-LID-RETPEDCT      GREATER 2 AND
              ENV-SUBTIPO-H0           EQUAL 1

              IF PEDCTNAC-TABELA-ERROS NOT EQUAL SPACES AND LOW-VALUES
                 PERFORM 2090-GRAVAR-PEDCTNAC
              ELSE
                 PERFORM 2100-GRAVAR-PEDCTAC
              END-IF

              PERFORM 2110-LER-VVPEDICT

              IF WRK-FS-VVPEDICT       EQUAL ZEROS
                 PERFORM 2120-ATUALIZAR-VVPEDICT
              END-IF

              INITIALIZE               REG-PEDCTAC
                                       REG-PEDCTNAC
           END-IF

           IF ENV-SUBTIPO-H0           EQUAL 1
              INITIALIZE               WRK-TABELA-ERROS

              MOVE ENV-CNPJ-D1         TO PEDCTAC-CNPJ
              MOVE ENV-FILIAL-D1       TO PEDCTAC-FILIAL
              MOVE ENV-CONTROLE-D1     TO PEDCTAC-CONTROLE
              MOVE ENV-RAZAO-SOCIAL-D1 TO PEDCTAC-RAZAO-SOCIAL

              IF ENV-COD-ERROS-D1      NOT EQUAL SPACES AND LOW-VALUES
                                       AND '0000;'
                 MOVE    2             TO PEDCTNAC-TIPO-REG
                 MOVE    ENV-COD-ERROS-D1
                                       TO WRK-TABELA-ERROS
                 PERFORM 2080-MOVER-ERROS

              ELSE
                 ADD     1             TO WRK-AC-REG-AC
              END-IF

           ELSE
              MOVE ENV-LIMITE-CREDITO-D1-2
                                       TO PEDCTAC-VLR-LIMITE

              IF ENV-COD-ERROS-D1-2    NOT EQUAL SPACES AND LOW-VALUES
                                       AND '0000;'
                 MOVE    2             TO PEDCTNAC-TIPO-REG
                 MOVE    ENV-COD-ERROS-D1-2
                                       TO WRK-TABELA-ERROS
                 PERFORM 2080-MOVER-ERROS

              ELSE
                 ADD     1             TO WRK-AC-REG-AC
              END-IF
           END-IF.

       2020-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2030-PROCESSAR-D2               SECTION.
      *----------------------------------------------------------------*

           IF ENV-COD-ERROS-D2         NOT EQUAL SPACES AND LOW-VALUES
                                       AND '0000;'
              INITIALIZE               WRK-TABELA-ERROS

              MOVE    2                TO PEDCTNAC-TIPO-REG
              MOVE    ENV-COD-ERROS-D2 TO WRK-TABELA-ERROS

              PERFORM 2080-MOVER-ERROS

           ELSE
              ADD     1                TO WRK-AC-REG-AC
           END-IF.

       2030-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2040-PROCESSAR-D3               SECTION.
      *----------------------------------------------------------------*

           IF ENV-SUBTIPO-H0           EQUAL 1
              MOVE ENV-NUM-CONTRATO-D3 TO PEDCTAC-NUM-CONTRATO

              INITIALIZE               WRK-TABELA-ERROS

              IF ENV-COD-ERROS-D3      NOT EQUAL SPACES AND LOW-VALUES
                                       AND '0000;'
                 MOVE    2             TO PEDCTNAC-TIPO-REG
                 MOVE    ENV-COD-ERROS-D3
                                       TO WRK-TABELA-ERROS
                 PERFORM 2080-MOVER-ERROS
              END-IF

           ELSE
              IF ENV-COD-ERROS-D3-2    NOT EQUAL SPACES AND LOW-VALUES
                                       AND '0000;'
                 MOVE    2             TO PEDCTNAC-TIPO-REG
                 MOVE    ENV-COD-ERROS-D3-2
                                       TO WRK-TABELA-ERROS
                 PERFORM 2080-MOVER-ERROS
              END-IF
           END-IF.

       2040-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2050-PROCESSAR-D4               SECTION.
      *----------------------------------------------------------------*

           IF ENV-SUBTIPO-H0           EQUAL 1
              INITIALIZE               WRK-TABELA-ERROS

              MOVE ENV-NUM-CONTRATO-D4 TO PEDCTAC-NUM-CONTRATO
              MOVE ENV-COD-PRODUTO-D4  TO PEDCTAC-COD-PRODUTO
              MOVE ENV-FORMA-PGTO-D4   TO PEDCTAC-FORMA-PGTO
              MOVE ENV-MEIO-PGTO-D4    TO PEDCTAC-MEIO-PGTO

              IF ENV-COD-ERROS-D4      NOT EQUAL SPACES AND LOW-VALUES
                                       AND '0000;'
                 MOVE    2             TO PEDCTNAC-TIPO-REG
                 MOVE    ENV-COD-ERROS-D4
                                       TO WRK-TABELA-ERROS
                 PERFORM 2080-MOVER-ERROS

              ELSE
                 ADD     1             TO WRK-AC-REG-AC
              END-IF

           ELSE
              IF ENV-COD-ERROS-D4-2    NOT EQUAL SPACES AND LOW-VALUES
                                       AND '0000;'
                 MOVE    2             TO PEDCTNAC-TIPO-REG
                 MOVE    ENV-COD-ERROS-D4-2
                                       TO WRK-TABELA-ERROS
                 PERFORM 2080-MOVER-ERROS

              ELSE
                 ADD     1             TO WRK-AC-REG-AC
              END-IF
           END-IF.

       2050-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2060-PROCESSAR-D5               SECTION.
      *----------------------------------------------------------------*

           IF ENV-COD-ERROS-D5         NOT EQUAL SPACES AND LOW-VALUES
                                       AND '0000;'
              INITIALIZE               WRK-TABELA-ERROS

              MOVE    2                TO PEDCTNAC-TIPO-REG
              MOVE    ENV-COD-ERROS-D5 TO WRK-TABELA-ERROS

              PERFORM 2080-MOVER-ERROS

           ELSE
              ADD     1                TO WRK-AC-REG-AC
           END-IF.

       2060-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2070-PROCESSAR-T1               SECTION.
      *----------------------------------------------------------------*

           IF WRK-ERRO-HEADER          EQUAL  'N' AND
              WRK-AC-LID-RETPEDCT      GREATER 2

              IF PEDCTNAC-TABELA-ERROS NOT EQUAL SPACES AND LOW-VALUES
                 PERFORM 2090-GRAVAR-PEDCTNAC
              ELSE
                 PERFORM 2100-GRAVAR-PEDCTAC
              END-IF

              PERFORM 2110-LER-VVPEDICT

              IF WRK-FS-VVPEDICT       EQUAL ZEROS
                 PERFORM 2120-ATUALIZAR-VVPEDICT
              END-IF

           END-IF

           IF ENV-COD-ERROS-T1         NOT EQUAL SPACES AND LOW-VALUES
                                       AND '0000;'
              INITIALIZE               REG-PEDCTNAC
                                       WRK-TABELA-ERROS

              MOVE    3                TO PEDCTNAC-TIPO-REG
              MOVE    ENV-COD-ERROS-T1 TO WRK-TABELA-ERROS

              PERFORM 2080-MOVER-ERROS
              PERFORM 2090-GRAVAR-PEDCTNAC

           ELSE
              ADD     1                TO WRK-AC-REG-AC
           END-IF.

       2070-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2080-MOVER-ERROS                SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                   UNTIL   WRK-IND     GREATER  10 OR
                           WRK-TAB-COD-ERRO(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
              ADD  1                   TO PEDCTNAC-QTD-ERROS
              MOVE WRK-TAB-COD-ERRO(WRK-IND)
                                       TO PEDCTNAC-TAB-COD-ERRO
                                         (PEDCTNAC-QTD-ERROS)
           END-PERFORM

           ADD     1                   TO WRK-AC-REG-NAC.

       2080-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2090-GRAVAR-PEDCTNAC            SECTION.
      *----------------------------------------------------------------*

           MOVE    PEDCTAC-CNPJ        TO PEDCTNAC-CNPJ
           MOVE    PEDCTAC-FILIAL      TO PEDCTNAC-FILIAL
           MOVE    PEDCTAC-CONTROLE    TO PEDCTNAC-CONTROLE
           MOVE    PEDCTAC-RAZAO-SOCIAL
                                       TO PEDCTNAC-RAZAO-SOCIAL
           MOVE    PEDCTAC-NUM-CONTRATO
                                       TO PEDCTNAC-NUM-CONTRATO
           MOVE    PEDCTAC-VLR-LIMITE  TO PEDCTNAC-VLR-LIMITE
           MOVE    PEDCTAC-COD-PRODUTO TO PEDCTNAC-COD-PRODUTO
           MOVE    PEDCTAC-FORMA-PGTO  TO PEDCTNAC-FORMA-PGTO
           MOVE    PEDCTAC-MEIO-PGTO   TO PEDCTNAC-MEIO-PGTO


4SYSFB     MOVE CADA-FUNCIONARIO       TO PEDCTNAC-USUARIO
4SYSFB     MOVE CADA-DT-INICIO-VL-CRED TO PEDCTNAC-DT-INCLUSAO
4SYSFB     MOVE CADA-AGENCIA-VENDEU    TO PEDCTNAC-AGENCIA

           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO

           WRITE   FD-PEDCTNAC         FROM REG-PEDCTNAC
           PERFORM 1130-TESTAR-FS-PEDCTNAC

           IF PEDCTNAC-TIPO-REG        EQUAL 2
              ADD     1                TO WRK-AC-GRA-PEDCTNAC
           END-IF.

       2090-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2100-GRAVAR-PEDCTAC             SECTION.
      *----------------------------------------------------------------*

4SYSFB     MOVE CADA-FUNCIONARIO       TO PEDCTAC-FUNCIONARIO
4SYSFB     MOVE CADA-AGENCIA-VENDEU    TO PEDCTAC-AGENCIA

           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO

           WRITE   FD-PEDCTAC          FROM REG-PEDCTAC
           PERFORM 1120-TESTAR-FS-PEDCTAC

           ADD     1                   TO WRK-AC-GRA-PEDCTAC.

       2100-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2110-LER-VVPEDICT               SECTION.
      *----------------------------------------------------------------*

           MOVE    PEDCTAC-CNPJ        TO CADA-PDCT-CH-CNPJ-CONTR
           MOVE    PEDCTAC-FILIAL      TO CADA-PDCT-CH-FILIAL-CONTR
           MOVE    PEDCTAC-CONTROLE    TO CADA-PDCT-CH-CTRL-CONTR
           MOVE    PEDCTAC-COD-PRODUTO TO CADA-PDCT-CH-PRODUTO

           READ    VVPEDICT

           MOVE    WRK-LEITURA         TO WRK-OPERACAO
           PERFORM 1140-TESTAR-FS-VVPEDICT

           IF WRK-FS-VVPEDICT          EQUAL '23'
              DISPLAY '******************* BVVE1962 ******************'
              DISPLAY '*                                             *'
              DISPLAY '* PROPOSTA NAO ENCONTRADA NO ARQUIVO VVPEDICT *'
              DISPLAY '*           PROCESSAMENTO CANCELADO           *'
              DISPLAY '*                                             *'
170614        DISPLAY '* PEDCTAC-CNPJ        : ' PEDCTAC-CNPJ
170614        DISPLAY '* PEDCTAC-FILIAL      : ' PEDCTAC-FILIAL
170614        DISPLAY '* PEDCTAC-CONTROLE    : ' PEDCTAC-CONTROLE
170614        DISPLAY '* PEDCTAC-COD-PRODUTO : ' PEDCTAC-COD-PRODUTO
170614        DISPLAY '*                                             *'
              DISPLAY '******************* BVVE1962 ******************'
           END-IF.

       2110-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2120-ATUALIZAR-VVPEDICT         SECTION.
      *----------------------------------------------------------------*

           IF PEDCTNAC-TIPO-REG        EQUAL 2
              MOVE   'I'               TO CADA-STATUS

              PERFORM VARYING WRK-IND  FROM 1 BY 1
                      UNTIL   WRK-IND  GREATER 339 OR
                              PEDCTNAC-TAB-COD-ERRO(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      PERFORM 2130-PESQUISAR-ERRO
              END-PERFORM

           ELSE
              MOVE   'A'               TO CADA-STATUS
              MOVE    PEDCTAC-NUM-CONTRATO
                                       TO CADA-NRO-CONTRATO
4SYSFB        PERFORM 2150-MOVER-CONTRATO
           END-IF

           REWRITE CADA-PEDIDO-CARTAO

           MOVE    WRK-REGRAVACAO      TO WRK-OPERACAO
           PERFORM 1140-TESTAR-FS-VVPEDICT

           ADD     1                   TO WRK-AC-ATU-VVPEDICT.

       2120-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2130-PESQUISAR-ERRO             SECTION.
      *----------------------------------------------------------------*

           SEARCH  ALL  TAB-ERROS-VV
                   AT END
                      CONTINUE

                   WHEN TAB-COD-ERRO-VV       (IND-ERR-VV) EQUAL
                        PEDCTNAC-TAB-COD-ERRO (WRK-IND)
                        PERFORM 2140-MARCAR-FLAGS-ERRO
           END-SEARCH.

       2130-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       2140-MARCAR-FLAGS-ERRO          SECTION.
      *----------------------------------------------------------------*

           EVALUATE  TAB-CAMPO-ERRO-VV(IND-ERR-VV)
               WHEN  SPACES
                     CONTINUE

               WHEN 'LOGRADOURO'
                     MOVE 'X'          TO CADA-LOGRADOURO-F

               WHEN 'NRO LOGRADOURO'
                     MOVE 'X'          TO CADA-NRO-LOGRADOURO-F

               WHEN 'MUNICIPIO'
                     MOVE 'X'          TO CADA-MUNICIPIO-F

               WHEN 'ESTADO'
                     MOVE 'X'          TO CADA-ESTADO-F

               WHEN 'CEP'
                     MOVE 'X'          TO CADA-CEP-F

               WHEN 'DDD'
                     MOVE 'X'          TO CADA-DDD-F

               WHEN 'TELEFONE'
                     MOVE 'X'          TO CADA-TELEFONE-F

               WHEN 'CONTRATO'
                     MOVE 'X'          TO CADA-NRO-CONTRATO-F

               WHEN 'INTER D FUNC'
                     MOVE 'X'          TO CADA-INTER-D-FUNC-F

               WHEN 'NOME INTER D'
                     MOVE 'X'          TO CADA-NOME-INTER-D-F

               WHEN 'CPF INTER D'
                     MOVE 'X'          TO CADA-CPF-INTER-D-F

               WHEN 'DT NASC INTER D'
                     MOVE 'X'          TO CADA-DT-NASC-INTER-D-F

               WHEN 'DDD INTER D'
                     MOVE 'X'          TO CADA-DDD-INTER-D-F

               WHEN 'TELEFONE INTER D'
                     MOVE 'X'          TO CADA-TELEFONE-INTER-D-F

               WHEN 'INTER O FUNC'
                     MOVE 'X'          TO CADA-INTER-O-FUNC-F

               WHEN 'NOME INTER O'
                     MOVE 'X'          TO CADA-NOME-INTER-O-F

               WHEN 'CPF INTER O'
                     MOVE 'X'          TO CADA-CPF-INTER-O-F

               WHEN 'DT NASC INTER O'
                     MOVE 'X'          TO CADA-DT-NASC-INTER-O-F

               WHEN 'DDD INTER O'
                     MOVE 'X'          TO CADA-DDD-INTER-O-F

               WHEN 'TELEFONE INTER O'
                     MOVE 'X'          TO CADA-TELEFONE-INTER-O-F

               WHEN 'BANCO'
                     MOVE 'X'          TO CADA-BANCO-VENDEU-F

               WHEN 'AGENCIA'
                     MOVE 'X'          TO CADA-AGENCIA-VENDEU-F

               WHEN 'QTD FUNC'
                     MOVE 'X'          TO CADA-QTD-FUNC-BENEFI-F

               WHEN 'VLR MEDIO'
                     MOVE 'X'          TO CADA-VLR-MEDIO-BENEFI-F

               WHEN 'FORMA PAGTO'
                     MOVE 'X'          TO CADA-FORMA-PAGTO-F

               WHEN 'MEIO PAGTO'
                     MOVE 'X'          TO CADA-MEIO-PAGTO-F

               WHEN 'NOME EMBOSSING'
                     MOVE 'X'          TO CADA-NOME-EMBOSSING-F

               WHEN 'ENTREGA CENTRAL'
                     MOVE 'X'          TO CADA-ENTREGA-CENTRAL-F

               WHEN 'CNPJ ENTREGA'
                     MOVE 'X'          TO CADA-CNPJ-ENTREGA-F

               WHEN 'FATURA CENTRAL'
                     MOVE 'X'          TO CADA-FATURA-CENTRAL-F

               WHEN 'CNPJ FATURA'
                     MOVE 'X'          TO CADA-CNPJ-FATURA-F

               WHEN 'CNPJ COBRANCA'
                     MOVE 'X'          TO CADA-CNPJ-COBRANCA-F

               WHEN 'REEMISSAO'
                     MOVE 'X'          TO CADA-FUNC-PEDIR-REEMISSAO-F
           END-EVALUATE.

       2140-99-FIM.                    EXIT.


4SYSFB*----------------------------------------------------------------*
.      2150-MOVER-CONTRATO             SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE CADA-PEDIDO-CARTAO     TO CADA-CONTRATO
.
.          IF CADA-NRO-CONTRATO        EQUAL ZEROS
.             WRITE CADA-CONTRATO
.          ELSE
.             REWRITE CADA-CONTRATO
.          END-IF
.
.          MOVE WRK-GRAVACAO           TO WRK-OPERACAO
.          PERFORM 1150-TESTAR-FS-VVCONTRT
.
.          ADD     1                   TO WRK-AC-GRA-VVCONTRT.
.
.     *----------------------------------------------------------------*
.      2150-99-FIM.                    EXIT.
4SYSFB*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 9010-DISPLAY-TOTAIS

           CLOSE   RETPEDCT
                   VVPEDICT
                   PEDCTAC
                   PEDCTNAC
4SYSFB             VVCONTRT

           MOVE    WRK-FECHAMENTO      TO WRK-OPERACAO
           PERFORM 1100-TESTAR-FILE-STATUS

           STOP    RUN.

       9000-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       9010-DISPLAY-TOTAIS             SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-AC-LID-RETPEDCT TO WRK-ED-LID-RETPEDCT
           MOVE    WRK-AC-REG-AC       TO WRK-ED-REG-AC
           MOVE    WRK-AC-REG-NAC      TO WRK-ED-REG-NAC
           MOVE    WRK-AC-GRA-PEDCTAC  TO WRK-ED-GRA-PEDCTAC
           MOVE    WRK-AC-GRA-PEDCTNAC TO WRK-ED-GRA-PEDCTNAC
           MOVE    WRK-AC-ATU-VVPEDICT TO WRK-ED-ATU-VVPEDICT
           MOVE    WRK-AC-GRA-VVCONTRT TO WRK-ED-GRA-VVCONTRT

           DISPLAY '******************** BVVE1962 ********************'
           DISPLAY '*                                                *'
           DISPLAY '* TOTAL DE REGISTROS LIDOS RETPEDCT: '
                      WRK-ED-LID-RETPEDCT                          ' *'
           DISPLAY '* TOTAL DE REGISTROS ACEITOS       : '
                      WRK-ED-REG-AC                                ' *'
           DISPLAY '* TOTAL DE REGISTROS NAO ACEITOS   : '
                      WRK-ED-REG-NAC                               ' *'
           DISPLAY '*                                                *'
           DISPLAY '* TOTAL DE PROPOSTAS ACEITAS       : '
                      WRK-ED-GRA-PEDCTAC                           ' *'
           DISPLAY '* TOTAL DE PROPOSTAS NAO ACEITAS   : '
                      WRK-ED-GRA-PEDCTNAC                          ' *'
           DISPLAY '*                                                *'
           DISPLAY '* TOTAL DE PROPOSTAS ATUALIZADAS   : '
                      WRK-ED-ATU-VVPEDICT                          ' *'
4SYSFB     DISPLAY '*                                                *'
.          DISPLAY '* TOTAL DE CONTRATOS GRAVADOS      : '
.                     WRK-ED-GRA-VVCONTRT                          ' *'
.          DISPLAY '*                                                *'
4SYSFB     DISPLAY '******************** BVVE1962 ********************'.

       9010-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
       9999-CANCELA-PROGRAMA           SECTION.
      *----------------------------------------------------------------*

           MOVE   'BVVE1962'           TO ERR-PGM
           MOVE   'APL'                TO ERR-TIPO-ACESSO
           MOVE   'PROGRAMA CANCELADO' TO ERR-TEXTO

           CALL   'BRAD7100'           USING WRK-BATCH
                                             ERRO-AREA

           GOBACK.

       9999-99-FIM.                    EXIT.
