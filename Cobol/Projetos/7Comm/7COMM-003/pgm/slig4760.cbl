      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SLIG4760.
       AUTHOR.     VINICIUS LAURINDO OLIVEIRA.
      *================================================================*
      *                    B R Q  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   SLIG4760                                    *
      *    PROGRAMADOR.:   VINICIUS L OLIVEIRA                         *
      *    ANALISTA....:                                               *
      *    DATA........:   28/05/2013                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   REFORMATAR REGISTRO NA IMAGEM DA TABELA DE  *
      *                    DEPENDENCIA BANDA MAGNETICA                 *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                        INCLUDE/BOOK      *
      *                    DEPBNDAE                     I#SLIG52       *
      *                    DEPBNDAS                     I#SLIG5B       *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERROS                              *
      *================================================================*
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT DEPBNDAE ASSIGN      TO UT-S-DEPBNDAE
           FILE STATUS                 IS WRK-FS-DEPBNDAE.

           SELECT DEPBNDAS ASSIGN      TO UT-S-DEPBNDAS
           FILE STATUS                 IS WRK-FS-DEPBNDAS.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    INPUT   ARQUIVO CHEQUE - DEPDC-BANDA-MAGNT                  *
      *            ORG. SEQUENCIAL     -   LRECL   =   125             *
      *----------------------------------------------------------------*

       FD  DEPBNDAE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#SLIG52'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO COM CONVERSAO DE FILIAL                     *
      *            ORG. SEQUENCIAL     -   LRECL   =   130             *
      *----------------------------------------------------------------*

       FD  DEPBNDAS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#SLIG5B'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** SLIG4760 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-DEPBNDAE          PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-DEPBNDAS          PIC  9(012) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-DEPBNDAE             PIC  X(002)         VALUE SPACES.

       77  WRK-FS-DEPBNDAS             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MSG01                   PIC  X(075)         VALUE
           '** ARQUIVO DEPBNDAE VAZIO **'.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MENSAGEM DE ERRO FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(008)        VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)        VALUE SPACES.
           03  FILLER                  PIC  X(012)        VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(008)        VALUE SPACES.
           03  FILLER                  PIC  X(017)        VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)        VALUE SPACES.
           03  FILLER                  PIC  X(003)        VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)        VALUE
           '*** AREA PARA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(075)        VALUE
           '*** SLIG4760 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-FS-DEPBNDAE EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                     EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  DEPBNDAE
               OUTPUT  DEPBNDAS.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS        SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-DEPBNDAE.

           PERFORM 1120-TESTAR-FS-DEPBNDAS.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1110-TESTAR-FS-DEPBNDAE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DEPBNDAE         NOT EQUAL ZEROS
               MOVE 'DEPBNDAE'         TO WRK-NOME-ARQUIVO
               MOVE WRK-FS-DEPBNDAE    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1120-TESTAR-FS-DEPBNDAS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DEPBNDAS         NOT EQUAL ZEROS
               MOVE 'DEPBNDAS'         TO WRK-NOME-ARQUIVO
               MOVE WRK-FS-DEPBNDAS    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-DEPBNDAE.

           IF  WRK-FS-DEPBNDAE         EQUAL '10' AND
               ACU-LIDOS-DEPBNDAE      EQUAL ZEROS
               DISPLAY '****************** SLIG4760 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*          ARQUIVO DEPBNDAE VAZIO            *'
               DISPLAY '*                                            *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG4760 ******************'
               MOVE WRK-MSG01          TO ERR-TEXTO
VOLTAR         PERFORM 4000-FINALIZAR
VOLTAR*        P*RFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *---------------------------------------------------------------*
       2100-LER-DEPBNDAE               SECTION.
      *---------------------------------------------------------------*

           READ DEPBNDAE.

           IF  WRK-FS-DEPBNDAE         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           ADD  1                      TO ACU-LIDOS-DEPBNDAE.

           PERFORM 1110-TESTAR-FS-DEPBNDAE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-FORMATAR-DEPBNDAS.

           PERFORM 3200-GRAVAR-DEPBNDAS.

           PERFORM 2100-LER-DEPBNDAE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *---------------------------------------------------------------*
       3100-FORMATAR-DEPBNDAS          SECTION.
      *---------------------------------------------------------------*

           IF SLIG52-INDCD-PSSOA-SACDO   EQUAL 'F'
              MOVE '99991'               TO SLIG5B-CFLIAL-SACDO-ST
              MOVE ZEROS                 TO SLIG5B-CCTRL-SACDO-ST
           ELSE
              MOVE '99992'               TO SLIG5B-CFLIAL-SACDO-ST
              MOVE ZEROS                 TO SLIG5B-CCTRL-SACDO-ST
           END-IF.

           MOVE SLIG52-EMPR-INC          TO SLIG5B-EMPR-INC
           MOVE SLIG52-DEPENDENCIA       TO SLIG5B-DEPENDENCIA
           MOVE SLIG52-BCO-SACDO         TO SLIG5B-BCO-SACDO
           MOVE SLIG52-AGEN-SACDO        TO SLIG5B-AGEN-SACDO
           MOVE SLIG52-CONTA-SACDO       TO SLIG5B-CONTA-SACDO
           MOVE SLIG52-CGC-CPF-SACDO-ST     TO SLIG5B-CGC-CPF-SACDO-ST
           MOVE SLIG52-INDCD-PSSOA-SACDO TO SLIG5B-INDCD-PSSOA-SACDO
           MOVE SLIG52-IDTFD-CART-LIQDZ  TO SLIG5B-IDTFD-CART-LIQDZ
           MOVE SLIG52-VCTO-PER-PG       TO SLIG5B-VCTO-PER-PG
           MOVE SLIG52-VCTO-PER-PG-NULL  TO SLIG5B-VCTO-PER-PG-NULL
           MOVE SLIG52-VCTO-PER-NAO-PG   TO SLIG5B-VCTO-PER-NAO-PG
           MOVE SLIG52-VCTO-PER-BXADO    TO SLIG5B-VCTO-PER-BXADO
           MOVE SLIG52-VCTO-APOS-PER-PG  TO SLIG5B-VCTO-APOS-PER-PG
           MOVE SLIG52-CHEQ-PRIM-DEVLC   TO SLIG5B-CHEQ-PRIM-DEVLC
           MOVE SLIG52-CHEQ-REAP         TO SLIG5B-CHEQ-REAP
           MOVE SLIG52-CHEQ-REAP-NULL    TO SLIG5B-CHEQ-REAP-NULL
           MOVE SLIG52-CHEQ-REAP-DEVOL   TO SLIG5B-CHEQ-REAP-DEVOL
           MOVE SLIG52-CHEQ-REAP-PG      TO SLIG5B-CHEQ-REAP-PG
           MOVE SLIG52-CHEQ-REAP-PG-NULL TO SLIG5B-CHEQ-REAP-PG-NULL
           MOVE SLIG52-CHEQ-DEVOL-ANTER  TO SLIG5B-CHEQ-DEVOL-ANTER

           MOVE SLIG52-VCTO-PER-NAO-PG-NULL
                                     TO SLIG5B-VCTO-PER-NAO-PG-NULL.
           MOVE SLIG52-VCTO-PER-BXADO-NULL
                                     TO SLIG5B-VCTO-PER-BXADO-NULL.
           MOVE SLIG52-VCTO-APOS-PER-PG-NULL
                                     TO SLIG5B-VCTO-APOS-PER-PG-NULL.
           MOVE SLIG52-VCTO-APOS-PER-NAO-PG
                                     TO SLIG5B-VCTO-APOS-PER-NAO-PG.
           MOVE SLIG52-VCTO-APOS-PER-N-PG-NUL
                                     TO SLIG5B-VCTO-APOS-PER-N-PG-NUL.
           MOVE SLIG52-VCTO-APOS-PER-BXADO
                                     TO SLIG5B-VCTO-APOS-PER-BXADO.
           MOVE SLIG52-VCTO-APOS-PER-BXADO-NUL
                                     TO SLIG5B-VCTO-APOS-PER-BXADO-NUL.
           MOVE SLIG52-CHEQ-PRIM-DEVLC-NULL
                                     TO SLIG5B-CHEQ-PRIM-DEVLC-NULL.
           MOVE SLIG52-CHEQ-DEVOL-ANTER-NULL
                                     TO SLIG5B-CHEQ-DEVOL-ANTER-NULL.
           MOVE SLIG52-CHEQ-REAP-DEVOL-NULL
                                     TO SLIG5B-CHEQ-REAP-DEVOL-NULL.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *---------------------------------------------------------------*
       3200-GRAVAR-DEPBNDAS            SECTION.
      *---------------------------------------------------------------*

           WRITE REG-DEPDC-BANDA-MAGNT OF DEPBNDAS.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-DEPBNDAS.

           ADD 1                       TO ACU-GRAVA-DEPBNDAS.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3500-GERAR-ESTATISTICA          SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*----------------SLIG4760-----------------'
           DISPLAY '*   ESTATISTICAS DE PROCESSAMENTO         '
           DISPLAY '*-----------------------------------------'
           DISPLAY '* QTDE DE REGS LIDOS (DEPBNDAE) = '
                      ACU-LIDOS-DEPBNDAE
           DISPLAY '* QTDE DE REGS GRAV  (DEPBNDAS) = '
                      ACU-GRAVA-DEPBNDAS
           DISPLAY '*-----------------------------------------'.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           CLOSE DEPBNDAE
                 DEPBNDAS.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 3500-GERAR-ESTATISTICA.

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           CLOSE DEPBNDAE
                 DEPBNDAS.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'SLIG4760'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
