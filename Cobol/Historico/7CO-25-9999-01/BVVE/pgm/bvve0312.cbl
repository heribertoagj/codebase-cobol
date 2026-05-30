      *================================================================*
       IDENTIFICATION             DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE0312.
       AUTHOR.     FRANCISMARA PAGNONCELLI.
      *================================================================*
      *                    B  S  I - T E C N O L O G I A               *
      *----------------------------------------------------------------*
      *    PROGRAMA     : BVVE0312                                     *
      *    PROGRAMADOR  : FRANCISMARA PAGONCELLI      - BSI            *
      *    ANALISTA     : SERGIO VIEIRA JUNIOR        - BSI            *
      *    DATA         : 01/08/2013                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO     :                                              *
      *      CONVERTER CODIGO PARA AGENDA FINANCEIRA                   *
      *----------------------------------------------------------------*
      *    ARQUIVOS     :                                              *
      *      DDNAME                                INCLUDE/BOOK        *
      *       AGENDANT                                   -             *
      *       AGENDATU                                   -             *
      *----------------------------------------------------------------*
      *    MODULOS CHAMADOS:                                           *
      *      POOL7100 - MODULO DE TRATAMENTO DE ERRO                   *
      *================================================================*
      *    PROGRAMADORA: FRANCISMARA   - WIPRO                         *
      *    DATA........: SETEMBRO/2018                                 *
      *----------------------------------------------------------------*
      *    ALTERACAO...: EXPANSAO DO PRODUTO PARA 5 DIGITOS            *
      *----------------------------------------------------------------*

      *================================================================*
       ENVIRONMENT                DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION              SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT           IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT               SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT AGENDANT ASSIGN   TO UT-S-AGENDANT
                      FILE STATUS   IS WRK-FS-AGENDANT.

           SELECT AGENDATU ASSIGN   TO UT-S-AGENDATU
                      FILE STATUS   IS WRK-FS-AGENDATU.

      *================================================================*
       DATA                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                       SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  AGENDA FINACEIRA                                    *
      *            ORG. SEQUENCIAL - LRECL = 72                       *
      *----------------------------------------------------------------*

       FD  AGENDANT
           LABEL RECORD STANDARD
           RECORDING F
           BLOCK 0.

       01  FD-REG-AGENDANT             PIC  X(072).

      *----------------------------------------------------------------*
      *    OUTPUT: AGENDA FINANCIERA COM PRODUTOS CONVERTIDOS          *
      *            ORG. SEQUENCIAL - LRECL = 70                        *
      *----------------------------------------------------------------*

       FD  AGENDATU
           LABEL RECORD STANDARD
           RECORDING F
           BLOCK 0.

       01  FD-REG-AGENDATU               PIC X(070).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA AREA DE WORK ***'.
      *----------------------------------------------------------------*

       77  WRK-RECURSO                 PIC  X(008)         VALUE
                                                             'BATCH   '.

       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
       77  WRK-FS-AGENDANT             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-AGENDATU             PIC  X(002)         VALUE SPACES.

       77  ACU-LID-AGENDANT            PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV                    PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*** AREA DE LAYOUT ***'.
      *----------------------------------------------------------------*

       01  WRK-REG-ARQCREDT.
           05  WRK-CHAVE-CRED.
               10  WRK-CRED-AGEN       PIC  9(05)   COMP-3 VALUE ZEROS.
               10  WRK-CRED-CONTA      PIC  9(07)   COMP-3 VALUE ZEROS.
               10  WRK-CRED-PRODUTO    PIC  9(05)   COMP-3 VALUE ZEROS.
               10  WRK-CRED-DTCRED.
                   15  WRK-CRED-SSCRED PIC  9(02)          VALUE ZEROS.
                   15  WRK-CRED-AACRED PIC  9(02)          VALUE ZEROS.
                   15  WRK-CRED-MMCRED PIC  9(02)          VALUE ZEROS.
                   15  WRK-CRED-DDCRED PIC  9(02)          VALUE ZEROS.
               10  WRK-CRED-DTCREDN    REDEFINES WRK-CRED-DTCRED
                                       PIC  9(08).
           05  WRK-CRED-MOEDA          PIC  X(01)          VALUE SPACES.
           05  WRK-CRED-TIPOEXT        PIC  X(01)          VALUE SPACES.
           05  WRK-CRED-NUMVEND        PIC  9(13)    COMP-3 VALUE ZEROS.
           05  WRK-CRED-VALOR          PIC S9(13)V99 COMP-3 VALUE ZEROS.
           05  WRK-CRED-NOMPROP        PIC  X(35)          VALUE SPACES.
           05  WRK-CRED-BANDEIRA       PIC  X(02)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*** AREA PARA TESTES FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ABERTURA                PIC X(013) VALUE 'NA ABERTURA'.
       01  WRK-LEITURA                 PIC X(013) VALUE 'NA LEITURA'.
       01  WRK-GRAVACAO                PIC X(013) VALUE 'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC X(013) VALUE 'NO FECHAMENTO'.

       01  WRK-MENSAGEM-ERRO.
           03  FILLER                  PIC  X(004) VALUE '*** '.
           03  FILLER                  PIC  X(005) VALUE 'ERRO '.
           03  WRK-OPERACAO            PIC  X(013) VALUE SPACES.
           03  FILLER                  PIC  X(012) VALUE ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(009) VALUE SPACES.
           03  FILLER                  PIC  X(014) VALUE
               'FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002) VALUE SPACES.
           03  FILLER                  PIC  X(004) VALUE ' ***'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*** AREA PARA TRATAMENTO ERROS ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORK ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIO                     SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-FS-AGENDANT     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  AGENDANT
                OUTPUT AGENDATU.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-AGENDANT.
           PERFORM 1130-TESTAR-FS-AGENDATU.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-AGENDANT        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AGENDANT         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'AGENDANT'         TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-AGENDANT   TO WRK-FILE-STATUS
               MOVE  WRK-MENSAGEM-ERRO TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-AGENDATU           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AGENDATU          NOT EQUAL '00'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE 'AGENDATU'          TO WRK-NOME-ARQUIVO
               MOVE  WRK-FS-AGENDATU    TO WRK-FILE-STATUS
               MOVE  WRK-MENSAGEM-ERRO  TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-AGENDANT.

           IF  WRK-FS-AGENDANT          EQUAL '10'
               DISPLAY '******************************************'
               DISPLAY '*                                        *'
               DISPLAY '*            PROGRAMA ENCERRADO          *'
               DISPLAY '*                                        *'
               DISPLAY '*              ARQUIVO VAZIO             *'
               DISPLAY '*                                        *'
               DISPLAY '******************************************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-AGENDANT                SECTION.
      *----------------------------------------------------------------*

           READ AGENDANT              INTO WRK-REG-ARQCREDT.

           IF  WRK-FS-AGENDANT        EQUAL  '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-AGENDANT.

           ADD 1                       TO ACU-LID-AGENDANT.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           EVALUATE TRUE

               WHEN WRK-CRED-PRODUTO   EQUAL 01 AND
                    WRK-CRED-BANDEIRA  EQUAL '21'
                    MOVE 51            TO WRK-CRED-PRODUTO

               WHEN WRK-CRED-PRODUTO   EQUAL 02 AND
                    WRK-CRED-BANDEIRA  EQUAL '22'
                    MOVE 52            TO WRK-CRED-PRODUTO

               WHEN WRK-CRED-PRODUTO   EQUAL 11 AND
                    WRK-CRED-BANDEIRA  EQUAL '31'
                    MOVE 61            TO WRK-CRED-PRODUTO

               WHEN WRK-CRED-PRODUTO   EQUAL 12 AND
                    WRK-CRED-BANDEIRA  EQUAL '32'
                    MOVE 62            TO WRK-CRED-PRODUTO

           END-EVALUATE.

           PERFORM 3200-GRAVAR-AGENDATU.

           PERFORM 2100-LER-AGENDANT.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-GRAVAR-AGENDATU              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-REG-ARQCREDT(1:70)  TO FD-REG-AGENDATU

           WRITE FD-REG-AGENDATU.
           PERFORM 1130-TESTAR-FS-AGENDATU.

           ADD 1                       TO ACU-GRAV.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-TOTAIS.

           CLOSE AGENDANT
                 AGENDATU.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.
           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** BVVE0312 ******************'
           DISPLAY '*                                         *'

           MOVE ACU-LID-AGENDANT       TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS   : ' WRK-MASCARA '             *'

           DISPLAY '*                                         *'

           MOVE ACU-GRAV               TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVADOS: ' WRK-MASCARA '             *'

           DISPLAY '*                                         *'
           DISPLAY '*                                         *'
           DISPLAY '*************** BVVE0312 ******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE0312'             TO ERR-MODULO
                                          ERR-PGM.

           CALL 'BRAD7100'             USING WRK-RECURSO
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
