      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*
       PROGRAM-ID. GFCT2835.
       AUTHOR.     JOHNNY.
      *==============================================================*
      *                          G P T I                              *
      *---------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2835                                   *
      *    ANALISTA....:   J.C.R.                                     *
      *    DATA........:   19/11/2008                                 *
      *---------------------------------------------------------------*
      *    OBJETIVO....:                                              *
      *---------------------------------------------------------------*
      *    ARQUIVOS....:                                              *
      *                DDNAME                      INCLUDE/BOOK       *
      *                    CADIDCLI                   GFCTWAD5        *
      *                    ARQIDCLI                   GFCTWAD6        *
      *===============================================================*
      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*
      *---------------------------------------------------------------*
       CONFIGURATION                    SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT                     SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT CADIDCLI ASSIGN TO UT-S-CADIDCLI
                      FILE STATUS IS WRK-FS-CADIDCLI.

           SELECT ARQIDCLI ASSIGN TO UT-S-ARQIDCLI
                      FILE STATUS IS WRK-FS-ARQIDCLI.

      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*
      *---------------------------------------------------------------*
       FILE                             SECTION.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 142 + 4 (VB)      *
      *---------------------------------------------------------------*

       FD  CADIDCLI
           RECORDING MODE IS V
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAD5'.

      *---------------------------------------------------------------*
      *    OUTPUT:    ORG. SEQUENCIAL   -   LRECL = 154 + 4 (VB)      *
      *---------------------------------------------------------------*

       FD  ARQIDCLI
           RECORDING MODE IS V
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAD6'.

      *---------------------------------------------------------------*
       WORKING-STORAGE                  SECTION.
      *---------------------------------------------------------------*

       77  FILLER                      PIC X(32)        VALUE
                   '* INICIO DA WORKING GFCT2835 *'.

       77  WRK-BATCH                   PIC X(08) VALUE  'BATCH'.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA VERIFICACAO DOS FILE-STATUS    *
      *---------------------------------------------------------------*

       77  WRK-FS-CADIDCLI             PIC X(02) VALUE  SPACES.
       77  WRK-FS-ARQIDCLI             PIC X(02) VALUE  SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE  'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE  'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA ACUMULADORES                   *
      *---------------------------------------------------------------*

       77  WRK-ACU-LIDOS-CADIDCLI      PIC 9(09) COMP-3 VALUE  ZEROS.
       77  WRK-ACU-GRAVADOS            PIC 9(09) COMP-3 VALUE  ZEROS.

       01  FILLER.
           05    WRK-LINHA-IMP.
              07    WRK-NOME              PIC  X(40) VALUE SPACES.
              07    WRK-ENDERECO          PIC  X(40) VALUE SPACES.
              07    WRK-CEP               PIC  9(05) COMP-3 VALUE ZEROS.
              07    WRK-CEP-COMP          PIC  9(03) COMP-3 VALUE ZEROS.
              07    WRK-CPF               PIC  9(09) COMP-3 VALUE ZEROS.
              07    WRK-CIDADE            PIC  X(30) VALUE SPACES.
              07    WRK-ESTADO            PIC  X(02) VALUE SPACES.

      *---------------------------------------------------------------*
      *              AREAS AUXILIAR PARA MSGS DE ERRO                 *
      *---------------------------------------------------------------*

       01  WRK-MENSAGEM-ERRO.
           03  FILLER                  PIC  X(09) VALUE '*** ERRO '.
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.
           03  FILLER                  PIC  X(12) VALUE ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(08) VALUE SPACES.
           03  FILLER                  PIC  X(17) VALUE
               ' - FILE STATUS = '.
           03  WRK-FS                  PIC  X(02) VALUE SPACES.
           03  FILLER                  PIC  X(04) VALUE ' ***'.

      *---------------------------------------------------------------*
      *          AREA POOL0160 PARA JOBNAME                           *
      *---------------------------------------------------------------*

       01  WRK-JOB                     PIC X(08)           VALUE SPACES.
       01  WRK-VALORFAC                PIC 9(05)V99 COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
      *          AREA POOL7100                                        *
      *---------------------------------------------------------------*

       COPY POL7100C.

      *---------------------------------------------------------------*
      *          AREA DISPLAY DE ESTATISTICA                          *
      *---------------------------------------------------------------*

       COPY GFCTWAD0.

      *----------------------------------------------------------------*
      *    CAMPOS AUXILIARES UTILIZADOS NA POOL7600                    *
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC  9(05)  COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC  9(07)  COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC  9(09)  COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC  9(07)  COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(13)  COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP           PIC  X(20)          VALUE SPACES.
           03  FILLER                  REDEFINES        WRK-TIMESTAMP.
               05  WRK-DATA.
                   07  WRK-ANO         PIC  9(04).
                   07  WRK-MES         PIC  9(02).
                   07  WRK-DIA         PIC  9(02).
               05  WRK-HORA            PIC  9(02).
               05  WRK-MINUTO          PIC  9(02).
               05  WRK-SEGUNDO         PIC  9(02).
               05  FILLER              PIC  9(06).

       01  WRK-TIMESTAMP-DB2.
           03  WRK-ANO-TIMESTAMP-DB2       PIC 9(04) COMP-3 VALUE ZEROS.
           03  FILLER                      PIC X(01)  VALUE '-'.
           03  WRK-MES-TIMESTAMP-DB2       PIC 9(02) COMP-3 VALUE ZEROS.
           03  FILLER                      PIC X(01)  VALUE '-'.
           03  WRK-DIA-TIMESTAMP-DB2       PIC 9(02) COMP-3 VALUE ZEROS.
           03  FILLER                      PIC X(01)  VALUE '-'.
           03  WRK-HORA-TIMESTAMP-DB2      PIC 9(02) COMP-3 VALUE ZEROS.
           03  FILLER                      PIC X(01)  VALUE '.'.
           03  WRK-MIN-TIMESTAMP-DB2       PIC 9(02) COMP-3 VALUE ZEROS.
           03  FILLER                      PIC X(01)  VALUE '.'.
           03  WRK-SEG-TIMESTAMP-DB2       PIC 9(02) COMP-3 VALUE ZEROS.
           03  FILLER                      PIC X(01)  VALUE '.'.

       01  FILLER                      PIC X(32)        VALUE
           '*  FIM DA WORKING GFCT2835 *'.

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*
      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSA  UNTIL  WRK-FS-CADIDCLI EQUAL '10'.

           PERFORM 3000-FINALIZA.

      *---------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT   CADIDCLI
                OUTPUT  ARQIDCLI.

           MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-PRIMEIRA-LEITURA.

           PERFORM 1300-OBTER-DATA-HORA.

           CALL    'POOL0160'     USING WRK-JOB  WRK-VALORFAC.

           MOVE  WRK-JOB          TO    WRK-JOBNAME.
           MOVE  'GFCT2835'       TO    WRK-PROGNAME.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 1120-TESTAR-FS-CADIDCLI.

           PERFORM 1130-TESTAR-FS-ARQIDCLI.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1120-TESTAR-FS-CADIDCLI         SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CADIDCLI NOT EQUAL '00'
              DISPLAY '************** GFCT2835 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            CADIDCLI               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADIDCLI
                                                 '         *'
              DISPLAY '************** GFCT2835 *************'
              MOVE  'APL'             TO      ERR-TIPO-ACESSO
              MOVE  'CADIDCLI'        TO      WRK-NOME-ARQUIVO
              MOVE  WRK-FS-CADIDCLI   TO      WRK-FS
              MOVE  WRK-MENSAGEM-ERRO TO      ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1130-TESTAR-FS-ARQIDCLI          SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQIDCLI  NOT EQUAL '00'
              DISPLAY '************** GFCT2835 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            ARQIDCLI                *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQIDCLI
                                                 '         *'
              DISPLAY '************** GFCT2835 *************'
              MOVE  'APL'             TO      ERR-TIPO-ACESSO
              MOVE  'ARQIDCLI'        TO      WRK-NOME-ARQUIVO
              MOVE  WRK-FS-ARQIDCLI    TO      WRK-FS
              MOVE  WRK-MENSAGEM-ERRO TO      ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1200-PRIMEIRA-LEITURA           SECTION.
      *---------------------------------------------------------------*

           PERFORM  1310-LER-CADIDCLI.

           IF  WRK-FS-CADIDCLI  EQUAL  '10'
               DISPLAY '************ GFCT2835 ***************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO  CADIDCLI  VAZIO       *'
               DISPLAY '*                                   *'
               DISPLAY '*     PROCESSAMENTO ENCERRADO       *'
               DISPLAY '*                                   *'
               DISPLAY '************ GFCT2835 ***************'
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1300-OBTER-DATA-HORA            SECTION.
      *---------------------------------------------------------------*

           CALL  'POOL7600'  USING  WRK-DATA-HORA.

           MOVE  WRK-ANO        TO  WRK-ANO-TIMESTAMP-DB2.
           MOVE  WRK-MES        TO  WRK-MES-TIMESTAMP-DB2.
           MOVE  WRK-DIA        TO  WRK-DIA-TIMESTAMP-DB2.
           MOVE  WRK-HORA       TO  WRK-HORA-TIMESTAMP-DB2.
           MOVE  WRK-MINUTO     TO  WRK-MIN-TIMESTAMP-DB2.
           MOVE  WRK-SEGUNDO    TO  WRK-SEG-TIMESTAMP-DB2.

           STRING WRK-DIA '/'
                  WRK-MES '/'
                  WRK-ANO
           DELIMITED BY SIZE           INTO WRK-DATAMOV.

           STRING WRK-HORA ':'
                  WRK-MINUTO  ':'
                  WRK-SEGUNDO
           DELIMITED BY SIZE           INTO WRK-HORAMOV.

      *---------------------------------------------------------------*
       1300-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1310-LER-CADIDCLI               SECTION.
      *---------------------------------------------------------------*

           READ    CADIDCLI.

           IF      WRK-FS-CADIDCLI         EQUAL      '10'
                   GO  TO 1310-99-FIM
           END-IF.

           MOVE    WRK-LEITURA             TO   WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-CADIDCLI.

           ADD     1                       TO   WRK-ACU-LIDOS-CADIDCLI.

      *---------------------------------------------------------------*
       1310-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2000-PROCESSA                   SECTION.
      *---------------------------------------------------------------*

           PERFORM 2110-GRAVA-ARQIDCLI.

           PERFORM 1310-LER-CADIDCLI.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2110-GRAVA-ARQIDCLI              SECTION.
      *---------------------------------------------------------------*

           MOVE  D5-CAD-CHAVE           TO  D6-ARQ-CHAVE.

           MOVE  D5-CAD-NOME            TO  WRK-NOME.
           MOVE  D5-CAD-ENDERECO        TO  WRK-ENDERECO.
           MOVE  D5-CAD-CEP-NRO         TO  WRK-CEP.
           MOVE  D5-CAD-CEP-COMPL       TO  WRK-CEP-COMP.
           MOVE  SPACES                 TO  WRK-CIDADE
                                            WRK-ESTADO.

           MOVE  D5-CAD-NRO-CGC-CPF     TO  WRK-CPF.

           MOVE  WRK-LINHA-IMP          TO  D6-ARQ-LINHA-IMP.

           MOVE  WRK-GRAVACAO           TO  WRK-OPERACAO.
           WRITE REG-ARQIDCLI.
           PERFORM 1130-TESTAR-FS-ARQIDCLI.
           ADD   1                      TO  WRK-ACU-GRAVADOS.

      *---------------------------------------------------------------*
       2110-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       3000-FINALIZA                   SECTION.
      *---------------------------------------------------------------*

           PERFORM  3100-EXIBIR-DISP-ESTATISTICAS.

           CLOSE  CADIDCLI
                  ARQIDCLI.

           MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       3100-EXIBIR-DISP-ESTATISTICAS   SECTION.
      *---------------------------------------------------------------*

           DISPLAY WRK-DISP1.
           DISPLAY WRK-DISP2.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP4.
           DISPLAY WRK-DISP5.
           DISPLAY WRK-DISP6.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP7.
           DISPLAY WRK-DISP8.

           MOVE WRK-ACU-LIDOS-CADIDCLI TO WRK-QTDEARQ.
           MOVE 'CADIDCLI'             TO WRK-DDNAME.
           MOVE 'I'                    TO WRK-I-O.
           MOVE 'ARQUIVO IDENT CLIENTES     '
                                       TO WRK-DESCARQ.
           DISPLAY WRK-DISP9.

           MOVE WRK-ACU-GRAVADOS       TO WRK-QTDEARQ.
           MOVE 'ARQIDCLI'             TO WRK-DDNAME.
           MOVE 'O'                    TO WRK-I-O.
           MOVE 'GRAVADOS                   '
                                       TO WRK-DESCARQ.
           DISPLAY WRK-DISP9.

           DISPLAY WRK-DISP1.

      *---------------------------------------------------------------*
       3100-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *---------------------------------------------------------------*

           MOVE  'GFCT2835'            TO      ERR-PGM.

           CALL  'POOL7100'            USING   WRK-BATCH
                                               ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*

