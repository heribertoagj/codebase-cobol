      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*
       PROGRAM-ID. GFCT2895.
       AUTHOR.     ALESSANDRO.
      *===============================================================*
      *                         G P T I                               *
      *---------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2895                                   *
      *    ANALISTA....:   ALESSANDRO LUIS PESSOLANO                  *
      *    DATA........:   DEZ/2008                                   *
      *    OBJETIVO....:   CONVERTE ACENTUACAO MAINFRAME PARA XEROX   *
      *                    E REGRAVA O CAMPO TIPO-EXTR.               *
      *---------------------------------------------------------------*
      *    ARQUIVOS....:                                              *
      *                DDNAME                      INCLUDE/BOOK       *
      *   -INPUT.:     ARQENTRA                         -             *
      *   -OUTPUT:     ARQSAI                           -             *
      *===============================================================*

       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ARQENTRA  ASSIGN TO UT-S-ARQENTRA
                      FILE STATUS IS WRK-FS-ARQENTRA.

           SELECT ARQSAI ASSIGN TO UT-S-ARQSAI
                      FILE STATUS IS WRK-FS-ARQSAI.
      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT:     ORG. SEQUENCIAL   -   LRECL = 232 BYTES         *
      *---------------------------------------------------------------*

       FD  ARQENTRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ARQENTRA.
           03  FILLER                     PIC  X(050).
           03  FD-ENT-LINHA-IMP.
               05  FD-ENT-CARRO-IMP       PIC  X(001).
               05  FD-ENT-FONTINDEX       PIC  X(001).
               05  FD-ENT-RESTART         PIC  9(005).
               05  FD-ENT-SEQUENCIA-EXTR  PIC  9(011)  COMP-3.
               05  FILLER                 PIC  X(001).
           03  FILLER                     PIC  X(010).

      *---------------------------------------------------------------*
      *    OUTPUT:    ORG. SEQUENCIAL   -   56 BYTES                  *
      *---------------------------------------------------------------*

       FD  ARQSAI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ARQSAI.
           03  FILLER                     PIC  X(050).
           03  FD-SAI-LINHA-IMP.
               05  FD-SAI-CARRO-IMP       PIC  X(001).
               05  FD-SAI-LINHA-EXTR      PIC  X(158).
               05  FD-SAI-FONTINDEX       PIC  X(001).
               05  FD-SAI-SEQUENCIA-EXTR  PIC  9(011)  COMP-3.
               05  FILLER                 PIC  X(001).
           03  FILLER                     PIC  X(010).

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

       77  FILLER                      PIC X(32)        VALUE
                   '* INICIO DA WORKING GFCT2895 *'.

       77  WRK-BATCH                   PIC X(08)        VALUE  'BATCH'.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA VERIFICACAO DOS FILE-STATUS    *
      *---------------------------------------------------------------*

       77  WRK-FS-ARQENTRA             PIC X(02) VALUE  SPACES.
       77  WRK-FS-ARQSAI               PIC X(02) VALUE  SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE  'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE  'NA LEITURA'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE  'NO FECHAMENTO'.

      *---------------------------------------------------------------*
      *       REGISTRO DOS ARQUIVOS DE ENTRADA E SAIDA.               *
      *---------------------------------------------------------------*

       01  WRK-ARQ.
           03  WRK-TIPO-REG            PIC  9(002) VALUE ZEROS.
           03  WRK-SUB-TIPO            PIC  9(001) VALUE ZEROS.
           03  WRK-TIPO-PESSOA         PIC  X(001) VALUE SPACE.
           03  WRK-TIPO-CORSP          PIC  9(001) VALUE ZEROS.
      * 1-COM FRANQUIA    2-SEM FRANQUIA    3-CLIENTE ESPECIAL
           03  WRK-CEP-FULL.
               05  FILLER              PIC  9(001) VALUE ZEROS.
               05  WRK-CEP             PIC  9(005) VALUE ZEROS.
               05  WRK-COMPL-CEP       PIC  9(003) VALUE ZEROS.
           03  WRK-CNPJ-CPF            PIC  9(009) VALUE ZEROS.
           03  WRK-FILIAL-CNPJ         PIC  9(005) VALUE ZEROS.
           03  WRK-CONTR-CNPJ          PIC  9(002) VALUE ZEROS.
           03  WRK-NUM-LINHA           PIC S9(005) COMP-3 VALUE ZEROS.
           03  WRK-TIPO-LINHA          PIC  X(001) VALUE SPACE.
           03  WRK-SEQUENCIA           PIC  9(005) COMP-3 VALUE ZEROS.
           03  WRK-SEGMENTACAO         PIC  X(001) VALUE SPACE.
           03  WRK-ACUM-REGISTRO       PIC S9(011) COMP-3 VALUE ZEROS.
           03  WRK-TIPO-DISTR          PIC  X(002) VALUE SPACES.
           03  WRK-TIPO-EXTRATO        PIC  X(002) VALUE SPACES.
           03  WRK-LINHA-IMP.
               05  WRK-CARRO-IMP       PIC  X(001) VALUE SPACE.
               05  WRK-LINHA-EXTR      PIC  X(158) VALUE SPACES.
               05  WRK-FONTINDEX       PIC  X(001) VALUE SPACE.
               05  WRK-RESTART         PIC  9(005) VALUE ZEROS.
               05  WRK-SEQUENCIA-EXTR  PIC  9(011) COMP-3 VALUE ZEROS.
               05  FILLER              PIC  X(001) VALUE SPACE.
           03  FILLER                  PIC  X(010) VALUE SPACES.

      *---------------------------------------------------------------*
      *              AREAS AUXILIARES PARA MSGS DE ERRO               *
      *---------------------------------------------------------------*

       01  WRK-MENSAGEM-ERRO.
           03  FILLER                  PIC  X(09) VALUE '*** ERRO '.
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.
           03  WRK-NOME-ARQUIVO        PIC  X(08) VALUE SPACES.
           03  FILLER                  PIC  X(17) VALUE
               ' - FILE STATUS = '.
           03  WRK-FS                  PIC  X(02) VALUE SPACES.
           03  FILLER                  PIC  X(04) VALUE ' ***'.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA ACUMULADORES                   *
      *---------------------------------------------------------------*

       77  WRK-ACU-LIDOS-ARQENTRA       PIC 9(09) COMP-3 VALUE  ZEROS.
       77  WRK-ACU-GRAV-ARQSAI          PIC 9(09) COMP-3 VALUE  ZEROS.
       77  WRK-IND                      PIC 9(03) COMP-3 VALUE  ZEROS.
       77  WRK-CHAVE-ARQENTRA           PIC X(11) VALUE SPACES.

      *---------------------------------------------------------------*
      *          AREAS AUXILIARES PARA CARACTERES ESPECIAIS           *
      *---------------------------------------------------------------*

       01  WRK-TEXTO                    PIC X(158)       VALUE SPACES.
       01  WRK-TEXTO-1                  REDEFINES    WRK-TEXTO.

      *---------------------------------------------------------------*
      *          AREA POOL7100                                        *
      *---------------------------------------------------------------*

       01  FILLER                      PIC  X(33)          VALUE
           '*  AREA DA POOL7100  *'.

       COPY POL7100C.

       01  FILLER                      PIC X(32)        VALUE
           '*  FIM DA WORKING GFCT2895 *'.

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

                OUTPUT  ARQSAI.

           MOVE    WRK-ABERTURA            TO   WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 4000-TESTA-ARQENTRA-VAZIO.

           PERFORM 6000-PROCESSA  UNTIL  WRK-CHAVE-ARQENTRA EQUAL
                                                      HIGH-VALUES.
           PERFORM 7000-FINALIZA.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 1300-TESTAR-FS-ARQENTRA.


      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1300-TESTAR-FS-ARQENTRA         SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQENTRA NOT EQUAL '00'
              DISPLAY '************** GFCT2895 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            ARQENTRA               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQENTRA
                                                 '         *'
              DISPLAY '************** GFCT2895 *************'
              MOVE  'APL'             TO      ERR-TIPO-ACESSO
              MOVE  'ARQENTRA'        TO      WRK-NOME-ARQUIVO
              MOVE  WRK-FS-ARQENTRA   TO      WRK-FS
              MOVE  WRK-MENSAGEM-ERRO TO      ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO

      *---------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1400-TESTAR-FS-ARQSAI           SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQSAI NOT EQUAL '00'
              DISPLAY '************** GFCT2895 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            ARQSAI               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQSAI
                                                 '         *'
              DISPLAY '************** GFCT2895 *************'
              MOVE  'APL'             TO      ERR-TIPO-ACESSO
              MOVE  'ARQSAI'          TO      WRK-NOME-ARQUIVO
              MOVE  WRK-FS-ARQSAI     TO      WRK-FS
              MOVE  WRK-MENSAGEM-ERRO TO      ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO

      *---------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-TESTA-ARQENTRA-VAZIO       SECTION.
      *---------------------------------------------------------------*

           PERFORM  4100-LER-ARQENTRA.

           IF  WRK-FS-ARQENTRA  EQUAL  '10'
               DISPLAY '************* GFCT2895 *************'
               DISPLAY '*                                  *'
               DISPLAY '*           SR. OPERADOR,          *'
               DISPLAY '*   O ARQUIVO ARQENTRA ESTA VAZIO   *'
               DISPLAY '*  O PROCESSAMENTO SERA ENCERRADO. *'
               DISPLAY '*                                  *'
               DISPLAY '************** GFCT2895 ************'
               PERFORM 7000-FINALIZA
           END-IF.
      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4100-LER-ARQENTRA               SECTION.
      *---------------------------------------------------------------*

           READ    ARQENTRA.

           IF      WRK-FS-ARQENTRA          EQUAL  '10'
                   MOVE HIGH-VALUES TO WRK-CHAVE-ARQENTRA
                   GO  TO 4100-99-FIM
           END-IF.

           MOVE    WRK-LEITURA             TO  WRK-OPERACAO.
           PERFORM 1300-TESTAR-FS-ARQENTRA.
           MOVE    FD-REG-ARQENTRA         TO  WRK-ARQ.
           ADD     1                       TO  WRK-ACU-LIDOS-ARQENTRA.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       6000-PROCESSA                   SECTION.
      *---------------------------------------------------------------*

           IF FD-ENT-FONTINDEX  NOT EQUAL '3'
              PERFORM 6010-CONVERTE
           END-IF.

           PERFORM 6200-GRAVA-ARQSAI.

           PERFORM 4100-LER-ARQENTRA.

      *---------------------------------------------------------------*
       6000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       6010-CONVERTE                   SECTION.
      *---------------------------------------------------------------*
           MOVE  FD-ENT-LINHA-EXTR  TO  WRK-TEXTO.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND EQUAL 159
                EVALUATE WRK-CARACTER (WRK-IND)
                   WHEN 'Ã'
                       MOVE X'DE'       TO     WRK-CARACTER (WRK-IND)
                   WHEN 'À'
                       MOVE X'DB'       TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Á'
                       MOVE X'DF'       TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Â'
                       MOVE X'DA'       TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ä'
                       MOVE X'CF'       TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ã'
                       MOVE X'51'       TO     WRK-CARACTER (WRK-IND)
                   WHEN 'à'
                       MOVE X'49'       TO     WRK-CARACTER (WRK-IND)
                   WHEN 'á'
                       MOVE X'52'       TO     WRK-CARACTER (WRK-IND)
                   WHEN 'â'
                   WHEN 'ä'
                       MOVE X'47'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'é'
                       MOVE X'43'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ê'
                       MOVE X'41'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'è'
                       MOVE X'42'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ë'
                       MOVE X'3F'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'É'
                       MOVE X'BA'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ê'
                       MOVE X'78'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'È'
                       MOVE X'8A'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ë'
                       MOVE X'75'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Í'
                       MOVE X'74'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ì'
                   WHEN 'Î'
                       MOVE X'72'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ï'
                       MOVE X'71'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'í'
                       MOVE X'3E'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ì'
                       MOVE X'3D'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'î'
                       MOVE X'3C'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ï'
                       MOVE X'3B'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'õ'
                       MOVE X'36'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ô'
                       MOVE X'34'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ó'
                       MOVE X'37'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ö'
                       MOVE X'33'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Õ'
                   WHEN 'Ô'
                       MOVE X'64'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ó'
                       MOVE X'67'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ö'
                       MOVE X'4F'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ò'
                       MOVE X'65'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ú'
                       MOVE X'57'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ü'
                       MOVE X'54'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ù'
                       MOVE X'56'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Û'
                       MOVE X'55'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ü'
                       MOVE X'2C'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ú'
                       MOVE X'2F'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ù'
                   WHEN 'û'
                       MOVE X'2D'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ç'
                       MOVE X'44'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'Ç'
                       MOVE X'CA'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'º'
                       MOVE X'13'        TO     WRK-CARACTER (WRK-IND)
                   WHEN 'ª'
                       MOVE X'14'        TO     WRK-CARACTER (WRK-IND)
                   WHEN '°'
                       MOVE X'A1'        TO     WRK-CARACTER (WRK-IND)
                END-EVALUATE
           END-PERFORM.

           MOVE WRK-TEXTO          TO   WRK-LINHA-EXTR.

      *---------------------------------------------------------------*
       6010-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

       6200-GRAVA-ARQSAI             SECTION.
      *---------------------------------------------------------------*

           WRITE FD-REG-ARQSAI   FROM WRK-ARQ.

           MOVE  WRK-GRAVACAO    TO   WRK-OPERACAO.
           PERFORM   1400-TESTAR-FS-ARQSAI.

           ADD       1           TO   WRK-ACU-GRAV-ARQSAI.

      *---------------------------------------------------------------*
       6200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7000-FINALIZA                   SECTION.
      *---------------------------------------------------------------*

           PERFORM  7100-EXIBIR-DISP-ESTATIST.

           CLOSE  ARQENTRA

           MOVE    WRK-FECHAMENTO          TO   WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           STOP RUN.

      *---------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7100-EXIBIR-DISP-ESTATIST    SECTION.
      *---------------------------------------------------------------*

           DISPLAY '*************************************************'
           DISPLAY '                PROGRAMA GFCT2895'
           DISPLAY '*************************************************'
           DISPLAY '  LIDOS ARQENTRA : ' WRK-ACU-LIDOS-ARQENTRA
           DISPLAY '  GRAVADOS ARQSAI: ' WRK-ACU-GRAV-ARQSAI
           DISPLAY '*************************************************'.

       7100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-ROTINA-ERRO               SECTION.
      *---------------------------------------------------------------*

           MOVE  'GFCT2895'            TO      ERR-PGM.

           CALL  'POOL7100'            USING   WRK-BATCH
                                               ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*



