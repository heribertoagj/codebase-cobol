      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     BVVEA043.
       AUTHOR.         FRANCISMARA.
      *================================================================*
      *                         W I P R O                              *
      *----------------------------------------------------------------*
      *    ANALISTA....: FRANCISMARA          -   WIPRO                *
      *    DATA........: SETEMBRO/2020                                 *
      *----------------------------------------------------------------*
      *    OBJETIVO....: IDENTIFICA CLIENTES COM TRAG NO CADASTRO      *
      *                  DO OPT-IN                                     *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME        IN/OUT      INCLUDE               *
      *                ECTRLAGE      INPUT       I#BVVEFL              *
      *                EARQTRAG      INPUT       WORK                  *
      *                SARQTRAG      OUTPUT      I#BVVEFQ              *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *     I#BRAD7C - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     BRAD7600 - TRATAMENTO DE DATA E HORA CORRENTE              *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS          COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT  ECTRLAGE    ASSIGN  TO          UT-S-ECTRLAGE
                       FILE    STATUS  IS          WRK-FS-ECTRLAGE.

           SELECT  EARQTRAG    ASSIGN  TO          UT-S-EARQTRAG
                       FILE    STATUS  IS          WRK-FS-EARQTRAG.

           SELECT  SARQTRAG    ASSIGN  TO          UT-S-SARQTRAG
                       FILE    STATUS  IS          WRK-FS-SARQTRAG.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE BAIXA DA TABELA BVVEB045                 *
      *----------------------------------------------------------------*
       FD  ECTRLAGE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ECTRLAGE                 PIC  X(270).

      *----------------------------------------------------------------*
      *    BASE COM TRAG VALIDOS                                       *
      *----------------------------------------------------------------*
       FD  EARQTRAG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EARQTRAG                 PIC  X(050).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO PARA ATUALIZACAO DE AGENCIA/CONTA           *
      *----------------------------------------------------------------*
       FD  SARQTRAG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SARQTRAG                 PIC  X(100).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '*** BVVEA043 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*
       77  ACU-LIDOS-ECTRLAGE          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-EARQTRAG          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SARQTRAG          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DE FILE-STATUS ***'.
      *----------------------------------------------------------------*
       77  WRK-FS-ECTRLAGE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-EARQTRAG             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SARQTRAG             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

       01  WRK-S9-5                    PIC +9(05)          VALUE ZEROS.
       01  FILLER REDEFINES WRK-S9-5.
           03 FILLER                   PIC  X(01).
           03 WRK-9-5                  PIC  9(05).

       01  WRK-S9-13                   PIC +9(13)          VALUE ZEROS.
       01  FILLER REDEFINES WRK-S9-13.
           03 FILLER                   PIC  X(01).
           03 FILLER                   PIC  X(06).
           03 WRK-9-7                  PIC  9(07).

       01  WRK-CHAVE-EARQTRAG.
           05  WRK-CH-TRAG-AGENCIA     PIC  9(05)          VALUE ZEROS.
           05  WRK-CH-TRAG-CONTA       PIC  9(07)          VALUE ZEROS.

       01  WRK-CHAVE-OPTIN.
           05  WRK-CH-AGENC-OPT        PIC  9(05)          VALUE ZEROS.
           05  WRK-CH-CONTA-OPT        PIC  9(07)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(055)         VALUE
           '*** AREA DO ARQUIVO ECTRLAGE***'.
      *----------------------------------------------------------------*

            COPY 'I#BVVEFL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO ARQUIVO EARQTRAG '.
      *----------------------------------------------------------------*

       01  WRK-REG-EARQTRAG.
           05  WRK-TRAG-ANTIGA.
               10  WRK-TRAG-AGEVTRAN   PIC  9(05)  COMP-3  VALUE ZEROS.
               10  WRK-TRAG-RAZVTRAN   PIC  9(05)  COMP-3  VALUE ZEROS.
               10  WRK-TRAG-CTAVTRAN   PIC  9(07)  COMP-3  VALUE ZEROS.
               10  WRK-TRAG-DIGVTRAN   PIC  X(01)          VALUE SPACES.
           05  WRK-TRAG-NOVA.
               10  WRK-TRAG-AGENTRAN   PIC  9(05)  COMP-3  VALUE ZEROS.
               10  WRK-TRAG-RAZNTRAN   PIC  9(05)  COMP-3  VALUE ZEROS.
               10  WRK-TRAG-CTANTRAN   PIC  9(07)  COMP-3  VALUE ZEROS.
               10  WRK-TRAG-DIGNTRAN   PIC  X(01)          VALUE SPACES.
           05  FILLER                  PIC  X(28)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(055)         VALUE
           '*** AREA DO ARQUIVO EARQTRAG***'.
      *----------------------------------------------------------------*

            COPY 'I#BVVEFQ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO - FILE-STATUS ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(018)         VALUE
               ' - FILE STATUS =  '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVEA043 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE  DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR
             UNTIL WRK-FS-ECTRLAGE     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ECTRLAGE
                      EARQTRAG
               OUTPUT SARQTRAG.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO
           PERFORM 1100-TESTAR-FILE-STATUS
           .
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ECTRLAGE

           PERFORM 1120-TESTAR-FS-EARQTRAG

           PERFORM 1140-TESTAR-FS-SARQTRAG
           .
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-ECTRLAGE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ECTRLAGE         NOT EQUAL   ZEROS
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  WRK-FS-ECTRLAGE   TO  WRK-FILE-STATUS
               MOVE 'ECTRLAGE'         TO  WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF
           .
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-EARQTRAG         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EARQTRAG         NOT EQUAL   ZEROS
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  WRK-FS-EARQTRAG   TO  WRK-FILE-STATUS
               MOVE 'EARQTRAG'         TO  WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF
           .
      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1140-TESTAR-FS-SARQTRAG         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SARQTRAG         NOT EQUAL   ZEROS
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  WRK-FS-SARQTRAG   TO  WRK-FILE-STATUS
               MOVE 'SARQTRAG'         TO  WRK-NOME-ARQUIVO
               MOVE  WRK-ERRO-ARQUIVO  TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF
           .
      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ECTRLAGE

           IF  WRK-FS-ECTRLAGE         EQUAL '10'
               DISPLAY '**************** BVVEA043 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*         ARQUIVO ECTRLAGE VAZIO          *'
               DISPLAY '*                                        *'
               DISPLAY '**************** BVVEA043 ****************'
           END-IF

           PERFORM 2200-LER-EARQTRAG

           IF  WRK-FS-EARQTRAG         EQUAL '10'
               DISPLAY '**************** BVVEA043 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*         ARQUIVO EARQTRAG VAZIO         *'
               DISPLAY '*                                        *'
               DISPLAY '**************** BVVEA043 ****************'
           END-IF
           .
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-ECTRLAGE               SECTION.
      *----------------------------------------------------------------*

           READ ECTRLAGE               INTO BVVEFL-REG.

           IF  WRK-FS-ECTRLAGE         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-OPTIN
               GO                      TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-ECTRLAGE

           ADD  1                      TO ACU-LIDOS-ECTRLAGE

           IF  BVVEFL-CAG-BCRIA        EQUAL ZEROS OR
               BVVEFL-CCTA-BCRIA-CLI-N EQUAL '?'
               GO                      TO 2100-LER-ECTRLAGE
           END-IF.

           MOVE BVVEFL-CAG-BCRIA       TO WRK-S9-5
           MOVE WRK-9-5                TO WRK-CH-AGENC-OPT
           MOVE BVVEFL-CCTA-BCRIA-CLI  TO WRK-S9-13
           MOVE WRK-9-7                TO WRK-CH-CONTA-OPT
           .
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-EARQTRAG               SECTION.
      *----------------------------------------------------------------*

           READ EARQTRAG               INTO WRK-REG-EARQTRAG.

           IF  WRK-FS-EARQTRAG         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-EARQTRAG
               GO                      TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-EARQTRAG.

           ADD  1                      TO ACU-LIDOS-EARQTRAG.

           MOVE WRK-TRAG-AGEVTRAN      TO WRK-CH-TRAG-AGENCIA
           MOVE WRK-TRAG-CTAVTRAN      TO WRK-CH-TRAG-CONTA
           .
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-EARQTRAG         EQUAL WRK-CHAVE-OPTIN
               PERFORM 3100-TRATAR-IGUAL
               PERFORM 2100-LER-ECTRLAGE
           ELSE
             IF  WRK-CHAVE-OPTIN     LESS WRK-CHAVE-EARQTRAG
                 PERFORM 2100-LER-ECTRLAGE
             ELSE
                 PERFORM 2200-LER-EARQTRAG
             END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-TRATAR-IGUAL               SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEFL-NCTRL-AUTRZ-AGNDA
                                       TO BVVEFQ-NCTRL-AUTRZ-AGNDA
           MOVE WRK-TRAG-AGENTRAN      TO BVVEFQ-AGENCIA-NOVA
           MOVE WRK-TRAG-CTANTRAN      TO BVVEFQ-CONTA-NOVA
           MOVE WRK-TRAG-AGEVTRAN      TO BVVEFQ-AGENCIA-VELHA
           MOVE WRK-TRAG-CTAVTRAN      TO BVVEFQ-CONTA-VELHA

           IF  BVVEFL-CBANDE-VISAO-N   EQUAL '?'
               MOVE ZEROS              TO BVVEFQ-CBANDE-VISAO
           ELSE
               MOVE BVVEFL-CBANDE-VISAO
                                       TO BVVEFQ-CBANDE-VISAO
           END-IF

           IF  BVVEFL-CCRDRA-ESTBL-COML-N EQUAL '?'
               MOVE ZEROS              TO BVVEFQ-CCRDRA-ESTBL-COML
           ELSE
               MOVE BVVEFL-CCRDRA-ESTBL-COML
                                       TO BVVEFQ-CCRDRA-ESTBL-COML
           END-IF

FS2511     IF  BVVEFL-CCNPJ-CPF-AUTRZ EQUAL SPACES OR
FS2511         BVVEFL-CCNPJ-CPF-AUTRZ EQUAL LOW-VALUES
               MOVE SPACES             TO BVVEFQ-CCNPJ-CPF-AUTRZ
           ELSE
               MOVE BVVEFL-CCNPJ-CPF-AUTRZ
                                       TO BVVEFQ-CCNPJ-CPF-AUTRZ
           END-IF

FS2511     IF  BVVEFL-CFLIAL-CNPJ-AUTRZ EQUAL SPACES OR
FS2511         BVVEFL-CFLIAL-CNPJ-AUTRZ EQUAL LOW-VALUES
               MOVE SPACES             TO BVVEFQ-CFLIAL-CNPJ-AUTRZ
           ELSE
               MOVE BVVEFL-CFLIAL-CNPJ-AUTRZ
                                       TO BVVEFQ-CFLIAL-CNPJ-AUTRZ
           END-IF

           IF  BVVEFL-CCTRL-CPF-AUTRZ-N   EQUAL '?'
               MOVE ZEROS              TO BVVEFQ-CCTRL-CPF-AUTRZ
           ELSE
               MOVE BVVEFL-CCTRL-CPF-AUTRZ
                                       TO BVVEFQ-CCTRL-CPF-AUTRZ
           END-IF

           PERFORM 3300-GRAVAR-SARQTRAG
           .
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-GRAVAR-SARQTRAG            SECTION.
      *----------------------------------------------------------------*


           WRITE FD-SARQTRAG           FROM REG-BVVEFQ.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1140-TESTAR-FS-SARQTRAG.

           ADD 1                       TO ACU-GRAVA-SARQTRAG.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 5000-EMITIR-DISPLAY-FINAL.

           CLOSE   ECTRLAGE
                   EARQTRAG
                   SARQTRAG

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-EMITIR-DISPLAY-FINAL       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************* BVVEA043 ******************'.
           DISPLAY '*                                             *'.

           MOVE ACU-LIDOS-ECTRLAGE     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS ECTRLAGE   :   ' WRK-MASCARA
                                                               '  *'.
           MOVE ACU-LIDOS-EARQTRAG     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS EARQTRAG   :   ' WRK-MASCARA
                                                               '  *'.
           DISPLAY '*                                             *'.
           MOVE ACU-GRAVA-SARQTRAG     TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS SARQTRAG:   ' WRK-MASCARA
                                                               '  *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* BVVEA043 ******************'.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVEA043'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
