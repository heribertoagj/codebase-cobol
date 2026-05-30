      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVEA142
       AUTHOR.     FRANCISMARA.
      *================================================================*
      *                     W I P R O  S / A                           *
      *================================================================*
      *    PROGRAMA....:   BVVEA142                                    *
      *    PROGRAMADOR.:   FRANCISMARA               - WIPRO           *
      *    DATA........:   AGOSTO/2020                                 *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR ARQUIVO ARRC022 - GRANDES VOLUMES     *
      *                    ACEITE INCONDICIONAL = NAO                  *
      *  --------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/0           INCLUDE/BOOK     *
      *                EARQSOLI         INPUT           I#BVVEC4       *
      *                ARQARRC          OUTPUT          I#BVVEMB       *
      *                CONTSEQE         INPUT           WORK           *
      *                CONTSEQS         OUTPUT          WORK           *
      *                EARQDATA         INPUT           WORK           *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *            DB2                                                 *
      *              TABLE                            INCLUDE/BOOK     *
      *                DB2PRD.TCTRL_MSGRA_CIPAG        BVVEB054        *
      *                DB2PRD.TPRODT_AGNDA_FINCR       BVVEB048        *
      *                DB2PRD.TBANDE_VISAO_EXTER       BVVEB044        *
      *----------------------------------------------------------------*
      *    BOOKS .:                                                    *
      *    I#BVVEN6 - LAYOUT DA ARRC022                                *
      *    I#CKRS01 - AREA DE CONTROLE DE COMMIT/RESTART               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERROS PELA BRAD7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CKRS1000 - IDENTIFICA DINAMICAMENTE TIPO DE CONEXAO USADO   *
      *    CKRS0100 - TRATAMENTO DE COMMIT/RESTART                     *
      *    BRAD7100 - TRATAMENTO DE ERRO                               *
      *=============================================================== *

      *=============================================================== *
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

      *---------------------------------------------------------------*
       FILE-CONTROL.
      *---------------------------------------------------------------*

           SELECT  EARQSOLI ASSIGN TO UT-S-EARQSOLI
                      FILE  STATUS IS WRK-FS-EARQSOLI.

           SELECT  ARQARRC  ASSIGN TO UT-S-ARQARRC
                      FILE  STATUS IS WRK-FS-ARQARRC.

           SELECT  CONTSEQE ASSIGN TO UT-S-CONTSEQE
                      FILE STATUS IS WRK-FS-CONTSEQE.

           SELECT  CONTSEQS ASSIGN TO UT-S-CONTSEQS
                      FILE STATUS  IS WRK-FS-CONTSEQS.

           SELECT EARQDATA ASSIGN  TO UT-S-EARQDATA
                      FILE STATUS  IS WRK-FS-EARQDATA.

      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT:     ARQUIVO DE SOLICITACAO AUTORIZACAO              *
      *               ORG. SEQUENCIAL   -   LRECL = 300               *
      *---------------------------------------------------------------*

       FD  EARQSOLI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EARQSOLI                 PIC  X(300).

      *---------------------------------------------------------------*
      *    OUTPUT:    ARQUIVO DE TRANSFERENCIA DA IF PARA SAR         *
      *               ORG. SEQUENCIAL   -   LRECL = 32000             *
      *---------------------------------------------------------------*

       FD  ARQARRC
           RECORD IS VARYING DEPENDING ON WRK-TAMANHO
           LABEL RECORD IS STANDARD
           RECORDING MODE IS V.

       01  FD-ARQARRC-MENOR           PIC  X(01).
       01  FD-ARQARRC                 PIC  X(32000).

      *---------------------------------------------------------------*
      *    INPUT:     ARQUIVO DE CONTROLE DE SEQUENCIA                *
      *               ORG. SEQUENCIAL   -   LRECL = 080               *
      *---------------------------------------------------------------*

       FD  CONTSEQE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-CONTSEQE                 PIC  X(080).

      *---------------------------------------------------------------*
      *    SAIDA:     ARQUIVO DE CONTROLE DE SEQUENCIA                *
      *               ORG. SEQUENCIAL   -   LRECL = 080               *
      *---------------------------------------------------------------*

       FD  CONTSEQS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-CONTSEQS                 PIC  X(080).

      *---------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE DATAS                                   *
      *            ORG. SEQUENCIAL   -   LRECL = 050                  *
      *---------------------------------------------------------------*

       FD  EARQDATA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EARQDATA               PIC X(050).

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVEA142 - INICIO DA AREA DE WORKING ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *---------------------------------------------------------------*

       01  ACU-MULADORES.
           03  ACU-LID-EARQSOLI        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-ARQARRC         PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LID-CONTSEQE        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-CONTSEQS        PIC  9(09) COMP-3   VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE OPERACAO DE ARQUIVOS ***'.
      *---------------------------------------------------------------*

       01  WRK-FS-EARQSOLI             PIC  X(02) VALUE SPACES.
       01  WRK-FS-ARQARRC              PIC  X(02) VALUE SPACES.
       01  WRK-FS-CONTSEQE             PIC  X(02) VALUE SPACES.
       01  WRK-FS-CONTSEQS             PIC  X(02) VALUE SPACES.
       01  WRK-FS-EARQDATA             PIC  X(02) VALUE SPACES.
       01  WRK-ABERTURA                PIC  X(13) VALUE ' NA ABERTURA '.
       01  WRK-GRAVACAO                PIC  X(13) VALUE ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
       01  WRK-LEITURA                 PIC  X(13) VALUE ' NA LEITURA  '.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)      VALUE
           '*** AREAS AUXILIARES ***'.
      *---------------------------------------------------------------*
       77  WRK-CONTADOR-ARQUIVO        PIC  9(005) COMP-3 VALUE ZEROS.
       01  WRK-TAMANHO                 PIC  9(005)     VALUE 0 COMP.
       01  WRK-POSICAO                 PIC  9(005) COMP-3
                                                       VALUE ZEROS.
       01  WRK-AUX                     PIC  9(005) COMP-3
                                                       VALUE ZEROS.
       01  WRK-REG-ARQUIVO             PIC  X(31998)   VALUE SPACES.
       01  WRK-BATCH                   PIC  X(008)     VALUE 'BATCH'.
       01  WRK-MODULO                  PIC  X(008)     VALUE SPACES.
       01  WRK-NUMSEQ                  PIC  9(005) COMP-3 VALUE ZEROS.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-COD-OPER-ANT            PIC  X(19)       VALUE SPACES.
       77  WRK-COD-OPER-ATU            PIC  X(19)       VALUE SPACES.
       77  WRK-TITULAR-ATU             PIC  X(14)       VALUE SPACES.
       77  WRK-TITULAR-ANT             PIC  X(14)       VALUE SPACES.

       77  WRK-CKRS0100                PIC X(08)        VALUE
           'CKRS0100'.

       01  WRK-COD-MSG                 PIC  X(007)      VALUE 'ARRC022'.

       01  WRK-NOME-DO-ARQUIVO         PIC X(31)        VALUE SPACES.
       01  FILLER                      REDEFINES WRK-NOME-DO-ARQUIVO.
           10 WRK-NARQ-NOME            PIC X(07).
           10 WRK-NARQ-UN1             PIC X(01).
           10 WRK-NARQ-ISPBIF          PIC X(08).
           10 WRK-NARQ-UN2             PIC X(01).
           10 WRK-NARQ-DTMOVTO         PIC 9(08).
           10 WRK-NARQ-UN3             PIC X(01).
           10 WRK-NARQ-NUMSEQ          PIC 9(05).

090621 01  WRK-SEQUENCIA               PIC S9(008) COMP-3  VALUE ZEROS.
090621
       01    WRK-S9-9                  PIC +9(09)       VALUE ZEROS.
       01    FILLER                    REDEFINES WRK-S9-9.
             05 FILLER                 PIC  9(01).
             05 WRK-9-9                PIC  9(09).

       01    WRK-DTMOVTO               PIC  9(09)       VALUE ZEROS.
       01    FILLER                    REDEFINES WRK-DTMOVTO.
             05 FILLER                 PIC  9(01).
             05 WRK-DTMOVTO-R          PIC  9(08).

       01  WRK-ALFA-8.
           05 WRK-NUM-8                PIC  9(008)        VALUE ZEROS.

       01  WRK-CONTR-IF.
           05  WRK-IF-CCTRO-CUSTO      PIC  X(04)       VALUE SPACES.
           05  WRK-IF-DATA             PIC  X(08)       VALUE SPACES.
           05  WRK-IF-SEQ              PIC  9(08)       VALUE ZEROS.

       01  WRK-NUM-DEC                 PIC  9(15)V99    VALUE ZEROS.
       01  WRK-NUM-DEC-R REDEFINES WRK-NUM-DEC.
           03  WRK-INT                 PIC  9(15).
           03  WRK-DEC                 PIC  9(02).

       01  WRK-NUM-EDIT.
           03  WRK-SINAL               PIC  X(01)        VALUE '+'.
           03  WRK-ZEROS               PIC  X(02)        VALUE '00'.
           03  WRK-INT                 PIC  9(15)        VALUE ZEROS.
           03  WRK-DEC                 PIC  9(02)        VALUE ZEROS.

       01  WRK-CAMPOS-NULOS.
           05 WRK-N-HENVIO-MSGEM-CIPAG PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-N-HRETOR-MSGEM-CIPAG PIC S9(004) COMP    VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(32)       VALUE
           '*   AREA MENSAGENS DE ERRO     *'.
      *---------------------------------------------------------------*

       01  WRK-MSG-ERRO.
          03  FILLER                   PIC X(07)   VALUE SPACES.
          03  FILLER                   PIC X(07)   VALUE
              '* ERRO '.
          03  WRK-OPERACAO             PIC X(13)   VALUE SPACES.
          03  FILLER                   PIC X(12)   VALUE
              ' DO ARQUIVO '.
          03  WRK-NOME-ARQ             PIC X(08)   VALUE SPACES.
          03  FILLER                   PIC X(17)   VALUE
              ' - FILE-STATUS = '.
          03  WRK-FILE-STATUS          PIC X(02)   VALUE SPACES.
          03  FILLER                   PIC X(02)   VALUE ' *'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*** AREA PARA ERRO CKRS0100 ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MENSAGEM-01         PIC X(075)          VALUE
               'CKRS0100 - ERRO GRAVE - RETURN <> DE ZEROS E 8'.
           03  WRK-MENSAGEM-02         PIC X(075)          VALUE
               'CKRS0100 - CODIGO DE FUNCAO INCORRETO'.
           03  WRK-MENSAGEM-03         PIC X(075)          VALUE
               'CKRS0100 - JA FOI FEITA UMA CHAMADA EM MODO INITIALIZE'.
           03  WRK-MENSAGEM-04         PIC X(075)          VALUE
               'CKRS0100 - NAO FOI EFETUADA CHAMADA EM MODO INITIALIZE'.
           03  WRK-MENSAGEM-05         PIC X(075)          VALUE
               'CKRS0100 - CAMPO DB2-ID DEVE ESTAR INFORMADO'.
           03  WRK-MENSAGEM-06         PIC X(075)          VALUE
               'CKRS0100 - O PROCESSO NAO SE ENCONTRA CADATRADO'.
           03  WRK-MENSAGEM-07         PIC X(075)          VALUE
               'CKRS0100 - ERRO NAO IDENTIFICADO'.

       01  WRK-LINHA-PROGRAMA.
           03  FILLER                  PIC  X(038)         VALUE
               '************** BVVEA142 **************'.

       01  WRK-LINHA-BRANCO.
           03  FILLER                  PIC  X(038)         VALUE
               '*                                    *'.

       01  WRK-LINHA-RETORNO.
           03  FILLER                  PIC  X(024)         VALUE
               '* CK01-CODIGO-RETORNO : '.
           03  WRK-CODIGO-RETORNO      PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(013)         VALUE
               '           *'.

       01  WRK-LINHA-MENSAGEM.
           03  FILLER                  PIC  X(024)         VALUE
               '* CK01-CODIGO-MENSAGEM: '.
           03  WRK-CODIGO-MENSAGEM     PIC  X(008)         VALUE ZEROS.
           03  FILLER                  PIC  X(008)         VALUE
               '       *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)        VALUE
           '* AREA DO CKRS0100 *'.
      *----------------------------------------------------------------*

       COPY 'I#CKRS01'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)        VALUE
           '* AREA PARA HEADER *'.
      *----------------------------------------------------------------*

       COPY 'BVVEW500'.

      *---------------------------------------------------------------*
      *    '*** AREA DA BRAD7600 ***'.
      *---------------------------------------------------------------*

       01    WRK-DATA-HORA.
         05  WRK-DT-JULIANA            PIC  9(05)  COMP-3 VALUE ZEROS.
         05  WRK-DT-AAMMDD             PIC  9(07)  COMP-3 VALUE ZEROS.
         05  WRK-DT-AAAAMMDD           PIC  9(09)  COMP-3 VALUE ZEROS.
         05  WRK-TI-HHMMSS             PIC  9(07)  COMP-3 VALUE ZEROS.
         05  WRK-TI-HHMMSSMMMMMM       PIC  9(13)  COMP-3 VALUE ZEROS.
         05  WRK-TIMESTAMP.
           07 WRK-TS-AAAA              PIC  9(04)         VALUE ZEROS.
           07 WRK-TS-MM                PIC  9(02)         VALUE ZEROS.
           07 WRK-TS-DD                PIC  9(02)         VALUE ZEROS.
           07 WRK-TS-HH                PIC  9(02)         VALUE ZEROS.
           07 WRK-TS-MN                PIC  9(02)         VALUE ZEROS.
           07 WRK-TS-SS                PIC  9(02)         VALUE ZEROS.
           07 WRK-TS-MMM               PIC  9(06)         VALUE ZEROS.

       01  WRK-TIME-STAMP.
           03 WRK-TS-AAAA              PIC  9(04)         VALUE ZEROS.
           03 FILLER                   PIC  X(01)         VALUE '-'.
           03 WRK-TS-MM                PIC  9(02)         VALUE ZEROS.
           03 FILLER                   PIC  X(01)         VALUE '-'.
           03 WRK-TS-DD                PIC  9(02)         VALUE ZEROS.
           03 FILLER                   PIC  X(01)         VALUE '-'.
           03 WRK-TS-HH                PIC  9(02)         VALUE ZEROS.
           03 FILLER                   PIC  X(01)         VALUE '.'.
           03 WRK-TS-MN                PIC  9(02)         VALUE ZEROS.
           03 FILLER                   PIC  X(01)         VALUE '.'.
           03 WRK-TS-SS                PIC  9(02)         VALUE ZEROS.
           03 FILLER                   PIC  X(01)         VALUE '.'.
           03 WRK-TS-MMM               PIC  9(06)         VALUE ZEROS.

       01  WRK-DATA-CORRENTE           PIC  X(10)         VALUE SPACES.

       01  WRK-DATA-DB2.
           03 WRK-TS-DD                PIC  9(02)         VALUE ZEROS.
           03 FILLER                   PIC  X(01)         VALUE '.'.
           03 WRK-TS-MM                PIC  9(02)         VALUE ZEROS.
           03 FILLER                   PIC  X(01)         VALUE '.'.
           03 WRK-TS-AAAA              PIC  9(04)         VALUE ZEROS.
       01  WRK-DATA-DB2-R REDEFINES
           WRK-DATA-DB2                PIC  X(10).

       01  WRK-DATA-CIP.
           03 WRK-TS-AAAA              PIC  9(04)         VALUE ZEROS.
           03 WRK-TS-MM                PIC  9(02)         VALUE ZEROS.
           03 WRK-TS-DD                PIC  9(02)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA BRAD0431 *'.
      *----------------------------------------------------------------*
       01  WRK-BRAD0431.
           05  WRK-DIGITO-0431         PIC  X(001)         VALUE SPACES.
           05  WRK-TAMANHO-0431        PIC  9(002)         VALUE ZEROS.

       01  WRK-CONTA-AUX               PIC  9(013)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)         VALUE
           '*  AREA P/ CONTSEQE E CONTSEQS*'.
      *----------------------------------------------------------------*

       01  WRK-REG-CONTSEQ.
           03  WRK-SEQ-ROTINA          PIC X(004)      VALUE SPACES.
           03  WRK-SEQ-ORIGEM          PIC X(001)      VALUE SPACES.
           03  WRK-SEQ-ARQUIVO         PIC X(007)      VALUE SPACES.
           03  WRK-SEQ-DTMOVTO         PIC 9(008)      VALUE ZEROS.
           03  WRK-SEQ-NUMSEQ          PIC 9(005)      VALUE ZEROS.
           03  WRK-SEQ-TIMESTAMP       PIC X(026)      VALUE SPACES.
           03  WRK-SEQ-NUMCTIF         PIC X(020)      VALUE SPACES.
           03  FILLER                  PIC X(009)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(055)         VALUE
          '*** AREA DO ARQUIVO ARQDATA  ***'.
      *---------------------------------------------------------------

       01  WRK-ARQDATA.
          03 WRK-ARQ-DT-MOVTO-X.
             05 WRK-ARQ-DT-MOVTO   PIC  9(008)        VALUE ZEROS.
          03 FILLER                PIC  X(042)        VALUE SPACES.

      *---FORMATO DE WRK-ARQDATA - AAAAMMDD ---------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(46)         VALUE
           '* LAYOUT PARA O ARQUIVO EARQSOLI *'.
      *----------------------------------------------------------------*

       COPY 'I#BVVEC4'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)         VALUE
           '* LAYOUT PARA O ARQ ARRC022       *'.
      *----------------------------------------------------------------*
       COPY 'I#BVVEN6'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(36)         VALUE
           '* LAYOUT SAIDA P/ MSG ARRC022     *'.
      *----------------------------------------------------------------*
       COPY 'I#BVVEMB'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(36)         VALUE
           '* AREA DE COMUNICACAO SIMM8202 *'.
      *----------------------------------------------------------------*

       COPY 'I#SIMM09'.

       01  WRK-COMU-POL7100               PIC  X(107) VALUE SPACES.
       01  WRK-COMU-SQLCA                 PIC  X(136) VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)      VALUE
           '*** AREA DA BRAD7100 ***'.
      *---------------------------------------------------------------*
       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DB2                                   ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE BVVEB054
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING BVVEA142 ***'.
      *---------------------------------------------------------------*
           EJECT
      *===============================================================*
       PROCEDURE                       DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM 1000-INICIAR-PROGRAMA.

           PERFORM 2000-VER-VAZIO.

           PERFORM 3000-PROCESSAR     UNTIL
                   WRK-FS-EARQSOLI    EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIAR-PROGRAMA           SECTION.
      *---------------------------------------------------------------*


           OPEN INPUT  EARQSOLI
                       CONTSEQE
                       EARQDATA
               OUTPUT  ARQARRC
                       CONTSEQS.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.
           PERFORM 1300-TRATAR-CHECKPOINT-RESTART.

           PERFORM 1400-OBTER-DATA-CORRENTE.

           PERFORM 1700-OBTER-COD-ISPB.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 1210-TESTAR-FS-EARQSOLI.

           PERFORM 1220-TESTAR-FS-ARQARRC.

           PERFORM 1250-TESTAR-FS-EARQDATA.

      *---------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1210-TESTAR-FS-EARQSOLI         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-EARQSOLI         NOT EQUAL '00'
               MOVE 'EARQSOLI'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EARQSOLI    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1220-TESTAR-FS-ARQARRC         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQARRC         NOT EQUAL '00'
               MOVE 'ARQARRC'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQARRC    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO      TO ERR-TEXTO
               MOVE 'APL'             TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1230-TESTAR-FS-CONTSEQE         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-CONTSEQE         NOT EQUAL '00'
               MOVE 'CONTSEQE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CONTSEQE    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1240-TESTAR-FS-CONTSEQS         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-CONTSEQS         NOT EQUAL '00'
               MOVE 'CONTSEQS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CONTSEQS    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1250-TESTAR-FS-EARQDATA        SECTION.
      *---------------------------------------------------------------*


           IF  WRK-FS-EARQDATA         NOT EQUAL '00'
               MOVE 'EARQDATA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EARQDATA    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.


      *---------------------------------------------------------------*
       1250-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-TRATAR-CHECKPOINT-RESTART  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2'                  TO CK01-ID-DB2.
           MOVE 'I'                    TO CK01-FUNCAO.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 1310-TRATAR-ERRO-RESTART
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TRATA ERRO NO ACESSO AO MODULO CKRS0100                     *
      *----------------------------------------------------------------*
       1310-TRATAR-ERRO-RESTART        SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                    TO ERR-TIPO-ACESSO.

           IF  CK01-CODIGO-RETORNO       NOT EQUAL 8
               MOVE WRK-MENSAGEM-01      TO ERR-TEXTO
               MOVE CK01-CODIGO-RETORNO  TO WRK-CODIGO-RETORNO
               MOVE CK01-CODIGO-MENSAGEM TO WRK-CODIGO-MENSAGEM
               DISPLAY WRK-LINHA-PROGRAMA
               DISPLAY WRK-LINHA-BRANCO
               DISPLAY WRK-LINHA-RETORNO
               DISPLAY WRK-LINHA-MENSAGEM
               DISPLAY WRK-LINHA-BRANCO
               DISPLAY WRK-LINHA-PROGRAMA
               PERFORM 9999-ROTINA-ERRO
            END-IF.

            EVALUATE CK01-CODIGO-MENSAGEM
                WHEN 'CKRS0001'
                      MOVE WRK-MENSAGEM-02 TO ERR-TEXTO
                WHEN 'CKRS0002'
                      MOVE WRK-MENSAGEM-03 TO ERR-TEXTO
                WHEN 'CKRS0003'
                      MOVE WRK-MENSAGEM-04 TO ERR-TEXTO
                WHEN 'CKRS0004'
                      MOVE WRK-MENSAGEM-05 TO ERR-TEXTO
                WHEN 'CKRS0005'
                      MOVE WRK-MENSAGEM-06 TO ERR-TEXTO
                WHEN  OTHER
                      MOVE WRK-MENSAGEM-07 TO ERR-TEXTO
                      MOVE CK01-CODIGO-RETORNO  TO WRK-CODIGO-RETORNO
                      MOVE CK01-CODIGO-MENSAGEM TO WRK-CODIGO-MENSAGEM
                      DISPLAY WRK-LINHA-PROGRAMA
                      DISPLAY WRK-LINHA-BRANCO
                      DISPLAY WRK-LINHA-RETORNO
                      DISPLAY WRK-LINHA-MENSAGEM
                      DISPLAY WRK-LINHA-BRANCO
                      DISPLAY WRK-LINHA-PROGRAMA
            END-EVALUATE.

            PERFORM 9999-ROTINA-ERRO.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       1400-OBTER-DATA-CORRENTE        SECTION.
      *---------------------------------------------------------------*

           INITIALIZE                  WRK-DATA-HORA.

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

           MOVE CORR WRK-TIMESTAMP    TO WRK-TIME-STAMP.
           MOVE WRK-DT-AAAAMMDD       TO WRK-DTMOVTO.
           MOVE CORR WRK-TIME-STAMP   TO WRK-DATA-DB2.
           MOVE WRK-DATA-DB2          TO WRK-DATA-CORRENTE.

      *---------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       1700-OBTER-COD-ISPB             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  SIMM09-REGISTRO.

           MOVE 237                     TO SIMM09-BANCO.
           MOVE 'SIMM8202'              TO WRK-MODULO.

           CALL  WRK-MODULO             USING  SIMM09-REGISTRO
                                               WRK-COMU-POL7100
                                               WRK-COMU-SQLCA

           IF  SIMM09-LINK-COD-RETORNO  NOT EQUAL ZEROS
               MOVE WRK-COMU-POL7100    TO ERRO-AREA
           END-IF.

           IF  SIMM09-LINK-COD-RETORNO  NOT EQUAL ZEROS
               IF  SIMM09-LINK-COD-RETORNO       EQUAL 100
                   MOVE 'SIMM8202 - CODIGO BANCO INVALIDO - REMETENTE'
                                                   TO ERR-TEXTO
                   MOVE 'APL'               TO ERR-TIPO-ACESSO
                   PERFORM 9999-ROTINA-ERRO
               ELSE
                   MOVE WRK-COMU-POL7100    TO ERRO-AREA
                   PERFORM 9999-ROTINA-ERRO
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VER-VAZIO                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-EARQSOLI.

           IF  WRK-FS-EARQSOLI         EQUAL '10'
               DISPLAY '*************** BVVEA142 ******************'
               DISPLAY '*                                         *'
               DISPLAY '*    ARQUIVO DE SOLICITACAO AUTORIZACAO   *'
               DISPLAY '*              ESTA VAZIO                 *'
               DISPLAY '*         PROGRAMA ENCERRADO              *'
               DISPLAY '*                                         *'
               DISPLAY '*************** BVVEA142 ******************'
               PERFORM 2200-LER-CONTSEQE
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2300-LER-EARQDATA.
           PERFORM 2200-LER-CONTSEQE.

           IF  WRK-SEQ-DTMOVTO         EQUAL WRK-ARQ-DT-MOVTO
               MOVE WRK-SEQ-NUMSEQ     TO WRK-NUMSEQ
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-EARQSOLI               SECTION.
      *----------------------------------------------------------------*

           READ EARQSOLI               INTO REG-BVVEC4.

           IF  WRK-FS-EARQSOLI         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-COD-OPER-ATU
               GO                      TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-EARQSOLI.

           ADD 1                       TO ACU-LID-EARQSOLI.
           MOVE BVVEC4-COD-OPERACAO    TO WRK-COD-OPER-ATU.
           MOVE BVVEC4-CNPJ-USUAR-FNAL TO WRK-TITULAR-ATU.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-CONTSEQE               SECTION.
      *----------------------------------------------------------------*

           READ CONTSEQE.

           IF  WRK-FS-CONTSEQE         EQUAL '10'
               GO                      TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1230-TESTAR-FS-CONTSEQE

           ADD 1                       TO ACU-LID-CONTSEQE.

           MOVE FD-CONTSEQE            TO  WRK-REG-CONTSEQ.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-LER-EARQDATA               SECTION.
      *----------------------------------------------------------------*

           READ EARQDATA               INTO WRK-ARQDATA.

           IF  WRK-FS-EARQDATA         EQUAL '10'
               GO TO 2300-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1250-TESTAR-FS-EARQDATA.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       2400-GRAVA-CONTSEQS             SECTION.
      *---------------------------------------------------------------*

           WRITE FD-CONTSEQS           FROM  WRK-REG-CONTSEQ.

           PERFORM 1240-TESTAR-FS-CONTSEQS

           ADD 1                       TO ACU-GRA-CONTSEQS.

      *---------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           ADD 1                       TO WRK-NUMSEQ.

           PERFORM 3200-OBTER-NUM-CTRLIF.
           PERFORM 3205-INCLUIR-CTRL.
           PERFORM 3100-GRAVAR-HEADER.

           INITIALIZE WRK-REG-CONTSEQ.
           MOVE 'BVVE'                 TO WRK-SEQ-ROTINA
           MOVE SPACES                 TO WRK-SEQ-ORIGEM
           MOVE 'ARRC022'              TO WRK-SEQ-ARQUIVO
           MOVE WRK-ARQ-DT-MOVTO       TO WRK-SEQ-DTMOVTO
           MOVE WRK-NUMSEQ             TO WRK-SEQ-NUMSEQ
           MOVE WRK-TIME-STAMP         TO WRK-SEQ-TIMESTAMP.
           MOVE WRK-CONTR-IF           TO WRK-SEQ-NUMCTIF.

           MOVE ZEROS                  TO  WRK-CONTADOR-ARQUIVO.
           PERFORM 3300-PROCESSAR-ARQUIVO
                   UNTIL ( WRK-FS-EARQSOLI  EQUAL '10') OR
LOOP               ( WRK-CONTADOR-ARQUIVO   EQUAL 1 ).

           IF  WRK-REG-ARQUIVO         NOT EQUAL SPACES
               MOVE 1                  TO BVVEMB-SEQ-REGISTRO
               MOVE 'N'                TO BVVEMB-IDE-CONTINUIDADE
               ADD  1                  TO WRK-POSICAO
               MOVE WRK-POSICAO        TO WRK-TAMANHO
               MOVE WRK-REG-ARQUIVO    TO BVVEMB-REGISTRO-DDA
               WRITE FD-ARQARRC        FROM REG-BVVEMB
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1220-TESTAR-FS-ARQARRC
               ADD 1                   TO ACU-GRA-ARQARRC
           END-IF.
      *
      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-GRAVAR-HEADER              SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-COD-MSG            TO WRK-NARQ-NOME
           MOVE '_'                    TO WRK-NARQ-UN1
           MOVE SIMM09-CODISPB         TO WRK-NARQ-ISPBIF
           MOVE '_'                    TO WRK-NARQ-UN2
           MOVE WRK-ARQ-DT-MOVTO       TO WRK-NARQ-DTMOVTO
           MOVE '_'                    TO WRK-NARQ-UN2
           MOVE '_'                    TO WRK-NARQ-UN3
           MOVE WRK-NUMSEQ             TO WRK-NARQ-NUMSEQ

           MOVE WRK-NOME-DO-ARQUIVO    TO BVVEW500-NOME-ARQUIVO

           MOVE WRK-CONTR-IF           TO BVVEW500-NRO-CTRL-IF
           MOVE '********************' TO BVVEW500-NRO-CTRL-ORIG
           MOVE SIMM09-CODISPB         TO BVVEW500-ISPB-EMISSOR
           MOVE '29011780'             TO BVVEW500-ISPB-DESTINO

           MOVE WRK-TIME-STAMP         TO BVVEW500-DT-HR
           MOVE '**'                   TO BVVEW500-SIT-REQ
           MOVE WRK-ARQ-DT-MOVTO-X     TO BVVEW500-DATA-MOVIM
           MOVE '*'                    TO BVVEW500-IND-CONTIN

           MOVE BVVEW500-ARRC-HEADER   TO REG-BVVEMB.
           MOVE 143                    TO WRK-TAMANHO.

           WRITE FD-ARQARRC           FROM REG-BVVEMB.
           MOVE WRK-GRAVACAO          TO WRK-OPERACAO.

           PERFORM 1220-TESTAR-FS-ARQARRC.

           INITIALIZE REG-BVVEMB.
           MOVE  SPACES   TO   REG-BVVEMB.

           ADD 1                       TO ACU-GRA-ARQARRC.

      *----------------------------------------------------------------*
       3100-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3110-GRAVAR-SAIDA              SECTION.
      *---------------------------------------------------------------*
      *
           MOVE 1                     TO BVVEMB-SEQ-REGISTRO
           MOVE 'S'                   TO BVVEMB-IDE-CONTINUIDADE
           ADD  1                     TO WRK-POSICAO
           MOVE WRK-POSICAO           TO WRK-TAMANHO.
           MOVE WRK-REG-ARQUIVO       TO BVVEMB-REGISTRO-DDA

           WRITE FD-ARQARRC           FROM REG-BVVEMB
           MOVE WRK-GRAVACAO          TO WRK-OPERACAO
           PERFORM 1220-TESTAR-FS-ARQARRC
           ADD 1                      TO ACU-GRA-ARQARRC
      *
           MOVE  SPACES               TO REG-BVVEMB
           INITIALIZE REG-BVVEMB
           MOVE SPACES                TO WRK-REG-ARQUIVO
           MOVE 1                     TO WRK-POSICAO.

      *---------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-OBTER-NUM-CTRLIF           SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM'                 TO CCTRO-CUSTO OF BVVEB054

090621     EXEC SQL
090621          SET :WRK-SEQUENCIA     =
090621          NEXT VALUE FOR DB2PRD.SEQ_BVVEN054_TCTRL_MSGRA_CIPAG
090621     END-EXEC
090621
090621     IF (SQLCODE                  NOT EQUAL ZEROS
090621     OR  SQLWARN0                 EQUAL 'W')
090621         MOVE 'DB2'               TO ERR-TIPO-ACESSO
090621         MOVE 'SEQ_BVVEN001'      TO ERR-DBD-TAB
090621         MOVE 'NEXT VALUE'        TO ERR-FUN-COMANDO
090621         MOVE SQLCODE             TO ERR-SQL-CODE
090621         MOVE '0999'              TO ERR-LOCAL
090621         MOVE SPACES              TO ERR-SEGM
090621         PERFORM 9999-ROTINA-ERRO
090621     END-IF.
090621
090621     MOVE WRK-SEQUENCIA          TO CSEQ-CTRL-CIPAG OF BVVEB054.
090621
      *----------------------------------------------------------------*
       3200-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3205-INCLUIR-CTRL               SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM'                 TO CCTRO-CUSTO        OF BVVEB054
           MOVE WRK-DATA-CORRENTE      TO DINCL-CTRL-CIPAG   OF BVVEB054
           MOVE '1'                    TO CSIT-PROCM-CIPAG   OF BVVEB054
           MOVE 'ARRC022V'             TO CMSGEM-ENVID-CIPAG OF BVVEB054

           MOVE WRK-NOME-DO-ARQUIVO    TO CPROT-SIST-ORIGE   OF BVVEB054

           MOVE WRK-TIME-STAMP         TO HENVIO-MSGEM-CIPAG OF BVVEB054
           MOVE ZEROS                  TO WRK-N-HENVIO-MSGEM-CIPAG

           MOVE SPACES                 TO HRETOR-MSGEM-CIPAG OF BVVEB054
           MOVE -1                     TO WRK-N-HRETOR-MSGEM-CIPAG
           MOVE 1                      TO CIDTFD-ENTID-REG   OF BVVEB054

           EXEC SQL
             INSERT INTO DB2PRD.TCTRL_MSGRA_CIPAG
               (CCTRO_CUSTO
               ,DINCL_CTRL_CIPAG
               ,CSEQ_CTRL_CIPAG
               ,CSIT_PROCM_CIPAG
               ,CMSGEM_ENVID_CIPAG
               ,CPROT_SIST_ORIGE
               ,HENVIO_MSGEM_CIPAG
               ,HRETOR_MSGEM_CIPAG
               ,CIDTFD_ENTID_REG)
             VALUES
               (:BVVEB054.CCTRO-CUSTO
               ,:BVVEB054.DINCL-CTRL-CIPAG
               ,:BVVEB054.CSEQ-CTRL-CIPAG
               ,:BVVEB054.CSIT-PROCM-CIPAG
               ,:BVVEB054.CMSGEM-ENVID-CIPAG
               ,:BVVEB054.CPROT-SIST-ORIGE
               ,:BVVEB054.HENVIO-MSGEM-CIPAG
                  :WRK-N-HENVIO-MSGEM-CIPAG
               ,:BVVEB054.HRETOR-MSGEM-CIPAG
                  :WRK-N-HRETOR-MSGEM-CIPAG
               ,:BVVEB054.CIDTFD-ENTID-REG)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TCTRL_MSGRA_CIPAG' TO ERR-DBD-TAB
               MOVE 'INSERT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0003'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE CCTRO-CUSTO            OF BVVEB054
                                       TO WRK-IF-CCTRO-CUSTO
           MOVE WRK-DTMOVTO-R          TO WRK-NUM-8
           MOVE WRK-ALFA-8             TO WRK-IF-DATA
           MOVE CSEQ-CTRL-CIPAG        OF BVVEB054
                                       TO WRK-S9-9
           MOVE WRK-9-9(2:8)           TO WRK-IF-SEQ.

      *----------------------------------------------------------------*
       3205-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3210-TRATAR-RESTART             SECTION.
      *---------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2'                  TO CK01-ID-DB2.
           MOVE 'P'                    TO CK01-FUNCAO.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 1310-TRATAR-ERRO-RESTART
           END-IF.

      *---------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3300-PROCESSAR-ARQUIVO          SECTION.
      *---------------------------------------------------------------*

      *    PARTE FIXA - OCORRE APENAS UMA VEZ NO INICIO DO ARQUIVO

           MOVE 1                      TO WRK-POSICAO
           MOVE '{100000}'             TO BVVEN6-CHAVE-GRP-ATLZ-NEGC
           MOVE SIMM09-CODISPB         TO BVVEN6-IDENTD-PART-PRIN
                                          BVVEN6-IDENTD-PART-ADM

           MOVE BVVEN6-GRUPO-ATLZ-NEGC-REC TO WRK-REG-ARQUIVO
                (WRK-POSICAO : LENGTH OF BVVEN6-GRUPO-ATLZ-NEGC-REC)
           COMPUTE WRK-POSICAO         = (WRK-POSICAO +
                                LENGTH OF BVVEN6-GRUPO-ATLZ-NEGC-REC)

           PERFORM 3310-GERAR-GRUPO-OPERACAO UNTIL
                   WRK-FS-EARQSOLI        EQUAL '10' OR
LOOP             ( WRK-CONTADOR-ARQUIVO   EQUAL 1 ).

      *---------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *    GERADO PARA CADA OPERACAO DO ARQUIVO                       *
      *---------------------------------------------------------------*
       3310-GERAR-GRUPO-OPERACAO       SECTION.
      *---------------------------------------------------------------*

           ADD 1                     TO WRK-CONTADOR-ARQUIVO

           INITIALIZE   BVVEN6-DADOS-OPERACAO

           MOVE '{110000}'              TO BVVEN6-CHAVE-GRP-NEGC-REC

           MOVE BVVEC4-COD-OPERACAO     TO BVVEN6-ID-NEGOC-RECBVL
           MOVE BVVEC4-IND-I-A          TO BVVEN6-IND-I-A

           IF  BVVEC4-IND-I-A           EQUAL 'I'
               MOVE ALL '*'             TO BVVEN6-ID-OPER
           ELSE
               MOVE BVVEC4-COD-OPER-CIP TO BVVEN6-ID-OPER
           END-IF.

           MOVE 'TC'                    TO BVVEN6-INDR-TP-NEGC
           MOVE BVVEC4-DT-VCTO-OPER     TO WRK-DATA-DB2
           MOVE CORR WRK-DATA-DB2       TO WRK-DATA-CIP
           MOVE WRK-DATA-CIP            TO BVVEN6-DT-VENC-OP

           MOVE BVVEC4-VLR-TOT-OPER     TO WRK-NUM-DEC
           MOVE CORR WRK-NUM-DEC-R      TO WRK-NUM-EDIT
           MOVE WRK-NUM-EDIT            TO BVVEN6-VLR-TOT-LIM-SLD
           MOVE ALL '*'                 TO BVVEN6-VLR-GAR
           MOVE 'N'                     TO BVVEN6-IND-GEST-ER
           MOVE 'V'                     TO BVVEN6-IND-REGR-DIVS
           MOVE 'E'                     TO BVVEN6-IND-ALC-CONTR-CRED
           MOVE 'N'                     TO BVVEN6-IND-ACTE-INCOND
           MOVE 'N'                     TO BVVEN6-IND-ACTE-UR-RESER
           MOVE ALL '*'                 TO BVVEN6-ID-OP-DESCSTC
           MOVE '*'                     TO BVVEN6-IND-AUT-CESS

           MOVE '{111000}'              TO BVVEN6-CHAVE-GRP-CES-AU
           MOVE ALL '*'                 TO BVVEN6-CNPJ-CPF-CES
           MOVE '{112000}'              TO BVVEN6-CHAVE-GRP-RENEG
           MOVE ALL '*'                 TO BVVEN6-ID-OPER-ORIG

           COMPUTE WRK-AUX              = WRK-POSICAO +
                                        LENGTH OF BVVEN6-DADOS-OPERACAO

           IF  WRK-AUX                  GREATER 31998
               PERFORM 3110-GRAVAR-SAIDA
           END-IF

           MOVE BVVEN6-DADOS-OPERACAO   TO WRK-REG-ARQUIVO
                       (WRK-POSICAO : LENGTH OF BVVEN6-DADOS-OPERACAO)
           ADD  LENGTH OF BVVEN6-DADOS-OPERACAO
                                        TO WRK-POSICAO


           MOVE '{113000}'              TO BVVEN6-CHAVE-GRP-GEST-ER

           COMPUTE WRK-AUX              = WRK-POSICAO +
                                        LENGTH OF BVVEN6-GRUPO-GEST-ER

           IF  WRK-AUX                  GREATER 31998
               PERFORM 3110-GRAVAR-SAIDA
           END-IF

           MOVE BVVEN6-GRUPO-GEST-ER      TO WRK-REG-ARQUIVO
                       (WRK-POSICAO : LENGTH OF BVVEN6-GRUPO-GEST-ER)
           ADD  LENGTH OF BVVEN6-GRUPO-GEST-ER
                                          TO WRK-POSICAO

           MOVE ALL '*'                 TO BVVEN6-DADOS-PRODUTO
           MOVE '{113100}'              TO BVVEN6-CHAVE-GRP-TIT
           MOVE '{113110}'              TO BVVEN6-CHAVE-GRP-CRED
           MOVE '{113120}'              TO BVVEN6-CHAVE-GRP-ARR-PGTO
           MOVE '{113130}'              TO BVVEN6-CHAVE-GRP-USUAR

           COMPUTE WRK-AUX              = WRK-POSICAO +
                                        LENGTH OF BVVEN6-DADOS-PRODUTO

           IF  WRK-AUX                  GREATER 31998
               PERFORM 3110-GRAVAR-SAIDA
           END-IF

           MOVE BVVEN6-DADOS-PRODUTO    TO WRK-REG-ARQUIVO
                         (WRK-POSICAO : LENGTH OF BVVEN6-DADOS-PRODUTO)
           ADD  LENGTH OF BVVEN6-DADOS-PRODUTO
                                        TO WRK-POSICAO

           INITIALIZE BVVEN6-GESTAO-PART.
           MOVE '{114000}'              TO BVVEN6-GRUPO-GEST-PART

           COMPUTE WRK-AUX              = WRK-POSICAO +
                                        LENGTH OF BVVEN6-GRUPO-GEST-PART

           IF  WRK-AUX                  GREATER 31998
               PERFORM 3110-GRAVAR-SAIDA
           END-IF

           MOVE BVVEN6-GRUPO-GEST-PART  TO WRK-REG-ARQUIVO
                        (WRK-POSICAO : LENGTH OF BVVEN6-GRUPO-GEST-PART)
           ADD  LENGTH OF BVVEN6-GRUPO-GEST-PART
                                        TO WRK-POSICAO

           MOVE WRK-COD-OPER-ATU        TO WRK-COD-OPER-ANT
           PERFORM 3311-GERAR-TITULAR
                   UNTIL WRK-COD-OPER-ANT NOT EQUAL WRK-COD-OPER-ATU.

      *---------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3311-GERAR-TITULAR              SECTION.
      *---------------------------------------------------------------*

           MOVE '{114100}'              TO BVVEN6-CHAVE-GRP-TIT-P
           MOVE BVVEC4-CNPJ-USUAR-FNAL  TO BVVEN6-CNPJ-OU-BASE-TIT-P

           COMPUTE WRK-AUX              = WRK-POSICAO +
                                        LENGTH OF BVVEN6-GRUPO-TIT-PART

           IF  WRK-AUX                  GREATER 31998
               PERFORM 3110-GRAVAR-SAIDA
           END-IF

           MOVE BVVEN6-GRUPO-TIT-PART   TO WRK-REG-ARQUIVO
                         (WRK-POSICAO : LENGTH OF BVVEN6-GRUPO-TIT-PART)
           ADD  LENGTH OF BVVEN6-GRUPO-TIT-PART
                                        TO WRK-POSICAO

           MOVE '{114110}'              TO BVVEN6-CHAVE-GRP-DOCML

           MOVE BVVEC4-CPFCNPJ-TTLAR    TO BVVEN6-CNPJ-CPF-TIT-CONTAP
           MOVE SIMM09-CODISPB          TO BVVEN6-ISPB-BCO-RECBD-P
           MOVE BVVEC4-CTPO-CONTA       TO BVVEN6-TP-CTA-P
           MOVE BVVEC4-AGENCIA(2:4)     TO BVVEN6-AGENCIA-P
           MOVE BVVEC4-CONTA(2:12)      TO BVVEN6-NUM-CTA-P

           PERFORM 3313-CALULAR-DIGITO-CTA

           MOVE WRK-DIGITO-0431         TO BVVEN6-DIG-CTA-P
           MOVE ALL '*'                 TO BVVEN6-CONTA-PGTO-P

           COMPUTE WRK-AUX              = WRK-POSICAO +
                                 LENGTH OF BVVEN6-GRUPO-DOMCL-BANC

           IF  WRK-AUX                  GREATER 31998
               PERFORM 3110-GRAVAR-SAIDA
           END-IF

           MOVE BVVEN6-GRUPO-DOMCL-BANC TO WRK-REG-ARQUIVO
                  (WRK-POSICAO : LENGTH OF BVVEN6-GRUPO-DOMCL-BANC)
           ADD  LENGTH OF BVVEN6-GRUPO-DOMCL-BANC
                                        TO WRK-POSICAO

           MOVE WRK-TITULAR-ATU         TO WRK-TITULAR-ANT
           PERFORM 3312-GERAR-PRODUTO
                   UNTIL WRK-COD-OPER-ANT NOT EQUAL WRK-COD-OPER-ATU
                     OR  WRK-TITULAR-ANT  NOT EQUAL WRK-TITULAR-ATU.

      *---------------------------------------------------------------*
       3311-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3312-GERAR-PRODUTO              SECTION.
      *---------------------------------------------------------------*

           MOVE '{114111}'              TO BVVEN6-CHAVE-GRP-UR

           MOVE BVVEC4-CNPJ-CREDENC     TO BVVEN6-CNPJ-CREDEN-P
           MOVE BVVEC4-CNPJ-USUAR-FNAL
                                        TO BVVEN6-CNPJ-USUAR-UR
                                           BVVEN6-CNPJ-TIT-UR

           MOVE BVVEC4-ARRANJO          TO BVVEN6-ARRANJ-PGTO-UR
           MOVE BVVEC4-VLR-OPER-PRODUTO TO WRK-NUM-DEC
           MOVE CORR WRK-NUM-DEC-R      TO WRK-NUM-EDIT
           MOVE WRK-NUM-EDIT            TO BVVEN6-VLR-PERC-UR

           MOVE BVVEC4-DT-UR            TO WRK-DATA-DB2
           MOVE CORR WRK-DATA-DB2       TO WRK-DATA-CIP
           MOVE WRK-DATA-CIP            TO BVVEN6-DT-LIQ-UR

           COMPUTE WRK-AUX              = WRK-POSICAO +
                                        LENGTH OF BVVEN6-GRUPO-UR

           IF  WRK-AUX                  GREATER 31998
               PERFORM 3110-GRAVAR-SAIDA
           END-IF
           MOVE BVVEN6-GRUPO-UR         TO WRK-REG-ARQUIVO
                         (WRK-POSICAO : LENGTH OF BVVEN6-GRUPO-UR)
           ADD  LENGTH OF BVVEN6-GRUPO-UR
                                        TO WRK-POSICAO

           PERFORM 2100-LER-EARQSOLI.

           PERFORM 3210-TRATAR-RESTART.

      *---------------------------------------------------------------*
       3312-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CALCULO DO DIGITO DA CONTA                         *
      *----------------------------------------------------------------*
       3313-CALULAR-DIGITO-CTA         SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-CONTA-AUX
           MOVE 13                     TO WRK-TAMANHO-0431.
           MOVE BVVEN6-NUM-CTA-P       TO WRK-CONTA-AUX

           CALL 'BRAD0431'             USING WRK-CONTA-AUX
                                             WRK-DIGITO-0431
                                             WRK-TAMANHO-0431.

           IF  WRK-DIGITO-0431         EQUAL 'P' OR '.'
               MOVE '0'                TO WRK-DIGITO-0431
           END-IF.

      *----------------------------------------------------------------*
       3313-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 2400-GRAVA-CONTSEQS
           PERFORM 4100-EMITIR-DISPLAY.
           PERFORM 4200-FIM-CHECKPOINT-RESTART.

           CLOSE EARQSOLI
                 CONTSEQE
                 CONTSEQS
                 EARQDATA
                 ARQARRC.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           STOP RUN.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *---------------------------------------------------------------*

           DISPLAY '*************** BVVEA142 ******************'
           DISPLAY '*                                         *'

           MOVE ACU-LID-EARQSOLI      TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS EARQSOLI: ' WRK-MASCARA '      *'

           MOVE ACU-LID-CONTSEQE      TO WRK-MASCARA.
           DISPLAY '*   REG. LIDOS CONTSEQE: ' WRK-MASCARA '      *'

           MOVE ACU-GRA-ARQARRC      TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA P ENVIO : ' WRK-MASCARA '      *'

           MOVE ACU-GRA-CONTSEQS      TO WRK-MASCARA.
           DISPLAY '*   REG. GRAVA CONTSEQS: ' WRK-MASCARA '      *'

           DISPLAY '*                                         *'

           DISPLAY '*************** BVVEA142 ******************'.

      *---------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZAR CHECK POINT                                      *
      *----------------------------------------------------------------*
       4200-FIM-CHECKPOINT-RESTART     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE 'F'                    TO CK01-FUNCAO.

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               PERFORM 1310-TRATAR-ERRO-RESTART
           END-IF.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *---------------------------------------------------------------*

           MOVE 'BVVEA142'             TO ERR-PGM.

           IF (ERR-TIPO-ACESSO         EQUAL 'APL')
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             WRK-COMU-SQLCA
           ELSE
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.


           GOBACK.
      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
