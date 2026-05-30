      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2692.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2692                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   13/12/2008                                  *
      *                                                                *
      *    OBJETIVO    :   IDENTIFICA AGENCIA ESCOLHIDA PARA PREVIA.   *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      EXMSTPRV                                    GFCTWAET      *
100209*      IDTPERAG                                    GFCTWAFD      *
      *      EXMSTPV1                                    GFCTWAET      *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100  -  MODULO DE TRATAMENTO DE ERRO.                *
      *                                                                *
      *================================================================*
100209*================================================================*
100209*      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
100209*----------------------------------------------------------------*
100209*                A   L   T   E   R   A   C   A   O               *
100209*----------------------------------------------------------------*
100209*    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - PROCWORK/GP50 *
100209*    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - PROCWORK/GP50 *
100209*    DATA........:   02/02/2009                                  *
100209*----------------------------------------------------------------*
100209*    OBJETIVO....:   ALTERACAO NO PROCESSO INTERNO E             *
100209*                    ALTERACAO NA ENTRADA, UTILIZANDO INCLUDE.   *
100209*================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT EXMSTPRV ASSIGN      TO UT-S-EXMSTPRV
                      FILE STATUS      IS WRK-FS-EXMSTPRV.

           SELECT IDTPERAG ASSIGN      TO UT-S-IDTPERAG
                      FILE STATUS      IS WRK-FS-IDTPERAG.

           SELECT EXMSTPV1 ASSIGN      TO UT-S-EXMSTPV1
                      FILE STATUS      IS WRK-FS-EXMSTPV1.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 080                  *
      *----------------------------------------------------------------*

       FD  EXMSTPRV
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EXMSTPRV-REG         PIC X(080).

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
100209*            ORG. SEQUENTIAL     -  LRECL = 070                  *
      *----------------------------------------------------------------*

       FD  IDTPERAG
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

100209 COPY 'GFCTWAFD'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 080                  *
      *----------------------------------------------------------------*

       FD  EXMSTPV1
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EXMSTPV1-REG         PIC X(080).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2692  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-EXMSTPRV       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-IDTPERAG       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-EXMSTPV1    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADO           PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)            VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

100209 01 WRK-AG-S                     PIC S9(05)          VALUE ZEROS.
100209 01  FILLER                      REDEFINES WRK-AG-S.
100209     05 WRK-AG                   PIC  9(05).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-EXMSTPRV             PIC X(02)           VALUE SPACES.
       01  WRK-FS-IDTPERAG             PIC X(02)           VALUE SPACES.
       01  WRK-FS-EXMSTPV1             PIC X(02)           VALUE SPACES.
       01  WRK-ABERTURA                PIC X(13)           VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC X(13)           VALUE
           'NA  LEITURA'.
       01  WRK-GRAVACAO                PIC X(13)           VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC X(13)           VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA  DE MENSAGEM  DE ERRO  *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC X(07)           VALUE SPACES.
           05  FILLER                  PIC X(07)           VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           05  FILLER                  PIC X(12)           VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(08)           VALUE SPACES.
           05  FILLER                  PIC X(17)           VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC X(02)           VALUE SPACES.
           05  FILLER                  PIC X(02)           VALUE ' *'.
           05  FILLER                  PIC X(07)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA CHAVES       *'.
      *----------------------------------------------------------------*

       01  WRK-CHV-EXMSTPRV.
           05 WRK-AG-EXMSTPRV        PIC 9(05)           VALUE ZEROS.

       01  WRK-CHV-IDTPERAG.
           05 WRK-AG-IDTPERAG        PIC 9(05)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA INCS       *'.
      *----------------------------------------------------------------*

       COPY 'GFCTWAET'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT2692   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0700-VERIFICAR-VAZIO.

           PERFORM 1000-PROCESSAR      UNTIL
                    WRK-CHV-EXMSTPRV   EQUAL HIGH-VALUES.

           PERFORM 1400-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT EXMSTPRV
                      IDTPERAG
               OUTPUT EXMSTPV1.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE STATUS DOS ARQUIVOS DE ENTRADA E SAIDA          *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-EXMSTPRV.

           PERFORM 0400-TESTAR-FS-IDTPERAG.

           PERFORM 0600-TESTAR-FS-EXMSTPV1.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXMSTPRV                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-EXMSTPRV         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXMSTPRV         NOT EQUAL '00'
               MOVE 'EXMSTPRV'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXMSTPRV    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO IDTPERAG                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-IDTPERAG         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-IDTPERAG         NOT EQUAL '00'
               MOVE 'IDTPERAG'         TO WRK-NOME-ARQ
               MOVE WRK-FS-IDTPERAG    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXMSTPV1                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-EXMSTPV1         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXMSTPV1         NOT EQUAL '00'
               MOVE 'EXMSTPV1'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXMSTPV1    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA P/ VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS  *
      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-LER-EXMSTPRV.

           IF  WRK-CHV-EXMSTPRV        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2692 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  EXMSTPRV VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2692 ***********'
           END-IF.

           PERFORM 0900-LER-IDTPERAG.

           IF  WRK-CHV-IDTPERAG            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2692 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO IDTPERAG VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2692 ***********'
               MOVE  'APL'                      TO  ERR-TIPO-ACESSO
               MOVE  'IDTPERAG - ARQUIVO VAZIO' TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO EXMSTPRV                                 *
      *----------------------------------------------------------------*
       0800-LER-EXMSTPRV               SECTION.
      *----------------------------------------------------------------*

           READ EXMSTPRV               INTO GFCTWAET-REG.

           IF  WRK-FS-EXMSTPRV         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-EXMSTPRV
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-EXMSTPRV.

           MOVE WAET-AGENCIA-PREV      TO WRK-AG-S.
           MOVE WRK-AG                 TO WRK-AG-EXMSTPRV.

           ADD 1                       TO ACU-LIDOS-EXMSTPRV.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO IDTPERAG                                 *
      *----------------------------------------------------------------*
       0900-LER-IDTPERAG               SECTION.
      *----------------------------------------------------------------*

           READ IDTPERAG.

           IF  WRK-FS-IDTPERAG         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-IDTPERAG
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-IDTPERAG.

100209     MOVE WAFD-CAG-BCRIA         TO WRK-AG-S.
100209     MOVE WRK-AG                 TO WRK-AG-IDTPERAG.

           ADD 1                       TO ACU-LIDOS-IDTPERAG.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

100209*----------------------------------------------------------------*
100209*    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
100209*----------------------------------------------------------------*
100209 1000-PROCESSAR                  SECTION.
100209*----------------------------------------------------------------*
100209
100209     IF WRK-CHV-IDTPERAG EQUAL ZEROS
100209         PERFORM 1020-TRATAMENTO-B
100209     ELSE
100209         PERFORM 1010-TRATAMENTO-A
100209     END-IF.
100209
100209*----------------------------------------------------------------*
100209 1000-99-FIM.                    EXIT.
100209*----------------------------------------------------------------*

100209*----------------------------------------------------------------*
100209 1010-TRATAMENTO-A                  SECTION.
100209*----------------------------------------------------------------*
100209
100209     IF  WRK-CHV-EXMSTPRV      EQUAL WRK-CHV-IDTPERAG
100209         PERFORM 1300-GRAVAR-EXMSTPRV
100209         PERFORM 0800-LER-EXMSTPRV
100209     ELSE
100209         IF  WRK-CHV-EXMSTPRV  LESS WRK-CHV-IDTPERAG
100209             ADD 1   TO ACU-DESPREZADO
100209             PERFORM 0800-LER-EXMSTPRV
100209         ELSE
100209             PERFORM 0900-LER-IDTPERAG
100209         END-IF
100209     END-IF.
100209
100209*----------------------------------------------------------------*
100209 1010-99-FIM.                    EXIT.
100209*----------------------------------------------------------------*
100209
100209*----------------------------------------------------------------*
100209 1020-TRATAMENTO-B                  SECTION.
100209*----------------------------------------------------------------*
100209
100209     PERFORM 1300-GRAVAR-EXMSTPRV.
100209
100209     PERFORM 0800-LER-EXMSTPRV.
100209
100209*----------------------------------------------------------------*
100209 1020-99-FIM.                    EXIT.
100209*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  EXMSTPRV                                    *
      *----------------------------------------------------------------*
       1300-GRAVAR-EXMSTPRV            SECTION.
      *----------------------------------------------------------------*

           MOVE  FD-EXMSTPRV-REG  TO FD-EXMSTPV1-REG.

           WRITE FD-EXMSTPV1-REG  OF EXMSTPV1.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-EXMSTPV1.

           ADD 1                  TO ACU-GRAVADOS-EXMSTPV1.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1400-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1500-EMITIR-DISPLAY.

           CLOSE EXMSTPRV
                 IDTPERAG
                 EXMSTPV1.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMISSAO DOS DISPLAYS FINAIS DO PROGRAMA         *
      *----------------------------------------------------------------*
       1500-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT2692 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-EXMSTPRV     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM EXMSTPRV......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-IDTPERAG     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM IDTPERAG......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-EXMSTPV1  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO EXMSTPV1...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADO         TO WRK-MASCARA.
           DISPLAY '*  DESPREZADOS............: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2692 ********************'.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2692'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
