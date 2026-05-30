      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2606.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2606                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   17/11/2008                                  *
      *                                                                *
      *    OBJETIVO    :   PREPARA LAYOUT DA CARGA NA BASE DE EXTRATO  *
      *                    MENSAL.                                     *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      EXTRAMOK                                    GFCTWADM      *
      *      EXTRAMEN                                    GFCTWADN      *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100 -  MODULO DE TRATAMENTO DE ERRO.                 *
      *      BRAD7600 -  OBTER DATA E HORA.                            *
      *                                                                *
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

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT EXTRAMOK ASSIGN      TO UT-S-EXTRAMOK
                      FILE STATUS      IS WRK-FS-EXTRAMOK.

           SELECT EXTRAMEN ASSIGN      TO UT-S-EXTRAMEN
                      FILE STATUS      IS WRK-FS-EXTRAMEN.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 150                  *
      *----------------------------------------------------------------*

       FD  EXTRAMOK
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWADM'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 093                  *
      *----------------------------------------------------------------*

       FD  EXTRAMEN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWADN'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2606  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-EXTRAMOK       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-EXTRAMEN    PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                       PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH              PIC X(08)             VALUE
              'BATCH'.
           05 WRK-MASCARA            PIC ZZ.ZZZ.ZZZ.Z99    VALUE ZEROS.

       01  WRK-AAAAMM                PIC  9(006)           VALUE ZEROS.
       01  FILLER                    REDEFINES WRK-AAAAMM.
           05 WRK-ANO-A              PIC  9(004).
           05 WRK-MES-A              PIC  9(002).

       01  WRK-CONT                  PIC  9(003)    COMP-3 VALUE ZEROS.
       01  WRK-VLR-LANCAMENTO-COMP   PIC  9(12)V99  COMP-3 VALUE ZEROS.
       01  WRK-QTDE-DETRM            PIC  9(005)    COMP-3 VALUE ZEROS.
       01  WRK-VLR-DETRM             PIC  9(012)V99 COMP-3
                                                           VALUE ZEROS.

       01  WRK-VCONSL-TARIF-R        PIC  9(012)V99        VALUE ZEROS.
       01  FILLER                    REDEFINES WRK-VCONSL-TARIF-R.
           05 FILLER                 PIC  9(003).
           05 WRK-VCONSL-TARIF       PIC  9(009)V99.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-EXTRAMOK             PIC X(02)           VALUE SPACES.
       01  WRK-FS-EXTRAMEN             PIC X(02)           VALUE SPACES.
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

       01  WRK-CHV-EXTRAMOK-ATU.
           03 WRK-CHV-PRIN-ATU.
              05 WRK-AAAAMM-ATU            PIC 9(006)    VALUE ZEROS.
              05 WRK-AGENCIA-ATU           PIC 9(005)    VALUE ZEROS.
              05 WRK-CONTA-ATU             PIC 9(007)    VALUE ZEROS.
              05 WRK-IDENT-ATU             PIC 9(001)    VALUE ZEROS.
           03 WRK-SGN-LINHA-ATU            PIC X(032)    VALUE SPACES.

       01  WRK-CHV-EXTRAMOK-ANT.
           03 WRK-CHV-PRIN-ANT.
              05 WRK-AAAAMM-ANT            PIC 9(006)    VALUE ZEROS.
              05 WRK-AGENCIA-ANT           PIC 9(005)    VALUE ZEROS.
              05 WRK-CONTA-ANT             PIC 9(007)    VALUE ZEROS.
              05 WRK-IDENT-ANT             PIC 9(001)    VALUE ZEROS.
           03 WRK-SGN-LINHA-ANT            PIC X(032)    VALUE SPACES.

       01  WRK-REG-ANT.
           03 WRK-TIPO-REGISTRO-ANT        PIC X(001)    VALUE SPACES.
           03 WRK-COD-AGENCIA-ANT          PIC 9(005)    VALUE ZEROS.
           03 WRK-CPF-CNPJ-ANT.
ST25X6*       05 WRK-PRINCIPAL-ANT         PIC 9(009)    VALUE ZEROS.
ST25X6        05 WRK-PRINCIPAL-ANT         PIC X(009)    VALUE SPACES.
ST25X6*       05 WRK-FILIAL-ANT            PIC 9(004)    VALUE ZEROS.
ST25X6        05 WRK-FILIAL-ANT            PIC X(004)    VALUE SPACES.
              05 WRK-CTLE-ANT              PIC 9(002)    VALUE ZEROS.
           03 WRK-RAZAO-ANT.
              05 WRK-GRUPOR-ANT            PIC 9(003)    VALUE ZEROS.
              05 WRK-SUBGP-ANT             PIC 9(003)    VALUE ZEROS.
           03 WRK-NRO-CONTA-ANT            PIC 9(007)    VALUE ZEROS.
           03 WRK-NUMERO-DOCUMENTO-ANT     PIC 9(007)    VALUE ZEROS.
           03 WRK-COD-LANCAMENTO-ANT       PIC 9(005)    VALUE ZEROS.
           03 WRK-DATA-LANCAMENTO-ANT.
              05 WRK-DT-LANCTO-DD-ANT      PIC 9(002)    VALUE ZEROS.
              05 WRK-DT-LANCTO-MM-ANT      PIC 9(002)    VALUE ZEROS.
              05 WRK-DT-LANCTO-AAAA-ANT    PIC 9(004)    VALUE ZEROS.
           03 WRK-VLR-LANCAMENTO-ANT       PIC 9(012)V99 VALUE ZEROS.
           03 WRK-SEGUNDALINHA-ANT         PIC X(032)    VALUE ZEROS.
           03 WRK-IDENTR-ANT               PIC 9(001)    VALUE ZEROS.
           03 WRK-QTDE-TAR-ANT             PIC 9(005)    VALUE ZEROS.
           03 FILLER                       PIC X(044).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DA BRAD7600'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           05  WRK-TIMESTAMP.
               10  WRK-ANO             PIC  9(004)         VALUE ZEROS.
               10  WRK-MES             PIC  9(002)         VALUE ZEROS.
               10  WRK-DIA             PIC  9(002)         VALUE ZEROS.
               10  WRK-HOR             PIC  9(002)         VALUE ZEROS.
               10  WRK-MIN             PIC  9(002)         VALUE ZEROS.
               10  WRK-SEG             PIC  9(002)         VALUE ZEROS.
               10  WRK-MIL-SEG         PIC  9(006)         VALUE ZEROS.

       01  WRK-TIMESTAMP-AUX.
           05 WRK-ANO-AUX-1            PIC  9(004)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-MES-AUX-1            PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-DIA-AUX-1            PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-HOR-AUX-1            PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MIN-AUX-1            PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-SEG-AUX-1            PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MIL-AUX-1            PIC  9(006)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT2606   *'.
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

           PERFORM 0900-OBTER-DATA-SISTEMA.

           MOVE GFCTWADM-REG           TO WRK-REG-ANT.
           MOVE WRK-CHV-EXTRAMOK-ATU   TO WRK-CHV-EXTRAMOK-ANT.

           PERFORM 1000-PROCESSAR      UNTIL
                    WRK-FS-EXTRAMOK    EQUAL '10'.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT EXTRAMOK
               OUTPUT EXTRAMEN.

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

           PERFORM 0300-TESTAR-FS-EXTRAMOK.

           PERFORM 0600-TESTAR-FS-EXTRAMEN.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXTRAMOK                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-EXTRAMOK         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRAMOK         NOT EQUAL '00'
               MOVE 'EXTRAMOK'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXTRAMOK    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXTRAMEN                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-EXTRAMEN         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRAMEN         NOT EQUAL '00'
               MOVE 'EXTRAMEN'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXTRAMEN    TO WRK-FS
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

           PERFORM 0800-LER-EXTRAMOK.

           IF  WRK-FS-EXTRAMOK        EQUAL '10'
               DISPLAY '*********** GFCT2606 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  EXTRAMOK VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2606 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO EXTRAMOK                                 *
      *----------------------------------------------------------------*
       0800-LER-EXTRAMOK               SECTION.
      *----------------------------------------------------------------*

           READ EXTRAMOK.

           IF  WRK-FS-EXTRAMOK         EQUAL '10'
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-EXTRAMOK.

           MOVE WADM-DT-LANCTO-MM      TO WRK-MES-A.
           MOVE WADM-DT-LANCTO-AAAA    TO WRK-ANO-A.
           MOVE WRK-AAAAMM             TO WRK-AAAAMM-ATU.
           MOVE WADM-COD-AGENCIA       TO WRK-AGENCIA-ATU.
           MOVE WADM-NRO-CONTA         TO WRK-CONTA-ATU.
           MOVE WADM-IDENT             TO WRK-IDENT-ATU.
           MOVE WADM-SEGUNDA-LINHA     TO WRK-SGN-LINHA-ATU.

           ADD 1                       TO ACU-LIDOS-EXTRAMOK.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DATA DO SISTEMA                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       0900-OBTER-DATA-SISTEMA         SECTION.
      *----------------------------------------------------------------*

           CALL  'BRAD7600'            USING WRK-DATA-HORA.

           MOVE WRK-ANO                TO WRK-ANO-AUX-1.
           MOVE WRK-MES                TO WRK-MES-AUX-1.
           MOVE WRK-DIA                TO WRK-DIA-AUX-1.
           MOVE WRK-HOR                TO WRK-HOR-AUX-1.
           MOVE WRK-MIN                TO WRK-MIN-AUX-1.
           MOVE WRK-SEG                TO WRK-SEG-AUX-1.
           MOVE WRK-MIL-SEG            TO WRK-MIL-AUX-1.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-EXTRAMOK-ATU NOT EQUAL WRK-CHV-EXTRAMOK-ANT
               PERFORM 1050-QUEBRA
               MOVE ZEROS                TO WRK-QTDE-DETRM
                                            WRK-VLR-DETRM
               IF WRK-CHV-PRIN-ATU NOT EQUAL WRK-CHV-PRIN-ANT
                   MOVE ZEROS            TO WRK-CONT
               END-IF
               MOVE GFCTWADM-REG         TO WRK-REG-ANT
               MOVE WRK-CHV-EXTRAMOK-ATU TO WRK-CHV-EXTRAMOK-ANT
           END-IF.

           IF WADM-IDENT EQUAL 2 OR 3
               ADD  WADM-QTDE-TAR        TO WRK-QTDE-DETRM
               MOVE WRK-QTDE-DETRM       TO WADN-QCONSL-DETRM-TARIF
           ELSE
               ADD  1                    TO WRK-QTDE-DETRM
               MOVE WRK-QTDE-DETRM       TO WADN-QCONSL-DETRM-TARIF
           END-IF.

           MOVE WADM-VLR-LANCAMENTO      TO WRK-VLR-LANCAMENTO-COMP
           ADD  WRK-VLR-LANCAMENTO-COMP  TO WRK-VLR-DETRM
           MOVE WRK-VLR-DETRM            TO WRK-VCONSL-TARIF-R
           MOVE WRK-VCONSL-TARIF         TO WADN-VCONSL-DETRM-TARIF

           PERFORM 0800-LER-EXTRAMOK.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    QUEBRA DE CHAVES                                            *
      *----------------------------------------------------------------*
       1050-QUEBRA            SECTION.
      *----------------------------------------------------------------*

           ADD  1                      TO WRK-CONT.
           MOVE 237                    TO WADN-CBCO
                                          WADN-CBCO-CCREN.
           MOVE WRK-COD-AGENCIA-ANT    TO WADN-CAG-BCRIA.
           MOVE WRK-NRO-CONTA-ANT      TO WADN-CCTA-BCRIA-CLI.
           MOVE WRK-DT-LANCTO-AAAA-ANT TO WADN-DANO-REFT.
           MOVE WRK-DT-LANCTO-MM-ANT   TO WADN-DMES-REFT.
           MOVE WRK-IDENTR-ANT         TO WADN-CIDTFD-TARIF.
           MOVE WRK-CONT               TO WADN-NLIN-EXTRT-CLI.
           MOVE WRK-COD-LANCAMENTO-ANT TO WADN-CIDTFD-LCTO.
           MOVE WRK-QTDE-DETRM         TO WADN-QCONSL-DETRM-TARIF.
           MOVE WRK-VCONSL-TARIF       TO WADN-VCONSL-DETRM-TARIF.
           MOVE WRK-SEGUNDALINHA-ANT   TO WADN-RSEGDA-LIN.
           MOVE WRK-TIMESTAMP-AUX      TO WADN-HINCL-REG-SIST.

           IF WRK-TIPO-REGISTRO-ANT    EQUAL 'E'
               MOVE 2                  TO WADN-CIDTFD-SNAL-LCTO
           ELSE
               MOVE 1                  TO WADN-CIDTFD-SNAL-LCTO
           END-IF.

           PERFORM 1200-GRAVAR-EXTRAMEN.

      *----------------------------------------------------------------*
       1050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  EXTRAMEN                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-EXTRAMEN            SECTION.
      *----------------------------------------------------------------*

           WRITE GFCTWADN-REG          OF EXTRAMEN.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-EXTRAMEN.

           ADD 1                       TO ACU-GRAVADOS-EXTRAMEN.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           IF ACU-LIDOS-EXTRAMOK   NOT EQUAL ZEROS
               PERFORM 1050-QUEBRA
           END-IF.

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE EXTRAMOK
                 EXTRAMEN.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMISSAO DOS DISPLAYS FINAIS DO PROGRAMA         *
      *----------------------------------------------------------------*
       1400-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT2606 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-EXTRAMOK     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM EXTRAMOK......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-EXTRAMEN  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO EXTRAMEN...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2606 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2606'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
