      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1155.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1155                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   29/07/2008                                  *
      *                                                                *
      *    OBJETIVO    :   INCLUIR VALORES DO ARQDEBIT NO ARQUIVO      *
      *                    TOTALIZA.                                   *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      ETOTALIZ                                    GFCTWAAQ      *
      *      ARQDEBIT                                    I#GFCTZJ      *
      *      STOTALIZ                                    GFCTWAAQ      *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100  -  MODULO DE TRATAMENTO DE ERRO.                *
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

           SELECT ETOTALIZ ASSIGN      TO UT-S-ETOTALIZ
                      FILE STATUS      IS WRK-FS-ETOTALIZ.

           SELECT ARQDEBIT ASSIGN      TO UT-S-ARQDEBIT
                      FILE STATUS      IS WRK-FS-ARQDEBIT.

           SELECT STOTALIZ ASSIGN      TO UT-S-STOTALIZ
                      FILE STATUS      IS WRK-FS-STOTALIZ.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 400                  *
      *----------------------------------------------------------------*

       FD  ETOTALIZ
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01 FD-CONFERENCIA.
          05 FD-CHAVE-COBRANCA.
             10 FD-DATA-ENVIO         PIC 9(08).
             10 FD-DIG-CONTRATO       PIC X(01).
             10 FD-NUM-CONTRATO       PIC 9(07).
          05 FD-CHAVE-ORIGEM.
             10 FD-DATA-ORIG          PIC X(10).
             10 FD-ROTINA-ORIG        PIC X(04).
             10 FD-NUM-ARQ-ORIG       PIC 9(02).
             10 FD-SEQ-ARQ-ORIG       PIC 9(11).
             10 FD-TARIFA-ORIG        PIC 9(05).
          05 FD-CGC-CPF.
ST25X6*       10 FD-CGC-CPF-MUNERO     PIC 9(09).
ST25X6        10 FD-CGC-CPF-MUNERO     PIC X(09).
ST25X6*       10 FD-CGC-CPF-FILIAL     PIC 9(05).
ST25X6        10 FD-CGC-CPF-FILIAL     PIC X(05).
ST25X6*       10 FD-CGC-CPF-CONTROLE   PIC 9(02).
ST25X6        10 FD-CGC-CPF-CONTROLE   PIC 9(02).
          05 FD-DADOS-DEBITO.
             10 FD-BANCO-DEB          PIC 9(03).
             10 FD-AGENCIA-DEB        PIC 9(05).
             10 FD-CONTA-DEB          PIC 9(07).
             10 FD-DIGITO-DEB         PIC X(01).
             10 FD-CARTEIRA-DEB       PIC 9(05).
             10 FD-COD-LCTO-DEB       PIC 9(05).
             10 FD-COD-MOEDA-DEB      PIC 9(05).
             10 FD-RAZAO.
                15 FD-GRUPO-DEB       PIC 9(03).
                15 FD-SUBGRUPO-DEB    PIC 9(03).
             10 FD-VALOR-DEBITAR      PIC 9(11)V9(2).
          05 FD-DETALHES-COBRANCA.
             10 FD-SITUACAO-EVENTO    PIC 9(02).
             10 FD-TIPO-COBRANCA      PIC 9(02).
             10 FD-TIPO-ARQUIVO       PIC 9(02).
             10 FD-DEB-PARCIAL        PIC 9(02).
             10 FD-TIPO-SALDO         PIC 9(02).
             10 FD-TIPO-CONTA         PIC 9(01).
             10 FD-INCONDICIONAL      PIC X(01).
             10 FD-ACEITA-PARCELADO   PIC X(01).
             10 FD-PARCIAL-ULT-REPIQ  PIC X(01).
             10 FD-VALOR-MINIMO       PIC 9(11)V9(2).
             10 FD-BANCO-ORIG         PIC 9(03).
             10 FD-AGENCIA-ORIG       PIC 9(05).
             10 FD-DATA-EVNTO-ORIG    PIC 9(08).
             10 FD-SEG-LINHA-EXTRATO  PIC X(32).
          05 FD-RETORNO.
             10 FD-JOB                PIC X(08).
             10 FD-PROGRAMA           PIC X(08).
             10 FD-RECURSO            PIC X(08).
             10 FD-COD-RETORNO        PIC X(03).
             10 FD-VALOR-DEBITADO     PIC 9(11)V9(2).
             10 FD-DESCRICAO          PIC X(50).
          05 FD-TRAG-EXT.
             10 FD-CENTRALIZ          PIC X(01).
             10 FD-AGENCIA-RET        PIC 9(05).
             10 FD-CONTA-RET          PIC 9(07).
          05 FD-DADOS-EXTERNOS.
             10 FD-QUANT              PIC 9(01) VALUE 1.
             10 FD-TRAG-SN            PIC X(01).
             10 FD-FITA4.
                15 FD-F4-SN           PIC X(01).
                15 FD-F4-AGEN         PIC 9(05).
                15 FD-F4-CONTA        PIC 9(07).
                15 FD-F4-VLR          PIC 9(11)V9(2).
             10 FD-ARQDEBIT.
                15 FD-DB-SN           PIC X(01).
                15 FD-DB-AGEN         PIC 9(05).
                15 FD-DB-CONTA        PIC 9(07).
                15 FD-DB-VLR          PIC 9(11)V9(2).
             10 FD-ERRO.
                15 FD-ERR-SN          PIC X(01).
                15 FD-ERR-REL-SN      PIC X(01).
                15 FD-ERR-DESCR       PIC X(50).
             10 FILLER                PIC X(02).

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 300                  *
      *----------------------------------------------------------------*

       FD  ARQDEBIT
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTZJ'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 400                  *
      *----------------------------------------------------------------*

       FD  STOTALIZ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAAQ'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1155  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-ETOTALIZ       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-ARQDEBIT       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-STOTALIZ    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-ERR-PROCESSO-ANT     PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-REG-OK               PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-ERRO-AG-CTA          PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-ERR-DB-SEM-REG       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-SEM-ARQDEBIT         PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-F4-SEM-ARQDEBIT      PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA CHAVES       *'.
      *----------------------------------------------------------------*

       01  WRK-CHV-ETOTALIZ.
           05 WRK-AGEN-ETOTALIZ         PIC 9(05)          VALUE ZEROS.
           05 WRK-CTA-ETOTALIZ          PIC 9(07)          VALUE ZEROS.
           05 WRK-TARI-ETOTALIZ         PIC 9(05)          VALUE ZEROS.
           05 WRK-VLR-ETOTALIZ          PIC 9(11)V99       VALUE ZEROS.

       01  WRK-CHV-ARQDEBIT.
           05 WRK-AGEN-ARQDEBIT         PIC 9(05)          VALUE ZEROS.
           05 WRK-CTA-ARQDEBIT          PIC 9(07)          VALUE ZEROS.
           05 WRK-TARI-ARQDEBIT         PIC 9(05)          VALUE ZEROS.
           05 WRK-VLR-ARQDEBIT          PIC 9(11)V99       VALUE ZEROS.

       01  WRK-CONTA-S                  PIC 9(13)         VALUE ZEROS.
       01  WRK-FILLER                   REDEFINES WRK-CONTA-S.
           05 FILLER                    PIC X(06).
           05 WRK-CONTA                 PIC 9(07).

       01  WRK-VALOR-R                  PIC 9(15)V99      VALUE ZEROS.
       01  WRK-FILLER                   REDEFINES WRK-VALOR-R.
           05 FILLER                    PIC X(04).
           05 WRK-VALOR                 PIC 9(11)V99.

       01  WRK-DESCRICAO-ERR1           PIC X(50)
                       VALUE 'DADOS ARQDEBIT DIFERENTES DO FITA4'.

       01  WRK-DESCRICAO-ERR2           PIC X(50)
                       VALUE 'GRAVOU FITA4 E NAO GRAVOU ARQDEBIT'.

       01  WRK-DESCRICAO-ERR3           PIC X(50)
                       VALUE 'REGISTRO GERAD. DO DEBITO NAO ENCONTRADO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ETOTALIZ             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ARQDEBIT             PIC X(02)           VALUE SPACES.
       01  WRK-FS-STOTALIZ             PIC X(02)           VALUE SPACES.
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
           '*     AREA PARA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT1155   *'.
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
                    WRK-CHV-ETOTALIZ   EQUAL HIGH-VALUES AND
                    WRK-CHV-ARQDEBIT   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ETOTALIZ
                      ARQDEBIT
               OUTPUT STOTALIZ.

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

           PERFORM 0300-TESTAR-FS-ETOTALIZ.

           PERFORM 0400-TESTAR-FS-ARQDEBIT.

           PERFORM 0500-TESTAR-FS-STOTALIZ.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ETOTALIZ                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ETOTALIZ         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ETOTALIZ         NOT EQUAL '00'
               MOVE 'ETOTALIZ'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ETOTALIZ    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ARQDEBIT                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-ARQDEBIT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQDEBIT         NOT EQUAL '00'
               MOVE 'ARQDEBIT'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQDEBIT    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO STOTALIZ                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-STOTALIZ         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-STOTALIZ         NOT EQUAL '00'
               MOVE 'STOTALIZ'         TO WRK-NOME-ARQ
               MOVE WRK-FS-STOTALIZ    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA P/ VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS  *
      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-LER-ETOTALIZ.

           IF  WRK-CHV-ETOTALIZ        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1155 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  ETOTALIZ VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1155 ***********'
           END-IF.

           PERFORM 0900-LER-ARQDEBIT.

           IF  WRK-CHV-ARQDEBIT            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1155 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  ARQDEBIT VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1155 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ETOTALIZ                                 *
      *----------------------------------------------------------------*
       0800-LER-ETOTALIZ               SECTION.
      *----------------------------------------------------------------*

           READ ETOTALIZ.

           IF  WRK-FS-ETOTALIZ         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ETOTALIZ
               MOVE SPACES             TO FD-ERR-SN
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ETOTALIZ.

           MOVE FD-AGENCIA-RET TO WRK-AGEN-ETOTALIZ.
           MOVE FD-CONTA-RET   TO WRK-CTA-ETOTALIZ.
           MOVE FD-TARIFA-ORIG TO WRK-TARI-ETOTALIZ.
           MOVE FD-F4-VLR      TO WRK-VLR-ETOTALIZ.

           ADD 1               TO ACU-LIDOS-ETOTALIZ.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ARQDEBIT                                 *
      *----------------------------------------------------------------*
       0900-LER-ARQDEBIT               SECTION.
      *----------------------------------------------------------------*

           READ ARQDEBIT.

           IF  WRK-FS-ARQDEBIT         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ARQDEBIT
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-ARQDEBIT.

           MOVE GFCTZJ-COD-AGENCIA       TO WRK-AGEN-ARQDEBIT.
           MOVE GFCTZJ-NUM-CONTA         TO WRK-CONTA-S.
           MOVE WRK-CONTA                TO WRK-CTA-ARQDEBIT.
           MOVE GFCTZJ-COD-EVENTO-TARIFA TO WRK-TARI-ARQDEBIT.
           MOVE GFCTZJ-VLR-COBRADO       TO WRK-VALOR-R.
           MOVE WRK-VALOR                TO WRK-VLR-ARQDEBIT.

           ADD 1                         TO ACU-LIDOS-ARQDEBIT.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF FD-ERR-SN        EQUAL 'S'
               MOVE FD-CONFERENCIA   TO REG-CONFERENCIA
               ADD 1                 TO ACU-ERR-PROCESSO-ANT
               PERFORM 1100-GRAVAR-STOTALIZ
               PERFORM 0800-LER-ETOTALIZ
           ELSE
               IF  WRK-CHV-ETOTALIZ EQUAL WRK-CHV-ARQDEBIT
                   MOVE FD-CONFERENCIA          TO REG-CONFERENCIA
                   MOVE 'S'                     TO CONF-DB-SN
                   MOVE GFCTZJ-COD-AGENCIA      TO CONF-DB-AGEN
                   MOVE WRK-CONTA               TO CONF-DB-CONTA
                   MOVE WRK-VALOR               TO CONF-DB-VLR
                   IF CONF-FITA4 EQUAL CONF-ARQDEBIT
                       ADD 1 TO ACU-REG-OK
                   ELSE
                       MOVE WRK-DESCRICAO-ERR1 TO CONF-ERR-DESCR
                       MOVE 'S'                TO CONF-ERR-SN
                       ADD 1                   TO ACU-ERRO-AG-CTA
                   END-IF
                   PERFORM 1100-GRAVAR-STOTALIZ
                   PERFORM 0800-LER-ETOTALIZ
                   PERFORM 0900-LER-ARQDEBIT
               ELSE
                   IF  WRK-CHV-ETOTALIZ LESS WRK-CHV-ARQDEBIT
                       MOVE FD-CONFERENCIA        TO REG-CONFERENCIA
                       IF CONF-F4-SN EQUAL 'S'
                           MOVE 'S'                TO CONF-DB-SN
                           MOVE 'S'                TO CONF-ERR-SN
                           MOVE WRK-DESCRICAO-ERR2 TO
                                                      CONF-ERR-DESCR
                           ADD 1                  TO ACU-F4-SEM-ARQDEBIT
                       ELSE
                           MOVE 'N'               TO CONF-DB-SN
                           ADD 1                  TO ACU-SEM-ARQDEBIT
                       END-IF
                       PERFORM 1100-GRAVAR-STOTALIZ
                       PERFORM 0800-LER-ETOTALIZ
                   ELSE
                       INITIALIZE REG-CONFERENCIA
                       MOVE '01.01.0001'          TO CONF-DATA-ORIG
                       MOVE 'ERRO'                TO CONF-ROTINA-ORIG
                       MOVE 1                     TO CONF-QUANT
                       MOVE GFCTZJ-COD-AGENCIA    TO CONF-AGENCIA-DEB
                       MOVE WRK-CONTA             TO CONF-CONTA-DEB
                       MOVE GFCTZJ-COD-GRUPO      TO CONF-GRUPO-DEB
                       MOVE GFCTZJ-COD-SUB-GRUPO  TO CONF-SUBGRUPO-DEB
                       MOVE GFCTZJ-COD-EVENTO-TARIFA TO CONF-TARIFA-ORIG
                       MOVE WRK-VALOR TO CONF-DB-VLR
                       MOVE GFCTZJ-CNPJ-CPF-PRINCIPAL TO
                                               CONF-CGC-CPF-MUNERO
                       MOVE GFCTZJ-CNPJ-CPF-FILIAL    TO
                                               CONF-CGC-CPF-FILIAL
                       MOVE GFCTZJ-CNPJ-CPF-CONTROLE  TO
                                                   CONF-CGC-CPF-CONTROLE
                       MOVE GFCTZJ-DATA-EVENTO        TO
                                                   CONF-DATA-EVNTO-ORIG
                       MOVE 'S'                       TO CONF-DB-SN
                       MOVE WRK-DESCRICAO-ERR3        TO CONF-ERR-DESCR
                       MOVE 'S'                       TO CONF-ERR-SN
                       ADD 1 TO ACU-ERR-DB-SEM-REG
                       PERFORM 1100-GRAVAR-STOTALIZ
                       PERFORM 0900-LER-ARQDEBIT
                   END-IF
               END-IF
           END-IF.
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  STOTALIZ                                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-STOTALIZ            SECTION.
      *----------------------------------------------------------------*

           WRITE REG-CONFERENCIA OF STOTALIZ.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-STOTALIZ.

           ADD 1                       TO ACU-GRAVADOS-STOTALIZ.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE ETOTALIZ
                 ARQDEBIT
                 STOTALIZ.

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

           DISPLAY '******************** GFCT1155 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ETOTALIZ     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ETOTALIZ......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ARQDEBIT     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ARQDEBIT......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-STOTALIZ  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO STOTALIZ...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-ERR-PROCESSO-ANT   TO WRK-MASCARA.
           DISPLAY '*  ERRO PROCESSO ANTERIOR.: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-REG-OK             TO WRK-MASCARA.
           DISPLAY '*  REGISTROS OK...........: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-ERRO-AG-CTA        TO WRK-MASCARA.
           DISPLAY '*  ARQDEBIT DIFERENTE FIT4: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-SEM-ARQDEBIT          TO WRK-MASCARA.
           DISPLAY '*  REGISTRO SEM ARQDEBIT..: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-F4-SEM-ARQDEBIT       TO WRK-MASCARA.
           DISPLAY '*  REG.GRV.F4 E NAO ARQDBT: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-ERR-DB-SEM-REG     TO WRK-MASCARA.
           DISPLAY '*  REG.GERAD.DEBTO.NAO ENC: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1155 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1155'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
