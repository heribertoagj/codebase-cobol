      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2104.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2104                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   05/09/2008                                  *
      *                                                                *
      *    OBJETIVO    :   VERIFICAR CONTA GARANTIDA.                  *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      ADESAO                                    I#GFCTD0        *
      *      SADESAO                                   I#GFCTD0        *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100  -  MODULO DE TRATAMENTO DE ERRO.                *
      *      EMPG0005  -  VERIFICA SE O CLIENTE POSSUI CONTA GARANTIDA *
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

           SELECT ADESAO   ASSIGN      TO UT-S-ADESAO
                      FILE STATUS      IS WRK-FS-ADESAO.

           SELECT SADESAO ASSIGN       TO UT-S-SADESAO
                      FILE STATUS      IS WRK-FS-SADESAO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 200                  *
      *----------------------------------------------------------------*

       FD  ADESAO
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTD0'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 200                  *
      *----------------------------------------------------------------*

       FD  SADESAO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SADESAO          PIC X(200).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2104  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-ADESAO         PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-SADESAO     PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                       PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-MODULO                  PIC X(08) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA CHAVES       *'.
      *----------------------------------------------------------------*

       01  WRK-CHV-ATU.
           05 WRK-AG-ATU            PIC 9(05)         VALUE ZEROS.
           05 WRK-CTA-ATU           PIC 9(07)         VALUE ZEROS.

       01  WRK-CHV-ANT.
           05 WRK-AG-ANT            PIC 9(05)         VALUE ZEROS.
           05 WRK-CTA-ANT           PIC 9(07)         VALUE ZEROS.

       01  WRK-CONTA-GARANTIDA      PIC X(01)         VALUE 'N'.

       01  WRK-CONTA                   PIC  9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CONTA.
           05  FILLER                  PIC  9(006).
           05  WRK-GFCT-CONTA          PIC  9(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ADESAO             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SADESAO             PIC X(02)           VALUE SPACES.
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
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO ***'.
      *----------------------------------------------------------------*

       01  WRK-IEMPGAL.
           03  WRK-X5IAL-AGENCIA          PIC  9(005) COMP-3
                                                      VALUE ZEROS.
           03  WRK-X5IAL-CONTA            PIC  9(013) COMP-3
                                                      VALUE ZEROS.
           03  WRK-X5IAL-TIPO-CHAMADA-ENT PIC  X(001) VALUE SPACES.
           03  WRK-X5IAL-INDCD-CONTA-ENC  PIC  X(001) VALUE SPACES.
           03  WRK-X5IAL-SQLCODE          PIC S9(004) VALUE ZEROS.
           03  WRK-X5IAL-COD-RETORNO      PIC  9(002) VALUE ZEROS.
           03  WRK-X5IAL-MENSAGEM         PIC  X(050) VALUE SPACES.
           03  WRK-X5IAL-LIVRE            PIC  X(015) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT2104   *'.
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
                       WRK-FS-ADESAO   EQUAL '10'.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ADESAO
               OUTPUT SADESAO.

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

           PERFORM 0300-TESTAR-FS-ADESAO.

           PERFORM 0500-TESTAR-FS-SADESAO.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO ADESAO                        *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ADESAO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESAO         NOT EQUAL '00'
               MOVE 'ADESAO'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESAO    TO WRK-FS
               MOVE WRK-ERRO-FS      TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SADESAO                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-SADESAO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SADESAO         NOT EQUAL '00'
               MOVE 'SADESAO'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SADESAO    TO WRK-FS
               MOVE WRK-ERRO-FS       TO ERR-TEXTO
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

           PERFORM 0800-LER-ADESAO.

           IF  WRK-FS-ADESAO        EQUAL '10'
               DISPLAY '*********** GFCT2104 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  ADESAO VAZIO     *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2104 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ADESAO                                   *
      *----------------------------------------------------------------*
       0800-LER-ADESAO               SECTION.
      *----------------------------------------------------------------*

           READ ADESAO.

           IF  WRK-FS-ADESAO           EQUAL '10'
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ADESAO.

           MOVE D0-AGENCIA             TO WRK-AG-ATU.
           MOVE D0-CONTA               TO WRK-GFCT-CONTA
                                          WRK-CTA-ATU.
           ADD 1                       TO ACU-LIDOS-ADESAO.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-ATU NOT EQUAL WRK-CHV-ANT
               PERFORM 1050-VERIFICAR-CONTA
           END-IF.

           IF WRK-CONTA-GARANTIDA EQUAL 'S'
               ADD 1 TO ACU-DESPREZADOS
           ELSE
               PERFORM 1100-GRAVAR-SADESAO
           END-IF.

           MOVE WRK-CHV-ATU TO WRK-CHV-ANT.

           PERFORM 0800-LER-ADESAO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VERIFICAR CONTA NO MODULO EMPG0005                          *
      *----------------------------------------------------------------*
       1050-VERIFICAR-CONTA            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-IEMPGAL.

           MOVE  D0-AGENCIA               TO WRK-X5IAL-AGENCIA.
           MOVE  WRK-CONTA                TO WRK-X5IAL-CONTA.
           MOVE  'B'                      TO WRK-X5IAL-TIPO-CHAMADA-ENT.
           MOVE 'EMPG0005'                TO WRK-MODULO.
           CALL WRK-MODULO             USING WRK-IEMPGAL.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               DISPLAY '*********** GFCT2104 ***********'
               DISPLAY '*                              *'
               DISPLAY '*   ERRO NO ACESSO AO MODULO   *'
               DISPLAY '*           EMPG0005           *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2104 ***********'
               PERFORM 9999-ROTINA-ERRO
           ELSE
               MOVE  WRK-X5IAL-INDCD-CONTA-ENC
                                       TO WRK-CONTA-GARANTIDA
           END-IF.

      *----------------------------------------------------------------*
       1050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  SADESAO                                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-SADESAO            SECTION.
      *----------------------------------------------------------------*

           MOVE  D0-REGISTRO    TO FD-REG-SADESAO.

           WRITE FD-REG-SADESAO OF SADESAO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-SADESAO.

           ADD 1                       TO ACU-GRAVADOS-SADESAO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE ADESAO
                 SADESAO.

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

           DISPLAY '******************** GFCT2104 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ADESAO     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ADESAO........: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-SADESAO  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NA SAIDA......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  REGISTROS COM CONTAS                          *'.
           DISPLAY '*  GARANTIDAS.............: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2104 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2104'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
