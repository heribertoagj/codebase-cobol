      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2100.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2100                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   15/06/2009                                  *
      *                                                                *
      *    OBJETIVO    :   CADASTR0 DE CLIENTES SOMENTE COM AGENCIA    *
      *                    VALIDAS.                                    *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      CADACLIE                                    I#CLIE20      *
      *      CLIEADES                                    I#GFCTRU      *
      *      CLIEINVA                                    I#CLIE20      *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100  -  MODULO DE TRATAMENTO DE ERRO.                *
      *      MESU9400  -  MODULO DE PESQUISA DE JUNCOES.               *
      *                                                                *
      *================================================================*
MAI10 *================================================================*
MAI10 *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
MAI10 *----------------------------------------------------------------*
MAI10 *    PROGRAMADOR.:   FABRICA                     - SONDPROC/GP.50*
MAI10 *    ANALISTA....:   MARCUS VINICIUS             - SONDPROC/GP.50*
MAI10 *    DATA........:   MAI/2010                                    *
MAI10 *                                                                *
MAI10 *    OBJETIVO    :   INCLUIR ARQUIVO DE SAIDA CLIPSCCE.          *
MAI10 *================================================================*

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

           SELECT CADACLIE ASSIGN      TO UT-S-CADACLIE
                      FILE STATUS      IS WRK-FS-CADACLIE.

           SELECT CLIEADES ASSIGN      TO UT-S-CLIEADES
                      FILE STATUS      IS WRK-FS-CLIEADES.

           SELECT CLIEINVA ASSIGN      TO UT-S-CLIEINVA
                      FILE STATUS      IS WRK-FS-CLIEINVA.

MAI10      SELECT CLIPSCCE ASSIGN      TO UT-S-CLIPSCCE
MAI10                 FILE STATUS      IS WRK-FS-CLIPSCCE.
MAI10
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 180                  *
      *----------------------------------------------------------------*

       FD  CADACLIE
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#CLIE20'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 040                  *
      *----------------------------------------------------------------*

       FD  CLIEADES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 180                  *
      *----------------------------------------------------------------*

       FD  CLIEINVA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#CLIE20'.

MAI10 *----------------------------------------------------------------*
MAI10 *    OUTPUT:                                                     *
MAI10 *            ORG. SEQUENTIAL     -  LRECL = 090                  *
MAI10 *----------------------------------------------------------------*
MAI10
MAI10  FD  CLIPSCCE
MAI10      RECORDING MODE IS F
MAI10      LABEL RECORD IS STANDARD
MAI10      BLOCK CONTAINS 0 RECORDS.
MAI10
MAI10  01  FD-REG-CLIPSCCE             PIC X(090).
MAI10
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2100  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-CADACLIE       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-CLIEADES    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.
MAI10      05 ACU-GRAVADOS-CLIPSCCE    PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99  VALUE ZEROS.

       01  WRK-CADACLIE-ATU.
           03 WRK-AGENCIA-ATU          PIC 9(005)          VALUE ZEROS.

       01  WRK-CADACLIE-ANT.
           03 WRK-AGENCIA-ANT          PIC 9(005)          VALUE ZEROS.

       01  WRK-FLAG-AGENCIA            PIC X(001)          VALUE 'N'.
       01  WRK-MODULO                  PIC X(008)          VALUE SPACES.

MAI10  01  WRK-DIG-CTA-X.
MAI10      05  WRK-DIG-CTA-N           PIC  9(001)         VALUE ZEROS.
MAI10
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    MODULO MESU9410      *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-MESU9410.
           03 WRK-MESU9410-ENTRADA.
              05 WRK-MESU9410-BANCO    PIC 9(003) COMP-3   VALUE 237.
              05 WRK-MESU9410-AGENCIA  PIC 9(005) COMP-3   VALUE ZEROS.
           03 WRK-MESU9410-SAIDA.
              05 WRK-MESU9410-RETORNO  PIC 9(002)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-CADACLIE             PIC X(02)           VALUE SPACES.
       01  WRK-FS-CLIEADES             PIC X(02)           VALUE SPACES.
       01  WRK-FS-CLIEINVA             PIC X(02)           VALUE SPACES.
MAI10  01  WRK-FS-CLIPSCCE             PIC X(02)           VALUE SPACES.
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

MAI10 *----------------------------------------------------------------*
MAI10  01  FILLER                      PIC X(32)           VALUE
MAI10      '*     AREA LAYOUT CLIPSCCE     *'.
MAI10 *----------------------------------------------------------------*
MAI10
MAI10  COPY 'GFCTWANG'.
MAI10
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT2100   *'.
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
                    WRK-FS-CADACLIE    EQUAL '10'.

           PERFORM 1400-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT CADACLIE
               OUTPUT CLIEADES
MAI10                 CLIPSCCE
                      CLIEINVA.

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

           PERFORM 0300-TESTAR-FS-CADACLIE.

           PERFORM 0400-TESTAR-FS-CLIEADES.

           PERFORM 0500-TESTAR-FS-CLIEINVA.

MAI10      PERFORM 0600-TESTAR-FS-CLIPSCCE.
MAI10
      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO CADACLIE                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-CADACLIE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADACLIE         NOT EQUAL '00'
               MOVE 'CADACLIE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CADACLIE    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO CLIEADES                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-CLIEADES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEADES         NOT EQUAL '00'
               MOVE 'CLIEADES'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CLIEADES    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO CLIEINVA                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-CLIEINVA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEINVA         NOT EQUAL '00'
               MOVE 'CLIEINVA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CLIEINVA    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

MAI10 *----------------------------------------------------------------*
MAI10 *    TESTAR FILE-STATUS DO ARQUIVO CLIPSCCE                      *
MAI10 *----------------------------------------------------------------*
MAI10  0600-TESTAR-FS-CLIPSCCE         SECTION.
MAI10 *----------------------------------------------------------------*
MAI10
MAI10      IF  WRK-FS-CLIPSCCE         NOT EQUAL '00'
MAI10          MOVE 'CLIPSCCE'         TO WRK-NOME-ARQ
MAI10          MOVE WRK-FS-CLIPSCCE    TO WRK-FS
MAI10          MOVE WRK-ERRO-FS        TO ERR-TEXTO
MAI10          PERFORM 9999-ROTINA-ERRO
MAI10      END-IF.
MAI10
MAI10 *----------------------------------------------------------------*
MAI10  0600-99-FIM.                    EXIT.
MAI10 *----------------------------------------------------------------*
MAI10
      *----------------------------------------------------------------*
      *    ROTINA P/ VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS  *
      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0900-LER-CADACLIE.

           IF  WRK-FS-CADACLIE            EQUAL '10'
               DISPLAY '*********** GFCT2100 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO CADACLIE VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2100 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO CADACLIE                                 *
      *----------------------------------------------------------------*
       0900-LER-CADACLIE               SECTION.
      *----------------------------------------------------------------*

           READ CADACLIE.

           IF  WRK-FS-CADACLIE          EQUAL '10'
               GO                       TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA             TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-CADACLIE.

           MOVE CAD-AGENCIA OF CADACLIE TO WRK-AGENCIA-ATU.

           ADD 1                        TO ACU-LIDOS-CADACLIE.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF WRK-AGENCIA-ATU NOT GREATER ZEROS
               MOVE 'N' TO WRK-FLAG-AGENCIA
           ELSE
               IF WRK-AGENCIA-ATU NOT EQUAL WRK-AGENCIA-ANT
                   PERFORM 1100-VALIDAR-AGENCIA-MESU9410
                   MOVE WRK-AGENCIA-ATU TO WRK-AGENCIA-ANT
               END-IF
           END-IF.

           IF WRK-FLAG-AGENCIA EQUAL 'S'
               PERFORM 1200-GRAVAR-CLIEADES
MAI10          PERFORM 1350-GRAVAR-CLIPSCCE
           ELSE
               PERFORM 1300-GRAVAR-CLIEINVA
           END-IF

           PERFORM 0900-LER-CADACLIE.

      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    MODULO DE VALIDACAO DE JUNCOES                              *
      *----------------------------------------------------------------*
       1100-VALIDAR-AGENCIA-MESU9410            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA-ATU             TO WRK-MESU9410-AGENCIA.
           MOVE 'MESU9410'                  TO WRK-MODULO.

           CALL  WRK-MODULO                 USING WRK-AREA-MESU9410.

           IF RETURN-CODE NOT EQUAL ZEROS
               DISPLAY '*********** GFCT2100 ***********'
               DISPLAY '*                              *'
               DISPLAY '*       ERRO NO ACESSO AO      *'
               DISPLAY '*        MODULO MESU9410       *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2100 ***********'
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF WRK-MESU9410-RETORNO EQUAL 99
               DISPLAY '*********** GFCT2100 ***********'
               DISPLAY '*                              *'
               DISPLAY '*       ERRO NA MESU9410       *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2100 ***********'
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF WRK-MESU9410-RETORNO(1:1) NOT EQUAL 0
               MOVE 'N' TO WRK-FLAG-AGENCIA
           ELSE
               MOVE 'S' TO WRK-FLAG-AGENCIA
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  CLIEADES                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-CLIEADES            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO                TO WRK-OPERACAO.

           MOVE SPACES                      TO GFCTRU-ENTRADA.
           MOVE CAD-AGENCIA    OF CADACLIE  TO GFCTRU-AGENCIA.
           MOVE CAD-RAZAO      OF CADACLIE  TO GFCTRU-RAZAO.
           MOVE CAD-CONTA      OF CADACLIE  TO GFCTRU-CONTA.
           MOVE CAD-PRINCIPAL  OF CADACLIE  TO GFCTRU-CGC-CPF.
           MOVE CAD-FILIAL     OF CADACLIE  TO GFCTRU-FILIAL.
           MOVE CAD-CTLE       OF CADACLIE  TO GFCTRU-CONTR.
           MOVE CAD-POSTO-SERV OF CADACLIE  TO GFCTRU-POSTO-SERV.
           MOVE CAD-DT-ABERT   OF CADACLIE  TO GFCTRU-DT-ABERT.

           WRITE GFCTRU-ENTRADA OF CLIEADES.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-CLIEADES.

           ADD 1                  TO ACU-GRAVADOS-CLIEADES.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  CLIEINVA                                    *
      *----------------------------------------------------------------*
       1300-GRAVAR-CLIEINVA            SECTION.
      *----------------------------------------------------------------*

           MOVE REG-CADACLIE  OF CADACLIE TO REG-CADACLIE OF CLIEINVA.

           WRITE REG-CADACLIE OF CLIEINVA.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-CLIEINVA.

           ADD 1                  TO ACU-DESPREZADOS.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

MAI10 *----------------------------------------------------------------*
MAI10 *    GRAVAR ARQUIVO  CLIPSCCE                                    *
MAI10 *----------------------------------------------------------------*
MAI10  1350-GRAVAR-CLIPSCCE            SECTION.
MAI10 *----------------------------------------------------------------*
MAI10
MAI10      MOVE CAD-AGENCIA    OF CADACLIE TO WANG-AGENCIA
MAI10      MOVE CAD-RAZAO      OF CADACLIE TO WANG-RAZAO
MAI10      MOVE CAD-CONTA      OF CADACLIE TO WANG-CONTA
MAI10
MAI10      MOVE CAD-DIG-CTA    OF CADACLIE TO WRK-DIG-CTA-X
MAI10      MOVE WRK-DIG-CTA-N              TO WANG-DIG-CTA
MAI10
MAI10      MOVE CAD-NOME-CLIE  OF CADACLIE TO WANG-NOME-CLIENTE
MAI10      MOVE CAD-PRINCIPAL  OF CADACLIE TO WANG-CGC-CPF
MAI10      MOVE CAD-FILIAL     OF CADACLIE TO WANG-FILIAL
MAI10      MOVE CAD-CTLE       OF CADACLIE TO WANG-CONTR
MAI10      MOVE CAD-POSTO-SERV OF CADACLIE TO WANG-POSTO-SERV
MAI10
MAI10      WRITE FD-REG-CLIPSCCE FROM GFCTWANG-ENTRADA
MAI10
MAI10      MOVE WRK-GRAVACAO      TO WRK-OPERACAO.
MAI10
MAI10      PERFORM 0600-TESTAR-FS-CLIPSCCE.
MAI10
MAI10      ADD 1                  TO ACU-GRAVADOS-CLIPSCCE.
MAI10
MAI10 *----------------------------------------------------------------*
MAI10  1350-99-FIM.                    EXIT.
MAI10 *----------------------------------------------------------------*
MAI10
      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1400-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1500-EMITIR-DISPLAY.

           CLOSE CADACLIE
                 CLIEADES
MAI10            CLIPSCCE
                 CLIEINVA.

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

           DISPLAY '******************** GFCT2100 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-CADACLIE     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM CADACLIE......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-CLIEADES  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO CLIEADES...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  CLIENTES DESPREZADOS...: '
                                                WRK-MASCARA  '       *'.

MAI10      MOVE ACU-GRAVADOS-CLIPSCCE  TO WRK-MASCARA.
MAI10      DISPLAY '*  GRAVADOS NO CLIPSCCE...: '
MAI10                                           WRK-MASCARA  '       *'.
MAI10      DISPLAY '*                                                *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2100 ********************'.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2100'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------

