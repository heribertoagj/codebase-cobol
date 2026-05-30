      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1161.
       AUTHOR.     WAGNER SILVA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1161                                    *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK / GP 50 *
      *    DATA........:   26/09/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR REGISTROS PARA COBRANCA ACUMULADA DOS *
      *                    REG. TIPO 52 FLEXIBILIZACAO PARTE EMPRESA   *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  ENTEMPRE                    I#GFCTIL          *
      *                  CTRLDATA                    I#GFCT06          *
      *                  ENTCONTR                    I#GFCTIX          *
      *                  ACUEMPRE                    I#GFCTIL          *
      *                  DETEMPRE                    I#GFCTIL          *
      *                  SAICONTR                    I#GFCTIX          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCT0M - AREA PARA ERROS NO ACESSO AO MODULO GFCT5531     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - CANCELAMENTO DO PROGRAMA                         *
      *    BRAD1050 - CONEXAO DO COBOL COM O DB2                       *
      *================================================================*

VJET  *================================================================*
VJET  *                     A L T E R A C A O                          *
VJET  *----------------------------------------------------------------*
VJET  *    ANALISTA....:   VINICIUS C. MADUREIRA - PROCWORK - GRUPO 50 *
VJET  *    DATA........:   28/12/2007                                  *
VJET  *    OBJETIVO....:   ALTERACAO DA FORMATACAO DA DATA DE EVENTO   *
VJET  *================================================================*

0108  *================================================================*
0108  *                     A L T E R A C A O                          *
0108  *----------------------------------------------------------------*
0108  *    ANALISTA....:   WAGNER                - PROCWORK - GRUPO 50 *
0108  *    DATA........:   10/01/2008                                  *
0108  *    OBJETIVO....:   ALTERACAO DA FORMATACAO DA DATA DE EVENTO   *
0108  *                    NO ARQUIVO DE ACUMULADOS.                   *
0108  *                    DEVE CONTER A QUANTIDADE DE EVENTOS ACUM.   *
0108  *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ENTEMPRE ASSIGN      TO UT-S-ENTEMPRE
           FILE STATUS                 IS WRK-FS-ENTEMPRE.

           SELECT CTRLDATA ASSIGN      TO UT-S-CTRLDATA
           FILE STATUS                 IS WRK-FS-CTRLDATA.

           SELECT ENTCONTR ASSIGN      TO UT-S-ENTCONTR
           FILE STATUS                 IS WRK-FS-ENTCONTR.

           SELECT ACUEMPRE ASSIGN      TO UT-S-ACUEMPRE
           FILE STATUS                 IS WRK-FS-ACUEMPRE.

           SELECT DETEMPRE ASSIGN      TO UT-S-DETEMPRE
           FILE STATUS                 IS WRK-FS-DETEMPRE.

           SELECT SAICONTR ASSIGN      TO UT-S-SAICONTR
           FILE STATUS                 IS WRK-FS-SAICONTR.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE EVENTOS A UNIFICAR                       *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ENTEMPRE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE CONTROLE DE DATAS GFCT                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   100             *
      *----------------------------------------------------------------*

       FD  CTRLDATA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT06'.

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE NUMERO DO ULTIMO CONTRATO EMITIDO        *
      *            ORG. SEQUENCIAL     -   LRECL   =   050             *
      *----------------------------------------------------------------*

       FD  ENTCONTR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTIX'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE REGISTROS UNIFICADOS PARA COBRANCA       *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ACUEMPRE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE EVENTOS ORIG. UNIFICADOS NO ACUEMPRE     *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  DETEMPRE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE NUMERO DO ULTIMO CONTRATO EMITIDO        *
      *            ORG. SEQUENCIAL     -   LRECL   =   050             *
      *----------------------------------------------------------------*

       FD  SAICONTR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTIX'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADOR ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ENTEMPRE        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-LIDOS-CTRLDATA        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-LIDOS-ENTCONTR        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-ACUEMPRE        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-DETEMPRE        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-SAICONTR        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-VALOR-DEB             PIC  9(011)V99 COMP-3 VALUE ZEROS.
       77  ACU-ACUMULADOS            PIC  9(011)    COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTE DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-ENTEMPRE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CTRLDATA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ENTCONTR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ACUEMPRE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-DETEMPRE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAICONTR             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-SEG-LINHA-EXTRATO       PIC  X(032)         VALUE SPACES.
       77  WRK-FIM-ENTEMPRE            PIC  X(001)         VALUE 'N'.
       77  WRK-FIM-PGM                 PIC  X(001)         VALUE 'N'.
       77  WRK-INI-PGM                 PIC  X(001)         VALUE 'S'.

*******77  WRK-DATA-EVENTO             PIC  9(008) VALUE 99999999.
0108   77  WRK-DATA-EVENTO             PIC  9(008) VALUE ZEROS.

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-DATA-DISPL              PIC  9(008)         VALUE ZEROS.
       77  WRK-DIGITO-DISPL            PIC  X(001)         VALUE SPACES.
       77  WRK-CONTRATO-DISPL          PIC  9(007)         VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-DATA-PROC               PIC  9(008)         VALUE ZEROS.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9.
       77  WRK-DIGITO                  PIC  X(036)         VALUE
           '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS AUXILIARES ***'.
      *---------------------------------------------------------------*

       01  WRK-MSG01                   PIC  X(044)         VALUE
           '** GFCT5531 - AMBIENTE DIFERENTE DE B E O **'.
       01  WRK-MSG03                   PIC  X(049)         VALUE
           '** GFCT5531 - PARAMETROS DE PESQUISA INVALIDOS **'.
       01  WRK-MSG04                   PIC  X(048)         VALUE
           '** GFCT5531 - PROGRAMA CHAMADOR NAO INFORMADO **'.
       01  WRK-MSG99                   PIC  X(025)         VALUE
           '** GFCT5531 - ERRO DB2 **'.

       01  WRK-MSG-EXTOURO.
           05  FILLER                  PIC  X(051)         VALUE
               '** QUANTIDADE DE CONTRATOS DO DIA EXCEDEU AO MAXIMO'.
           05  FILLER                  PIC  X(012)         VALUE
               ' POSSIVEL **'.

       01  WRK-MSG-GFCT5531.
           05  FILLER                  PIC  X(052)         VALUE
               '** ERRO NO ACESSO AO MODULO GFCT5531 - RETUN CODE = '.
           05  WRK-RETURN-CODE         PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA CHAVE DE ARQUIVO ***'.
      *---------------------------------------------------------------*

       01  WRK-CHAVE-ATUAL.
           05  WRK-CHV-AGENCIA-ATU     PIC  9(005)         VALUE ZEROS.
           05  WRK-CHV-CONTA-ATU       PIC  9(007)         VALUE ZEROS.
           05  WRK-CHV-TARIFA-ATU      PIC  9(005)         VALUE ZEROS.

       01  WRK-CHAVE-ANTER.
           05  WRK-CHV-AGENCIA-ANT     PIC  9(005)         VALUE ZEROS.
           05  WRK-CHV-CONTA-ANT       PIC  9(007)         VALUE ZEROS.
           05  WRK-CHV-TARIFA-ANT      PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGEM DE ERRO ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(018)         VALUE
               ' - FILE STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MODULO GFCT5531 ***'.
      *----------------------------------------------------------------*

       01  WRK-5531-AREA-ENTRADA.
           05  WRK-5531-AMBIENTE            PIC  X(001)    VALUE SPACES.
           05  WRK-5531-CHAMADOR            PIC  X(008)    VALUE SPACES.

       01  WRK-5531-AREA-SAIDA.
           05  WRK-5531-COD-RETORNO         PIC  9(002)    VALUE ZEROS.
           05  WRK-5531-COD-SQL-ERRO        PIC S9(003)    VALUE ZEROS.
           05  WRK-5531-COD-MSG-GFCT        PIC  9(004)    VALUE ZEROS.
           05  WRK-5531-DESC-MSG            PIC  X(070)    VALUE SPACES.
           05  WRK-5531-QTD-OCORR           PIC  9(005)    VALUE ZEROS.
           05  WRK-5531-DADOS-RETORNO.
             10  WRK-5531-CDEPDC-GSTOR      PIC  9(005)    VALUE ZEROS.
             10  WRK-5531-DINIC-VGCIA-GSTOR PIC  X(010)    VALUE SPACES.
             10  WRK-5531-DFIM-VGCIA-GSTOR  PIC  X(010)    VALUE SPACES.
             10  WRK-5531-QMAX-TENTV-GSTOR  PIC  9(003)    VALUE ZEROS.
             10  WRK-5531-QMAX-FLEXZ        PIC  9(005)    VALUE ZEROS.
             10  WRK-5531-QMAX-RTCAO        PIC  9(003)    VALUE ZEROS.
             10  WRK-5531-QMAX-CAREN        PIC  9(003)    VALUE ZEROS.
             10  WRK-5531-QMES-EXCL-TARIF   PIC  9(002)    VALUE ZEROS.
             10  WRK-5531-QMES-EXCL-PARM    PIC  9(002)    VALUE ZEROS.
             10  WRK-5531-QMAX-PAREC-ESTRN  PIC  9(002)    VALUE ZEROS.
             10  WRK-5531-CFUNC-BDSCO       PIC  9(009)    VALUE ZEROS.
             10  WRK-5531-CTERM-MANUT       PIC  X(008)    VALUE SPACES.
             10  WRK-5531-HMANUT-REG        PIC  X(026)    VALUE SPACES.
             10  WRK-5531-CSIT-ONLINE       PIC  9(001)    VALUE ZEROS.
             10  WRK-5531-QMAX-EXCL-FLEXZ   PIC  9(002)    VALUE ZEROS.
             10  WRK-5531-QMAX-FRANQ-TARIF  PIC  9(003)    VALUE ZEROS.
             10  WRK-5531-CIDTFD-LCTO       PIC  9(005)    VALUE ZEROS.
             10  WRK-5531-CCART-DEB-UNFCA   PIC  9(005)    VALUE ZEROS.
             10  WRK-5531-RSEGDA-LIN-UNFCA  PIC  X(032)    VALUE SPACES.

       01  WRK-IO-PCB.
           05  WRK-IO-LTERM                 PIC  X(008)    VALUE SPACES.
           05  FILLER                       PIC  X(002)    VALUE SPACES.
           05  WRK-IO-STATUS                PIC  X(002)    VALUE SPACES.
           05  FILLER                       PIC  X(012)    VALUE SPACES.
           05  WRK-IO-MODNAME               PIC  X(008)    VALUE SPACES.

       01  WRK-ALT-PCB.
           05  WRK-ALT-LTERM                PIC  X(008)    VALUE SPACES.
           05  FILLER                       PIC  X(002)    VALUE SPACES.
           05  WRK-ALT-STATUS               PIC  X(002)    VALUE SPACES.
           05  FILLER                       PIC  X(012)    VALUE SPACES.
           05  WRK-ALT-MODNAME              PIC  X(008)    VALUE SPACES.

       COPY 'I#GFCT0M'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TRATAMENTO DE ERRO ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB052
           END-EXEC.

           EXEC SQL
               DECLARE CSR-1 CURSOR FOR
               SELECT  RSEGDA_LIN_EXTRT,
                       HIDTFD_FLEXZ_AGPTO
               FROM    DB2PRD.FLEXZ_GRP_EMPR
               WHERE   CSERVC_TARIF      = :GFCTB052.CSERVC-TARIF
               AND     CAGPTO_CTA        = :GFCTB052.CAGPTO-CTA
               AND     CJUNC_DEPDC       = :GFCTB052.CJUNC-DEPDC
               AND     CCTA_CLI          = :GFCTB052.CCTA-CLI
               ORDER   BY HIDTFD_FLEXZ_AGPTO DESC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      * ROTINA PRINCIPAL.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR   UNTIL
                      WRK-FIM-PGM   EQUAL 'S'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ABERTURA DE ARQUIVO E INICIALIZACAO DE VARIAVEIS.  *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           OPEN INPUT  ENTEMPRE
                       CTRLDATA
                       ENTCONTR
                OUTPUT ACUEMPRE
                       DETEMPRE
                       SAICONTR.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTES DE FILES-STATUS.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ENTEMPRE.

           PERFORM 1120-TESTAR-FS-CTRLDATA.

           PERFORM 1130-TESTAR-FS-ENTCONTR.

           PERFORM 1140-TESTAR-FS-ACUEMPRE.

           PERFORM 1150-TESTAR-FS-DETEMPRE.

           PERFORM 1160-TESTAR-FS-SAICONTR.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO ENTEMPRE.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ENTEMPRE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTEMPRE         NOT EQUAL '00'
               MOVE 'ENTEMPRE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTEMPRE    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO CTRLDATA.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-CTRLDATA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTRLDATA         NOT EQUAL '00'
               MOVE 'CTRLDATA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CTRLDATA    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO ENTCONTR.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-ENTCONTR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTCONTR         NOT EQUAL '00'
               MOVE 'ENTCONTR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTCONTR    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO ACUEMPRE.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-ACUEMPRE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ACUEMPRE         NOT EQUAL '00'
               MOVE 'ACUEMPRE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ACUEMPRE    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO DETEMPRE.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1150-TESTAR-FS-DETEMPRE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DETEMPRE         NOT EQUAL '00'
               MOVE 'DETEMPRE'         TO WRK-NOME-ARQ
               MOVE WRK-FS-DETEMPRE    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO SAICONTR.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1160-TESTAR-FS-SAICONTR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAICONTR         NOT EQUAL '00'
               MOVE 'SAICONTR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SAICONTR    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR SE ARQUIVOS DE ENTRADA ESTAO VAZIOS.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CTRLDATA.

           IF  WRK-FS-CTRLDATA             EQUAL '10'
               DISPLAY '*************** GFCT1161 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*        ARQUIVO CTRLDATA VAZIO        *'
               DISPLAY '*                                      *'
               DISPLAY '*       PROCESSAMENTO  ENCERRADO       *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT1161 ***************'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2200-LER-ENTCONTR.

           IF  WRK-FS-ENTCONTR             EQUAL '10'
               DISPLAY '*************** GFCT1161 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*        ARQUIVO ENTCONTR VAZIO        *'
               DISPLAY '*                                      *'
               DISPLAY '*       PROCESSAMENTO  ENCERRADO       *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT1161 ***************'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2300-LER-ENTEMPRE.

           IF  WRK-FS-ENTEMPRE             EQUAL '10'
               DISPLAY '*************** GFCT1161 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*        ARQUIVO ENTEMPRE VAZIO        *'
               DISPLAY '*                                      *'
               DISPLAY '*       PROCESSAMENTO  ENCERRADO       *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT1161 ***************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA LEITURA DO ARQUIVO CTRLDATA. BUSCAR DATA PROCESSA- *
      *    MENTO.                                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CTRLDATA               SECTION.
      *----------------------------------------------------------------*

           READ CTRLDATA.

           IF  WRK-FS-CTRLDATA             EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA                TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-CTRLDATA.

           ADD 1                           TO ACU-LIDOS-CTRLDATA.

           MOVE  GFCT06-DPROCM-ATUAL(1:2)  TO WRK-DATA-PROC(7:2).
           MOVE  GFCT06-DPROCM-ATUAL(4:2)  TO WRK-DATA-PROC(5:2).
           MOVE  GFCT06-DPROCM-ATUAL(7:4)  TO WRK-DATA-PROC(1:4).

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA LEITURA DO ARQUIVO ENTCONTR. BUSCAR ULTIMO CONTRATO*
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-ENTCONTR               SECTION.
      *----------------------------------------------------------------*

           READ ENTCONTR.

           IF  WRK-FS-ENTCONTR         EQUAL '10'
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-ENTCONTR.

           ADD 1                       TO ACU-LIDOS-ENTCONTR.

           IF  WRK-DATA-PROC           NOT EQUAL CONT-DATA OF ENTCONTR
               MOVE  WRK-DATA-PROC     TO CONT-DATA        OF ENTCONTR
           END-IF.

           EVALUATE  CONT-DIGITO OF ENTCONTR
               WHEN '0'
                   MOVE  1             TO IND-1
               WHEN '1'
                   MOVE  2             TO IND-1
               WHEN '2'
                   MOVE  3             TO IND-1
               WHEN '3'
                   MOVE  4             TO IND-1
               WHEN '4'
                   MOVE  5             TO IND-1
               WHEN '5'
                   MOVE  6             TO IND-1
               WHEN '6'
                   MOVE  7             TO IND-1
               WHEN '7'
                   MOVE  8             TO IND-1
               WHEN '8'
                   MOVE  9             TO IND-1
               WHEN '9'
                   MOVE  10            TO IND-1
               WHEN 'A'
                   MOVE  11            TO IND-1
               WHEN 'B'
                   MOVE  12            TO IND-1
               WHEN 'C'
                   MOVE  13            TO IND-1
               WHEN 'D'
                   MOVE  14            TO IND-1
               WHEN 'E'
                   MOVE  15            TO IND-1
               WHEN 'F'
                   MOVE  16            TO IND-1
               WHEN 'G'
                   MOVE  17            TO IND-1
               WHEN 'H'
                   MOVE  18            TO IND-1
               WHEN 'I'
                   MOVE  19            TO IND-1
               WHEN 'J'
                   MOVE  20            TO IND-1
               WHEN 'K'
                   MOVE  21            TO IND-1
               WHEN 'L'
                   MOVE  22            TO IND-1
               WHEN 'M'
                   MOVE  23            TO IND-1
               WHEN 'N'
                   MOVE  24            TO IND-1
               WHEN 'O'
                   MOVE  25            TO IND-1
               WHEN 'P'
                   MOVE  26            TO IND-1
               WHEN 'Q'
                   MOVE  27            TO IND-1
               WHEN 'R'
                   MOVE  28            TO IND-1
               WHEN 'S'
                   MOVE  29            TO IND-1
               WHEN 'T'
                   MOVE  30            TO IND-1
               WHEN 'U'
                   MOVE  31            TO IND-1
               WHEN 'V'
                   MOVE  32            TO IND-1
               WHEN 'W'
                   MOVE  33            TO IND-1
               WHEN 'X'
                   MOVE  34            TO IND-1
               WHEN 'Y'
                   MOVE  35            TO IND-1
               WHEN 'Z'
                   MOVE  36            TO IND-1
           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA LEITURA DO ARQUIVO ENTEMPRE.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-ENTEMPRE               SECTION.
      *----------------------------------------------------------------*

           READ ENTEMPRE.

           IF  WRK-FS-ENTEMPRE         EQUAL '10'
               MOVE 'S'                   TO  WRK-FIM-ENTEMPRE
               GO TO 2300-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ENTEMPRE.

           ADD 1                       TO ACU-LIDOS-ENTEMPRE.

           MOVE  COBR-AGENCIA-DEB      OF ENTEMPRE
                                       TO WRK-CHV-AGENCIA-ATU.
           MOVE  COBR-CONTA-DEB        OF ENTEMPRE
                                       TO WRK-CHV-CONTA-ATU.
           MOVE  COBR-TARIFA-ORIG      OF ENTEMPRE
                                       TO WRK-CHV-TARIFA-ATU.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA GERAR REGISTRO ACUMULADO PARA COBRANCA UNIFICADA.  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

      *------------>> QUEBRA FINAL
           IF  WRK-FIM-ENTEMPRE         = 'S'     AND
               ACU-ACUMULADOS     GREATER  ZEROS
               PERFORM 3210-GRAVAR-ACUEMPRE
               MOVE 'S'                TO WRK-FIM-PGM
               GO                      TO 3000-99-FIM
           END-IF.

      *------------>> QUEBRA CHAVE
           IF  WRK-CHAVE-ATUAL      NOT = WRK-CHAVE-ANTER
               PERFORM 3205-BUSCAR-SEG-LINHA
               PERFORM 3210-GRAVAR-ACUEMPRE
               PERFORM 3200-PREPARA-ACUEMPRE
               MOVE  WRK-CHAVE-ATUAL   TO WRK-CHAVE-ANTER
               MOVE  ZEROS             TO ACU-ACUMULADOS
0108           MOVE  ZEROS             TO WRK-DATA-EVENTO
           END-IF.

      *------------>> ACUMULA VALOR E GRAVA DETEMPRE

           MOVE  REG-COBRANCA          OF ENTEMPRE
                                       TO REG-COBRANCA      OF DETEMPRE.

           MOVE  WRK-SEG-LINHA-EXTRATO TO COBR-SEG-LINHA-EXTRATO
                                                            OF DETEMPRE.

           MOVE  10                   TO COBR-TIPO-COBRANCA OF DETEMPRE.

           MOVE  COBR-DIG-CONTRATO     OF ACUEMPRE
                                       TO COBR-DIG-CONTRATO OF DETEMPRE.
           MOVE  COBR-NUM-CONTRATO     OF ACUEMPRE
                                       TO COBR-NUM-CONTRATO OF DETEMPRE.
           ADD   COBR-VALOR-DEBITAR    OF DETEMPRE
                                       TO ACU-VALOR-DEB.

VJET  *------------>> ALTERACAO DA FORMATACAO DA DATA DE EVENTO.
VJET       IF COBR-DATA-EVNTO-ORIG OF ENTEMPRE  LESS WRK-DATA-EVENTO
VJET           MOVE COBR-DATA-EVNTO-ORIG OF ENTEMPRE TO WRK-DATA-EVENTO
VJET       END-IF.
VJET  *------------>> SUBSTITUIDO 0108
0108       ADD 1                       TO WRK-DATA-EVENTO.

           WRITE REG-COBRANCA          OF DETEMPRE.
           MOVE  WRK-GRAVACAO          TO WRK-OPERACAO.
           PERFORM 1150-TESTAR-FS-DETEMPRE.
           ADD 1                       TO ACU-GRAVA-DETEMPRE
                                          ACU-ACUMULADOS.

           PERFORM 2300-LER-ENTEMPRE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR REGISTROS UNIFICADOS.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-PREPARA-ACUEMPRE           SECTION.
      *----------------------------------------------------------------*

      *--------------->> MOVE DADOS FIXOS
           MOVE  REG-COBRANCA  OF ENTEMPRE TO REG-COBRANCA OF ACUEMPRE.

           MOVE  WRK-SEG-LINHA-EXTRATO TO COBR-SEG-LINHA-EXTRATO
                                                           OF ACUEMPRE.

           INITIALIZE COBR-CHAVE-ORIGEM    OF ACUEMPRE.

           MOVE  9                         TO COBR-TIPO-COBRANCA
                                                           OF ACUEMPRE.

      *--------------->> CALCULA NUMERO DO CONTRATO E MOVE PARA ACUEMPRE
           IF  CONT-DIGITO   OF ENTCONTR   EQUAL 'Z' AND
               CONT-CONTRATO OF ENTCONTR   EQUAL 9999999
               MOVE  1                     TO IND-1
               MOVE  WRK-DIGITO(IND-1:1)   TO CONT-DIGITO   OF ENTCONTR
               MOVE  0                     TO CONT-CONTRATO OF ENTCONTR
           END-IF.

           IF  CONT-CONTRATO OF ENTCONTR   EQUAL 9999999
               ADD   1                     TO IND-1
               IF    WRK-DIGITO(IND-1:1)    = 'N'
                     ADD 1                 TO IND-1
               END-IF
               MOVE  WRK-DIGITO(IND-1:1)   TO CONT-DIGITO   OF ENTCONTR
               MOVE  1                     TO CONT-CONTRATO OF ENTCONTR
           ELSE
               ADD   1                     TO CONT-CONTRATO OF ENTCONTR
           END-IF.

           MOVE  CONT-DIGITO   OF ENTCONTR TO COBR-DIG-CONTRATO
                                                            OF ACUEMPRE.
           MOVE  CONT-CONTRATO OF ENTCONTR TO COBR-NUM-CONTRATO
                                                            OF ACUEMPRE.

VJET  *------------>> ALTERACAO DA FORMATACAO DA DATA DE EVENTO.
VJET  *    MOVE COBR-DATA-EVNTO-ORIG OF ENTEMPRE TO WRK-DATA-EVENTO.
VJET  *------------>> RETIRADO EM 0108

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR REGISTROS UNIFICADOS.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3205-BUSCAR-SEG-LINHA           SECTION.
      *----------------------------------------------------------------*

      *-------------->> BUSCAR SEGUNDA LINHA DE EXTRATO NA TAB. GFCTB052

           MOVE  COBR-TARIFA-ORIG  OF ENTEMPRE TO CSERVC-TARIF.
           MOVE  COBR-AGENCIA-DEB  OF ENTEMPRE TO CJUNC-DEPDC.
           MOVE  COBR-CONTA-DEB    OF ENTEMPRE TO CCTA-CLI.
           MOVE  16                            TO CAGPTO-CTA.

           EXEC SQL
               OPEN CSR-1
           END-EXEC.
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_EMPR  ' TO ERR-DBD-TAB
               MOVE 'OPEN      '       TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           EXEC SQL
               FETCH    CSR-1
               INTO    :GFCTB052.RSEGDA-LIN-EXTRT,
                       :GFCTB052.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF SQLCODE                  EQUAL ZEROS
              MOVE RSEGDA-LIN-EXTRT    TO    WRK-SEG-LINHA-EXTRATO
           ELSE
           IF SQLCODE                  EQUAL +100
              MOVE SPACES              TO    WRK-SEG-LINHA-EXTRATO
           ELSE
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_EMPR  ' TO ERR-DBD-TAB
               MOVE 'FETCH     '       TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           EXEC SQL
               CLOSE CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'FLEXZ_GRP_EMPR  ' TO ERR-DBD-TAB
               MOVE 'CLOSE     '       TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3205-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3210-GRAVAR-ACUEMPRE            SECTION.
      *----------------------------------------------------------------*
      *-------------->> GRAVA REGISTRO ACUMULADO TIPO 52

      *-------------->> NAO GRAVA NA PRIMEIRA VEZ
           IF    WRK-INI-PGM            = 'S'
                 MOVE 'N'                 TO WRK-INI-PGM
                 INITIALIZE REG-COBRANCA  OF ACUEMPRE
                 MOVE  WRK-CHAVE-ATUAL    TO WRK-CHAVE-ANTER
                 GO                       TO 3210-99-FIM
           END-IF.

      *-------------->> GRAVACAO
           MOVE  ACU-VALOR-DEB         TO COBR-VALOR-DEBITAR
                                                            OF ACUEMPRE.

           IF  COBR-TIPO-ARQUIVO OF ACUEMPRE EQUAL 2
               MOVE  1                 TO COBR-TIPO-ARQUIVO OF ACUEMPRE
           END-IF.

VJET  *------------>> ALTERACAO DA FORMATACAO DA DATA DE EVENTO.
VJET       MOVE WRK-DATA-EVENTO TO COBR-DATA-EVNTO-ORIG OF ACUEMPRE.
VJET  *------------>> OK 0108

           WRITE REG-COBRANCA          OF ACUEMPRE.
           MOVE  WRK-GRAVACAO          TO WRK-OPERACAO.
           PERFORM 1140-TESTAR-FS-ACUEMPRE.
           ADD 1                       TO ACU-GRAVA-ACUEMPRE.

      *-------------->> REINICIO
           INITIALIZE REG-COBRANCA     OF ACUEMPRE.
           MOVE       ZEROS            TO ACU-VALOR-DEB.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA EMITIR DISPLAY QUANDO QUANTIDADE DE CONTRATO DO DIA*
      *    EXCEDER AO MAXIMO POSSIVEL.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3221-DISPLAY-EXTOURO            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************************** GFCT1161 ****************
      -    '*********'.
           DISPLAY '*
      -            '        *'.
           DISPLAY '* QUANTIDADE DE CONTRATOS DO DIA EXCEDEU AO MAXIMO P
      -            'OSSIVEL *'.
           DISPLAY '*
      -            '        *'.
           DISPLAY '* REG-CONTRATO.....:                  '
            CONT-DATA OF ENTCONTR ' , ' CONT-DIGITO OF ENTCONTR ', '
            CONT-CONTRATO OF ENTCONTR ' *'.
           DISPLAY '* COBR-CHAVE-ORIGEM:       '
            COBR-CHAVE-ORIGEM OF ENTEMPRE ' *'.

           MOVE  ACU-GRAVA-ACUEMPRE    TO WRK-MASCARA.

           DISPLAY '* GRAVADOS-SAIUNIF:                            '
                    WRK-MASCARA ' *'.
           DISPLAY '*
      -            '        *'.
           DISPLAY '************************** GFCT1161 ****************
      -            '*********'.

           PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       3221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO NORMAL DO PROGRAMA.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

      *    IF  ACU-LIDOS-ENTEMPRE      GREATER ZEROS
               PERFORM 4100-GRAVAR-SAICONTR
      *    END-IF.

           PERFORM 4200-EMITIR-ESTATISTICAS.

           CLOSE ENTEMPRE
                 CTRLDATA
                 ENTCONTR
                 ACUEMPRE
                 DETEMPRE
                 SAICONTR.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ATUALIZAR ARQUIVO COM ULTIMO CONTRATO GRAVADO.     *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-GRAVAR-SAICONTR            SECTION.
      *----------------------------------------------------------------*

           MOVE  REG-CONTRATO   OF ENTCONTR TO
                                               REG-CONTRATO OF SAICONTR.

           MOVE  CONT-DATA      OF SAICONTR TO WRK-DATA-DISPL.
           MOVE  CONT-DIGITO    OF SAICONTR TO WRK-DIGITO-DISPL.
           MOVE  CONT-CONTRATO  OF SAICONTR TO WRK-CONTRATO-DISPL.

           WRITE REG-CONTRATO          OF SAICONTR.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1160-TESTAR-FS-SAICONTR.
           ADD 1                       TO ACU-GRAVA-SAICONTR.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA EMISSAO DE DISPLAY                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-ESTATISTICAS        SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-ENTEMPRE     TO WRK-MASCARA.

           DISPLAY '****************** GFCT1161 ******************'.
           DISPLAY '*                                            *'.
           DISPLAY '* TOTAL DE REGS. LIDOS ENTEMPRE: ' WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-CTRLDATA     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. LIDOS CTRLDATA: ' WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-ENTCONTR     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. LIDOS ENTCONTR: ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-ACUEMPRE     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. GRAVA ACUEMPRE: ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-DETEMPRE     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. GRAVA DETEMPRE: ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-SAICONTR     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. GRAVA SAICONTR: ' WRK-MASCARA ' *'.
           DISPLAY '*                                            *'.

           DISPLAY '* REG-CONTRATO........: ' WRK-DATA-DISPL ', '
                      WRK-DIGITO-DISPL ', ' WRK-CONTRATO-DISPL ' *'.
           DISPLAY '*                                            *'.
           DISPLAY '****************** GFCT1161 ******************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CANCELAMENTO DO PROGRAMA QUANDO HOUVER ALGUM ERRO  *
      *    NO DECORRER DO PROCESSAMENTO.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1161'             TO ERR-PGM.
           MOVE  04                    TO RETURN-CODE.


           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           ELSE
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

