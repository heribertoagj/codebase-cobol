      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1174.
       AUTHOR.     MARCELO FARIA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1174                                    *
      *    PROGRAMADOR.:   MARCELO FARIA           - CPM/PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE SOMMER           - CPM/PATO BRANCO   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK / GP 50 *
      *    DATA........:   16/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GRAVAR REGISTROS PARA COBRANCA NORMAL, CON- *
      *      SOLIDADA E UNIFICADA.                                     *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  ENTCOBRA                    I#GFCTIL          *
      *                  CTRLDATA                    I#GFCT06          *
      *                  ENTCONTR                    I#GFCTIX          *
      *                  SAINDIVI                    I#GFCTIL          *
      *                  SAIUNIFI                    I#GFCTIL          *
      *                  SAICONSL                    I#GFCTIL          *
      *                  SAICONTR                    I#GFCTIX          *
      *                  SAIAGRUP                    I#GFCTIL          *
      *                  ARQOCORR                    I#GFCTIL          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA CHAMADA POOL7100 - CANCELAMENTO.            *
      *    I#GFCT0M - AREA PARA ERROS NO ACESSO AOS MODULOS            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5530 - VERIFICA UNIFICACAO AG/CONTA                     *
      *    GFCT5538 - VERIFICA UNIFICACAO CPF/CNPJ                     *
      *    POOL0081 - MODULO PARA CHAMADA DINAMICA DE MODULOS          *
      *    POOL7100 - CANCELAMENTO DO PROGRAMA                         *
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR.:   FERNANDO NAIM SCHMITZ   - CPM/FPOLIS        *
      *    ANALISTA CPM:   MARCIO RODRIGO DA CUNHA - CPM/FPOLIS        *
      *    ANALISTA....:   VAGNER SILVA - PROCWORK - GRUPO 50          *
      *    DATA........:   14/12/2006                                  *
      *                                                                *
      *    OBJETIVO....:   GRAVAR O ARQUIVO SAIAGRUP QUANDO            *
      *      TIPO-COBRANCA = 8.                                        *
      *                                                                *
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA....:   WAGNER SILVA - PROCWORK - GRUPO 50          *
      *    DATA........:   12/02/2007                                  *
      *                                                                *
      *    OBJETIVO....:   INCLUIR OPCAO DE DEBITO PARCIAL NO ULTIMO   *
      *                    REPIQUE                                     *
      *                                                                *
      *================================================================*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                TABLE                       INCLUDE/BOOK        *
      *                    DB2PRD.MOVTO_EVNTO_CRRTT  GFCTB092          *
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA....:   WAGNER SILVA - PROCWORK - GRUPO 50          *
      *    DATA........:   20/03/2007                                  *
      *                                                                *
      *    OBJETIVO....:   SUBSTITUIR POOL0081 POR P00L0080            *
      *                                                                *
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA....:   WAGNER SILVA - PROCWORK - GRUPO 50          *
      *    DATA........:   30/03/2007                                  *
      *                                                                *
      *    OBJETIVO....:   DEIXAR DE ZERAR DIARIAMENTE O NUMERO DO     *
      *                    CONTRATO.                                   *
      *                                                                *
070615*================================================================*
070615*                     A L T E R A C A O                          *
070215*----------------------------------------------------------------*
070615*    ANALISTA....:   WAGNER SILVA - PROCWORK - GRUPO 50          *
070615*    DATA........:   15/06/2007                                  *
070615*    OBJETIVO....:   NAO EMITIR CONTRATO COM DIGITO N            *
070615*================================================================*
BI0813*                     A L T E R A C A O                          *
BI0813*----------------------------------------------------------------*
BI0813*    ANALISTA....:   UBIRAJARA    - PROCWORK - GRUPO 45          *
BI0813*    DATA........:   16/08/2013                                  *
BI0813*    OBJETIVO....:   AJUSTES NA COBRANCA PARCIAL DO ULTIMO       *
BI0813*                    REPIQUE.                                    *
BI0813*                                                                *
      *----------------------------------------------------------------*
      *                 U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
      *        SONDA IT       - CONSULTORIA - ALTERACAO - BI1113       *
      *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA....:  UBIRAJARA          - SONDA IT                *
      *    DATA........:  NOV/2013                                     *
      *    OBJETIVO....:  INCLUIR TRATAMENTO DE TIPO DE CONTA = 3      *
      *                   ( CONTA BENEFICIO ).                         *
      *                                                                *
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

           SELECT ENTCOBRA ASSIGN      TO UT-S-ENTCOBRA
           FILE STATUS                 IS WRK-FS-ENTCOBRA.

           SELECT CTRLDATA ASSIGN      TO UT-S-CTRLDATA
           FILE STATUS                 IS WRK-FS-CTRLDATA.

           SELECT ENTCONTR ASSIGN      TO UT-S-ENTCONTR
           FILE STATUS                 IS WRK-FS-ENTCONTR.

           SELECT SAINDIVI ASSIGN      TO UT-S-SAINDIVI
           FILE STATUS                 IS WRK-FS-SAINDIVI.

           SELECT SAIUNIFI ASSIGN      TO UT-S-SAIUNIFI
           FILE STATUS                 IS WRK-FS-SAIUNIFI.

           SELECT SAICONSL ASSIGN      TO UT-S-SAICONSL
           FILE STATUS                 IS WRK-FS-SAICONSL.

           SELECT SAICONTR ASSIGN      TO UT-S-SAICONTR
           FILE STATUS                 IS WRK-FS-SAICONTR.

           SELECT ARQOCORR ASSIGN      TO UT-S-ARQOCORR
           FILE STATUS                 IS WRK-FS-ARQOCORR.

           SELECT SAIAGRUP ASSIGN      TO UT-S-SAIAGRUP
           FILE STATUS                 IS WRK-FS-SAIAGRUP.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE EVENTOS A COBRAR MORA / POUP             *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ENTCOBRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTIL.

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE CONTROLE DE DATAS GFCT                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   100             *
      *----------------------------------------------------------------*

       FD  CTRLDATA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCT06.

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE NUMERO DO ULTIMO CONTRATO EMITIDO        *
      *            ORG. SEQUENCIAL     -   LRECL   =   050             *
      *----------------------------------------------------------------*

       FD  ENTCONTR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTIX.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE EVENTOS NAO UNIFICADOS A COBRAR          *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  SAINDIVI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTIL.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE EVENTOS A UNIFICAR                       *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  SAIUNIFI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTIL.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE EVENTOS A UNIFICAR                       *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  SAICONSL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTIL.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE NUMERO DO ULTIMO CONTRATO EMITIDO        *
      *            ORG. SEQUENCIAL     -   LRECL   =   050             *
      *----------------------------------------------------------------*

       FD  SAICONTR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTIX.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE ERROS DE PROCESSAMENTO                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ARQOCORR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTIL.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE SAIDA COM REGISTROS A AGRUPAR            *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  SAIAGRUP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTIL.

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

       77  ACU-LIDOS-ENTCOBRA        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-LIDOS-CTRLDATA        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-LIDOS-ENTCONTR        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-SAINDIVI        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-SAIUNIFI        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-SAICONSL        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-SAICONTR        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-ARQOCORR        PIC  9(009)    COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-SAIAGRUP        PIC  9(009)    COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTE DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-ENTCOBRA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CTRLDATA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ENTCONTR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAINDIVI             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAIUNIFI             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAICONSL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAICONTR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQOCORR             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAIAGRUP             PIC  X(002)         VALUE SPACES.

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

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-PROCESS-OK              PIC  X(001)         VALUE SPACES.
       77  WRK-DATA-DISPL              PIC  9(008)         VALUE ZEROS.
       77  WRK-DIGITO-DISPL            PIC  X(001)         VALUE SPACES.
       77  WRK-CONTRATO-DISPL          PIC  9(007)         VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-DATA-PROC               PIC  9(008)         VALUE ZEROS.
BI1113 77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
       77  WRK-RECURSO                 PIC  X(008)         VALUE SPACES.
       77  WRK-DESCRICAO               PIC  X(050)         VALUE SPACES.
       77  WRK-RETORNO                 PIC  X(003)         VALUE SPACES.
       77  WRK-TIPO-COBRANCA           PIC  9(002)         VALUE ZEROS.
       77  WRK-DIGITO                  PIC  X(036)         VALUE
           '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-MSG01                   PIC  X(033)         VALUE
           '** AMBIENTE DIFERENTE DE B E O **'.
       77  WRK-MSG03                   PIC  X(038)         VALUE
           '** PARAMETROS DE PESQUISA INVALIDOS **'.
       77  WRK-MSG04                   PIC  X(037)         VALUE
           '** PROGRAMA CHAMADOR NAO INFORMADO **'.
       77  WRK-MSG99                   PIC  X(014)         VALUE
           '** ERRO DB2 **'.

       01  WRK-MSG-EXTOURO.
           05  FILLER                  PIC  X(051)         VALUE
               '** QUANTIDADE DE CONTRATOS DO DIA EXCEDEU AO MAXIMO'.
           05  FILLER                  PIC  X(012)         VALUE
               ' POSSIVEL **'.

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

       01  WRK-ERRO-POOL7100.
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
           '*** AREA MODULO GFCT5530 ***'.
      *----------------------------------------------------------------*

       01  WRK-5530-AREA-ENTRADA.
           05 WRK-5530-AMBIENTE        PIC  X(001)   VALUE SPACES.
           05 WRK-5530-CHAMADOR        PIC  X(008)   VALUE SPACES.
           05 WRK-5530-AGENCIA         PIC  9(005)   VALUE ZEROS.
           05 WRK-5530-CONTA           PIC  9(007)   VALUE ZEROS.
           05 WRK-5530-TARIFA          PIC  9(005)   VALUE ZEROS.
           05 WRK-5530-DATA            PIC  X(010)   VALUE SPACES.

       01  WRK-5530-AREA-SAIDA.
           05 WRK-5530-COD-RETORNO     PIC 9(002)    VALUE ZEROS.
           05 WRK-5530-COD-SQL-ERRO    PIC S9(003)   VALUE ZEROS.
           05 WRK-5530-COD-MSG-GFCT    PIC  9(004)   VALUE ZEROS.
           05 WRK-5530-DESC-MSG        PIC  X(070)   VALUE SPACES.
           05 WRK-5530-QTD-OCORR       PIC  9(005)   VALUE ZEROS.
           05 WRK-5530-DADOS-RETORNO.
            10 WRK-5530-CJUNC-DEPDC    PIC  9(005)   VALUE ZEROS.
            10 WRK-5530-CCTA-CLI       PIC  9(007)   VALUE ZEROS.
            10 WRK-5530-CSERVC-TARIF   PIC  9(005)   VALUE ZEROS.
            10 WRK-5530-DATA-DEB-UNIC  PIC  X(010)   VALUE SPACES.
            10 WRK-5530-COBR-UNIC      PIC  X(001)   VALUE SPACES.

       01  WRK-IO-PCB.
           05 WRK-IO-LTERM             PIC  X(008)   VALUE SPACES.
           05 FILLER                   PIC  X(002)   VALUE SPACES.
           05 WRK-IO-STATUS            PIC  X(002)   VALUE SPACES.
           05 FILLER                   PIC  X(012)   VALUE SPACES.
           05 WRK-IO-MODNAME           PIC  X(008)   VALUE SPACES.

       01  WRK-ALT-PCB.
           05 WRK-ALT-LTERM            PIC  X(008)   VALUE SPACES.
           05 FILLER                   PIC  X(002)   VALUE SPACES.
           05 WRK-ALT-STATUS           PIC  X(002)   VALUE SPACES.
           05 FILLER                   PIC  X(012)   VALUE SPACES.
           05 WRK-ALT-MODNAME          PIC  X(008)   VALUE SPACES.

       COPY I#GFCT0M.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MODULO GFCT5538 ***'.
      *----------------------------------------------------------------*

       01  WRK-5538-AREA-ENTRADA.
           05  WRK-5538-AMBIENTE       PIC  X(001)   VALUE SPACES.
           05  WRK-5538-CHAMADOR       PIC  X(008)   VALUE SPACES.
           05  WRK-5538-CCGC-CPF       PIC  9(009)   VALUE ZEROS.
           05  WRK-5538-CFLIAL-CGC     PIC  9(005)   VALUE ZEROS.
           05  WRK-5538-CCTRL-CPF-CGC  PIC  9(002)   VALUE ZEROS.
           05  WRK-5538-CSERVC-TARIF   PIC  9(005)   VALUE ZEROS.

       01  WRK-5538-AREA-SAIDA.
           05  WRK-5538-COD-RETORNO    PIC 9(002)    VALUE ZEROS.
           05  WRK-5538-COD-SQL-ERRO   PIC S9(003)   VALUE ZEROS.
           05  WRK-5538-COD-MSG-GFCT   PIC  9(004)   VALUE ZEROS.
           05  WRK-5538-DESC-MSG       PIC  X(070)   VALUE SPACES.
           05  WRK-5538-QTD-OCORR      PIC  9(005)   VALUE ZEROS.
           05  WRK-5538-DADOS-RETORNO.
            10  WRK-5538-UNIFICADO     PIC  X(001)   VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TRATAMENTO DE ERRO ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
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

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-ENTCOBRA     EQUAL '10'.

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

           CALL 'POOL1050'.

           OPEN INPUT  ENTCOBRA
                       CTRLDATA
                       ENTCONTR
                OUTPUT SAINDIVI
                       SAIUNIFI
                       SAICONSL
                       SAICONTR
                       ARQOCORR
                       SAIAGRUP.

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

           PERFORM 1110-TESTAR-FS-ENTCOBRA.

           PERFORM 1120-TESTAR-FS-CTRLDATA.

           PERFORM 1130-TESTAR-FS-ENTCONTR.

           PERFORM 1140-TESTAR-FS-SAINDIVI.

           PERFORM 1150-TESTAR-FS-SAIUNIFI.

           PERFORM 1160-TESTAR-FS-SAICONSL.

           PERFORM 1170-TESTAR-FS-SAICONTR.

           PERFORM 1180-TESTAR-FS-ARQOCORR.

           PERFORM 1190-TESTAR-FS-SAIAGRUP.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO ENTCOBRA.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ENTCOBRA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTCOBRA         NOT EQUAL '00'
               MOVE 'ENTCOBRA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTCOBRA    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
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
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
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
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO SAINDIVI.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-SAINDIVI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAINDIVI         NOT EQUAL '00'
               MOVE 'SAINDIVI'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SAINDIVI    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO SAIUNIFI.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1150-TESTAR-FS-SAIUNIFI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIUNIFI         NOT EQUAL '00'
               MOVE 'SAIUNIFI'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SAIUNIFI    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO SAICONSL.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1160-TESTAR-FS-SAICONSL         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAICONSL         NOT EQUAL '00'
               MOVE 'SAICONSL'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SAICONSL    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO SAICONTR.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1170-TESTAR-FS-SAICONTR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAICONTR         NOT EQUAL '00'
               MOVE 'SAICONTR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SAICONTR    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1170-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO ARQOCORR.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1180-TESTAR-FS-ARQOCORR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQOCORR         NOT EQUAL '00'
               MOVE 'ARQOCORR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQOCORR    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1180-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TESTE DE FILE-STATUS DO ARQUIVO SAIAGRUP.          *
      ******************************************************************
      *----------------------------------------------------------------*
       1190-TESTAR-FS-SAIAGRUP         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIAGRUP         NOT EQUAL '00'
               MOVE 'SAIAGRUP'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SAIAGRUP    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1190-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR SE ARQUIVOS DE ENTRADA ESTAO VAZIOS.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CTRLDATA.

           IF  WRK-FS-CTRLDATA             EQUAL '10'
               DISPLAY '*************** GFCT1174 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*        ARQUIVO CTRLDATA VAZIO        *'
               DISPLAY '*                                      *'
               DISPLAY '*       PROCESSAMENTO  ENCERRADO       *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT1174 ***************'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2200-LER-ENTCONTR.

           IF  WRK-FS-ENTCONTR             EQUAL '10'
               DISPLAY '*************** GFCT1174 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*        ARQUIVO ENTCONTR VAZIO        *'
               DISPLAY '*                                      *'
               DISPLAY '*       PROCESSAMENTO  ENCERRADO       *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT1174 ***************'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2300-LER-ENTCOBRA.

           IF  WRK-FS-ENTCOBRA             EQUAL '10'
               DISPLAY '*************** GFCT1174 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*        ARQUIVO ENTCOBRA VAZIO        *'
               DISPLAY '*                                      *'
               DISPLAY '*       PROCESSAMENTO  ENCERRADO       *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT1174 ***************'
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
0330***        MOVE  ZEROS             TO CONT-DIGITO      OF ENTCONTR
0330***                                   CONT-CONTRATO    OF ENTCONTR
           END-IF.

3003************ ALIMENTAR IND-1
3003       EVALUATE  CONT-DIGITO OF ENTCONTR
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
3003       END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA LEITURA DO ARQUIVO ENTCOBRA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-ENTCOBRA               SECTION.
      *----------------------------------------------------------------*

           READ ENTCOBRA.

           IF  WRK-FS-ENTCOBRA         EQUAL '10'
               GO TO 2300-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ENTCOBRA.

           ADD 1                       TO ACU-LIDOS-ENTCOBRA.

           MOVE  COBR-AGENCIA-DEB      OF ENTCOBRA
                                       TO WRK-CHV-AGENCIA-ATU.
           MOVE  COBR-CONTA-DEB        OF ENTCOBRA
                                       TO WRK-CHV-CONTA-ATU.
           MOVE  COBR-TARIFA-ORIG      OF ENTCOBRA
                                       TO WRK-CHV-TARIFA-ATU.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SEPARAR REGISTROS COM COBRANCA UNIFICADA.          *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                        TO WRK-PROCESS-OK.

           PERFORM 3100-VERIFICAR-TIPO-ARQUIVO.

           IF  WRK-PROCESS-OK              EQUAL 'S'
               IF  WRK-CHAVE-ATUAL         NOT EQUAL WRK-CHAVE-ANTER
                   PERFORM 3200-VERIFICAR-SEPARACAO
               END-IF
               PERFORM 3300-VERIFICAR-TIPO-COBRANCA
           END-IF.

           PERFORM 2300-LER-ENTCOBRA.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR O TIPO DE ARQUIVOS.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-VERIFICAR-TIPO-ARQUIVO     SECTION.
      *----------------------------------------------------------------*

           IF (COBR-SITUACAO-EVENTO         OF ENTCOBRA EQUAL 02 AND
BI1113         COBR-TIPO-CONTA              OF ENTCOBRA EQUAL 1 OR 3)
               MOVE  1                         TO COBR-TIPO-ARQUIVO
                                                           OF ENTCOBRA
           ELSE
               IF (COBR-SITUACAO-EVENTO     OF ENTCOBRA EQUAL 03 AND
BI1113             COBR-TIPO-CONTA          OF ENTCOBRA EQUAL 1 OR 3)
                   MOVE  2                     TO COBR-TIPO-ARQUIVO
                                                           OF ENTCOBRA
               ELSE
                   IF  COBR-TIPO-CONTA         OF ENTCOBRA EQUAL 2
                       MOVE  3                 TO COBR-TIPO-ARQUIVO
                                                           OF ENTCOBRA
                       MOVE  10                TO COBR-GRUPO-DEB
                                                           OF ENTCOBRA
                       MOVE  51                TO COBR-SUBGRUPO-DEB
                                                           OF ENTCOBRA
                   ELSE
                       MOVE 'GFCT1174'         TO WRK-RECURSO
                       MOVE  ZEROS             TO WRK-RETORNO
                       MOVE  SPACES            TO WRK-DESCRICAO
                       MOVE 'TIPO DE ARQUIVO INVALIDO' TO WRK-DESCRICAO
                       MOVE 'N'                TO WRK-PROCESS-OK
                       PERFORM 3110-GRAVAR-ARQOCORR
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE GRAVACAO DO ARQUIVO ARQOCORR, COM ERROS DO MODULO.   *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-GRAVAR-ARQOCORR            SECTION.
      *----------------------------------------------------------------*

           MOVE  REG-COBRANCA          OF ENTCOBRA
                                       TO REG-COBRANCA     OF ARQOCORR.

           INITIALIZE COBR-RETORNO     OF ARQOCORR.

           MOVE 'GFCT0616'             TO COBR-JOB         OF ARQOCORR.
           MOVE 'GFCT1174'             TO COBR-PROGRAMA    OF ARQOCORR.
           MOVE  WRK-RECURSO           TO COBR-RECURSO     OF ARQOCORR.
           MOVE  WRK-DESCRICAO         TO COBR-DESCRICAO   OF ARQOCORR.
           MOVE  WRK-RETORNO           TO COBR-COD-RETORNO OF ARQOCORR.

           MOVE 14                  TO COBR-SITUACAO-EVENTO OF ARQOCORR.

           WRITE REG-COBRANCA          OF ARQOCORR.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1180-TESTAR-FS-ARQOCORR.

           ADD 1                       TO ACU-GRAVA-ARQOCORR.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR SE A TARIFA DEVE SER CONSOLIDADE OU UNI- *
      *   FICADA.                                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-VERIFICAR-SEPARACAO        SECTION.
      *----------------------------------------------------------------*

           PERFORM 3210-ACESSAR-GFCT5530.

           IF  WRK-PROCESS-OK          EQUAL 'S'
               PERFORM 3220-ACESSAR-GFCT5538
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSAR MODULO GFCT5530.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-ACESSAR-GFCT5530           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5530-AREA-ENTRADA
                      WRK-5530-AREA-SAIDA
                      WRK-IO-PCB
                      WRK-ALT-PCB
                      GFCT0M-AREA-ERROS.

           MOVE 'B'                        TO WRK-5530-AMBIENTE.
           MOVE 'GFCT1174'                 TO WRK-5530-CHAMADOR.
           MOVE  COBR-AGENCIA-DEB          OF ENTCOBRA
                                           TO WRK-5530-AGENCIA.
           MOVE  COBR-CONTA-DEB            OF ENTCOBRA
                                           TO WRK-5530-CONTA.
           MOVE  COBR-TARIFA-ORIG          OF ENTCOBRA
                                           TO WRK-5530-TARIFA.
           MOVE  COBR-DATA-ENVIO           OF ENTCOBRA(7:2)
                                           TO WRK-5530-DATA(1:2).
           MOVE  COBR-DATA-ENVIO           OF ENTCOBRA(5:2)
                                           TO WRK-5530-DATA(4:2).
           MOVE  COBR-DATA-ENVIO           OF ENTCOBRA(1:4)
                                           TO WRK-5530-DATA(7:4).
           MOVE 'GFCT5530'                 TO WRK-MODULO.

           INSPECT WRK-5530-DATA REPLACING ALL ' ' BY '.'.

           CALL 'POOL0080'                 USING WRK-MODULO
                                                 WRK-5530-AREA-ENTRADA
                                                 WRK-5530-AREA-SAIDA
                                                 WRK-IO-PCB
                                                 WRK-ALT-PCB
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 'GFCT5530'             TO WRK-RECURSO
               MOVE  RETURN-CODE           TO WRK-RETORNO
               MOVE  SPACES                TO WRK-DESCRICAO
               MOVE 'ERRO NO ACESSO AO MODULO'
                                           TO WRK-DESCRICAO
               MOVE 'N'                    TO WRK-PROCESS-OK
               MOVE ZEROS                  TO WRK-TIPO-COBRANCA
               GO TO 3210-99-FIM
           END-IF.

           IF  WRK-5530-COD-RETORNO        NOT EQUAL ZEROS AND 02
               MOVE 'GFCT5530'             TO WRK-RECURSO
               MOVE WRK-5530-COD-RETORNO   TO WRK-RETORNO
               MOVE  SPACES                TO WRK-DESCRICAO
               EVALUATE WRK-5530-COD-RETORNO
                   WHEN 01
                       MOVE  WRK-MSG01     TO WRK-DESCRICAO
                   WHEN 03
                       MOVE  WRK-MSG03     TO WRK-DESCRICAO
                   WHEN 04
                       MOVE  WRK-MSG04     TO WRK-DESCRICAO
                   WHEN 99
                       MOVE  WRK-MSG99     TO WRK-DESCRICAO
               END-EVALUATE
               MOVE 'N'                    TO WRK-PROCESS-OK
               MOVE ZEROS                  TO WRK-TIPO-COBRANCA
               GO TO 3210-99-FIM
           END-IF.

           IF  WRK-5530-COBR-UNIC          EQUAL 'S'
               MOVE  WRK-CHAVE-ATUAL       TO WRK-CHAVE-ANTER
               MOVE 04                     TO WRK-TIPO-COBRANCA
               MOVE 'N'                    TO WRK-PROCESS-OK
               GO TO 3210-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSAR MODULO GFCT5538.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3220-ACESSAR-GFCT5538           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5538-AREA-ENTRADA
                      WRK-5538-AREA-SAIDA
                      WRK-IO-PCB
                      WRK-ALT-PCB
                      GFCT0M-AREA-ERROS.

           MOVE 'B'                        TO WRK-5538-AMBIENTE.
           MOVE 'GFCT1174'                 TO WRK-5538-CHAMADOR.
           MOVE  COBR-CGC-CPF-MUNERO       OF ENTCOBRA
                                           TO WRK-5538-CCGC-CPF.
           MOVE  COBR-CGC-CPF-FILIAL       OF ENTCOBRA
                                           TO WRK-5538-CFLIAL-CGC
           MOVE  COBR-CGC-CPF-CONTROLE     OF ENTCOBRA
                                           TO WRK-5538-CCTRL-CPF-CGC.
           MOVE  COBR-TARIFA-ORIG          OF ENTCOBRA
                                           TO WRK-5538-CSERVC-TARIF.
           MOVE 'GFCT5538'                 TO WRK-MODULO.

           CALL 'POOL0080'                 USING WRK-MODULO
                                                 WRK-5538-AREA-ENTRADA
                                                 WRK-5538-AREA-SAIDA
                                                 WRK-IO-PCB
                                                 WRK-ALT-PCB
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 'GFCT5538'             TO WRK-RECURSO
               MOVE RETURN-CODE            TO WRK-RETORNO
               MOVE  SPACES                TO WRK-DESCRICAO
               MOVE 'ERRO NO ACESSO AO MODULO'
                                           TO WRK-DESCRICAO
               MOVE ZEROS                  TO WRK-TIPO-COBRANCA
               GO TO 3220-99-FIM
           END-IF.

           IF  WRK-5538-COD-RETORNO        NOT EQUAL ZEROS AND 02
               MOVE 'GFCT5538'             TO WRK-RECURSO
               MOVE WRK-5538-COD-RETORNO   TO WRK-RETORNO
               MOVE  SPACES                TO WRK-DESCRICAO
               EVALUATE WRK-5538-COD-RETORNO
                   WHEN 01
                       MOVE  WRK-MSG01     TO WRK-DESCRICAO
                   WHEN 03
                       MOVE  WRK-MSG03     TO WRK-DESCRICAO
                   WHEN 04
                       MOVE  WRK-MSG04     TO WRK-DESCRICAO
                   WHEN 99
                       MOVE  WRK-MSG99     TO WRK-DESCRICAO
               END-EVALUATE
               MOVE ZEROS                  TO WRK-TIPO-COBRANCA
               GO TO 3220-99-FIM
           END-IF.

           IF  WRK-5538-UNIFICADO          EQUAL 'S'
               MOVE  WRK-CHAVE-ATUAL       TO WRK-CHAVE-ANTER
               MOVE  04                    TO WRK-TIPO-COBRANCA
               GO TO 3220-99-FIM
           END-IF.

           IF  COBR-TIPO-COBRANCA          OF ENTCOBRA EQUAL 5
               MOVE 05                     TO WRK-TIPO-COBRANCA
           ELSE
               IF  COBR-TIPO-COBRANCA      OF ENTCOBRA EQUAL 8
                   MOVE 08                 TO WRK-TIPO-COBRANCA
               ELSE
                   MOVE 01                 TO WRK-TIPO-COBRANCA
               END-IF
           END-IF.

           MOVE  WRK-CHAVE-ATUAL           TO WRK-CHAVE-ANTER.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR O TIPO DE COBRANCA.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-VERIFICAR-TIPO-COBRANCA    SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-TIPO-COBRANCA
               WHEN 1
                   PERFORM 3310-GRAVAR-SAINDIVI

               WHEN 4
                   PERFORM 3320-GRAVAR-SAIUNIFI

               WHEN 5
                   PERFORM 3330-GRAVAR-SAICONSL

               WHEN 8
                   PERFORM 3340-GRAVAR-SAIAGRUP

               WHEN OTHER
                   PERFORM 3110-GRAVAR-ARQOCORR

           END-EVALUATE.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE GRAVACAO DO ARQUIVO DE SAIDA COM REGISTROS SEM UNIFI-*
      *   CAO.                                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-GRAVAR-SAINDIVI            SECTION.
      *----------------------------------------------------------------*

           MOVE  REG-COBRANCA          OF ENTCOBRA
                                       TO REG-COBRANCA     OF SAINDIVI.
           MOVE  WRK-TIPO-COBRANCA     TO COBR-TIPO-COBRANCA
                                                           OF SAINDIVI.

BI0813*----------> COBRANCA PARCIAL NO ULTIMO REPIQUE
BI0813     IF    COBR-SITUACAO-EVENTO   OF SAINDIVI = 3   AND
BI0813           COBR-DEB-PARCIAL       OF SAINDIVI = 2   AND
BI0813           COBR-PARCIAL-ULT-REPIQ OF SAINDIVI = 'S' AND
BI0813           COBR-ULTIMO-REPIQUE    OF SAINDIVI = 'S'
BI0813           MOVE   1    TO   COBR-DEB-PARCIAL OF SAINDIVI
BI0813     END-IF.

           PERFORM 3311-NUMERA-REGISTRO.

           WRITE REG-COBRANCA          OF SAINDIVI.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1140-TESTAR-FS-SAINDIVI.

           ADD 1                       TO ACU-GRAVA-SAINDIVI.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA APURAR O PROXIMO NUMERO DE CONTRATO E MOVER PARA   *
      *   REGISTRO.                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3311-NUMERA-REGISTRO            SECTION.
      *----------------------------------------------------------------*

           IF  CONT-DIGITO   OF ENTCONTR   EQUAL 'Z' AND
               CONT-CONTRATO OF ENTCONTR   EQUAL 9999999
0330           MOVE '0'                    TO CONT-DIGITO   OF ENTCONTR
0330           MOVE  0                     TO CONT-CONTRATO OF ENTCONTR
               MOVE  1                     TO IND-1
0330           DISPLAY '****************** CONTRATO REINICIALIZADO'
0330***        MOVE  WRK-MSG-EXTOURO       TO ERR-TEXTO
0330***        PERFORM 3312-DISPLAY-EXTOURO
           END-IF.

           IF  CONT-CONTRATO OF ENTCONTR   EQUAL 9999999
               ADD 1                       TO IND-1
070615         IF    WRK-DIGITO(IND-1:1)    = 'N'
070515               ADD 1                 TO IND-1
070615         END-IF
               MOVE  WRK-DIGITO(IND-1:1)   TO CONT-DIGITO   OF ENTCONTR
               MOVE  1                     TO CONT-CONTRATO OF ENTCONTR
           ELSE
               ADD 1                       TO CONT-CONTRATO OF ENTCONTR
           END-IF.

           MOVE  CONT-DIGITO   OF ENTCONTR TO COBR-DIG-CONTRATO
                                                            OF SAINDIVI.
           MOVE  CONT-CONTRATO OF ENTCONTR TO COBR-NUM-CONTRATO
                                                            OF SAINDIVI.

      *----------------------------------------------------------------*
       3311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA EMITIR DISPLAY QUANDO QUANTIDADE DE CONTRATO DO DIA*
      *    EXCEDER AO MAXIMO POSSIVEL.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3312-DISPLAY-EXTOURO            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************************** GFCT1174 ****************
      -    '*********'.
           DISPLAY '*
      -            '        *'.
           DISPLAY '* QUANTIDADE DE CONTRATOS DO DIA EXCEDEU AO MAXIMO P
      -            'OSSIVEL *'.
           DISPLAY '*
      -            '        *'.
           DISPLAY '* CONT-DATA........:       '
            CONT-DATA OF ENTCONTR '                         *'.
           DISPLAY '* CONT-DIGITO......:       '
            CONT-DIGITO OF ENTCONTR '                                *'.
           DISPLAY '* CONT-CONTRATO....:       '
            CONT-CONTRATO OF ENTCONTR '                          *'.
           DISPLAY '* COBR-CHAVE-ORIGEM:       '
            COBR-CHAVE-ORIGEM OF SAINDIVI ' *'.

           MOVE  ACU-GRAVA-SAINDIVI    TO WRK-MASCARA.

           DISPLAY '* GRAVADOS-SAINDIVI:                            '
                    WRK-MASCARA ' *'.
           DISPLAY '*
      -            '        *'.
           DISPLAY '************************** GFCT1174 ****************
      -            '*********'.

           PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       3312-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE GRAVACAO DO ARQUIVO DE SAIDA COM REGISTROS A UNIFICAR*
      ******************************************************************
      *----------------------------------------------------------------*
       3320-GRAVAR-SAIUNIFI            SECTION.
      *----------------------------------------------------------------*

           MOVE  REG-COBRANCA          OF ENTCOBRA
                                       TO REG-COBRANCA     OF SAIUNIFI.
           MOVE  WRK-TIPO-COBRANCA     TO COBR-TIPO-COBRANCA
                                                           OF SAIUNIFI.

           WRITE REG-COBRANCA          OF SAIUNIFI.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1150-TESTAR-FS-SAIUNIFI.

           ADD 1                       TO ACU-GRAVA-SAIUNIFI.

      *----------------------------------------------------------------*
       3320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE GRAVACAO DO ARQUIVO DE SAIDA COM REGISTROS A UNIFICAR*
      ******************************************************************
      *----------------------------------------------------------------*
       3330-GRAVAR-SAICONSL            SECTION.
      *----------------------------------------------------------------*

           MOVE  REG-COBRANCA          OF ENTCOBRA
                                       TO REG-COBRANCA     OF SAICONSL.
           MOVE  WRK-TIPO-COBRANCA     TO COBR-TIPO-COBRANCA
                                                           OF SAICONSL.

           WRITE REG-COBRANCA          OF SAICONSL.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1160-TESTAR-FS-SAICONSL.

           ADD 1                       TO ACU-GRAVA-SAICONSL.

      *----------------------------------------------------------------*
       3330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA DE GRAVACAO DO ARQUIVO DE SAIDA COM REGISTROS A AGRUPAR *
      ******************************************************************
      *----------------------------------------------------------------*
       3340-GRAVAR-SAIAGRUP            SECTION.
      *----------------------------------------------------------------*

           MOVE  REG-COBRANCA          OF ENTCOBRA
                                       TO REG-COBRANCA     OF SAIAGRUP.
           MOVE  WRK-TIPO-COBRANCA     TO COBR-TIPO-COBRANCA
                                                           OF SAIAGRUP.

           WRITE REG-COBRANCA          OF SAIAGRUP.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1190-TESTAR-FS-SAIAGRUP.

           ADD 1                       TO ACU-GRAVA-SAIAGRUP.

      *----------------------------------------------------------------*
       3340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO NORMAL DO PROGRAMA.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

      *    IF  ACU-LIDOS-ENTCOBRA      GREATER ZEROS
               PERFORM 4100-GRAVAR-SAICONTR
      *    END-IF.

           PERFORM 4200-EMITIR-ESTATISTICAS.

           CLOSE ENTCOBRA
                 CTRLDATA
                 ENTCONTR
                 SAINDIVI
                 SAIUNIFI
                 SAICONSL
                 SAICONTR
                 ARQOCORR
                 SAIAGRUP.

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

           MOVE  REG-CONTRATO   OF ENTCONTR TO REG-CONTRATO OF SAICONTR.
           MOVE  CONT-DATA      OF SAICONTR TO WRK-DATA-DISPL.
           MOVE  CONT-DIGITO    OF SAICONTR TO WRK-DIGITO-DISPL.
           MOVE  CONT-CONTRATO  OF SAICONTR TO WRK-CONTRATO-DISPL.

           WRITE REG-CONTRATO               OF SAICONTR.

           MOVE WRK-GRAVACAO                TO WRK-OPERACAO.

           PERFORM 1170-TESTAR-FS-SAICONTR.

           ADD 1                            TO ACU-GRAVA-SAICONTR.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA EMISSAO DE DISPLAY                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-ESTATISTICAS        SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-ENTCOBRA     TO WRK-MASCARA.

           DISPLAY '****************** GFCT1174 ******************'.
           DISPLAY '*                                            *'.
           DISPLAY '* TOTAL DE REGS. LIDOS ENTCOBRA: ' WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-CTRLDATA     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. LIDOS CTRLDATA: ' WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-ENTCONTR     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. LIDOS ENTCONTR: ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-SAINDIVI     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. GRAVA SAINDIVI: ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-SAIUNIFI     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. GRAVA SAIUNIFI: ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-SAICONSL     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. GRAVA SAICONSL: ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-SAICONTR     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. GRAVA SAICONTR: ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-ARQOCORR     TO WRK-MASCARA.

           MOVE ACU-GRAVA-SAIAGRUP     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. GRAVA SAIAGRUP: ' WRK-MASCARA ' *'.

           MOVE ACU-GRAVA-ARQOCORR     TO WRK-MASCARA.

           DISPLAY '* TOTAL DE REGS. GRAVA ARQOCORR: ' WRK-MASCARA ' *'.
           DISPLAY '*                                            *'.

           DISPLAY '* CONT-DATA........:       '
            WRK-DATA-DISPL '          *'.
           DISPLAY '* CONT-DIGITO......:       '
            WRK-DIGITO-DISPL '                 *'.
           DISPLAY '* CONT-CONTRATO....:       '
            WRK-CONTRATO-DISPL '           *'.

           DISPLAY '*                                            *'.
           DISPLAY '****************** GFCT1174 ******************'.

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

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'GFCT1174'             TO ERR-PGM.
           MOVE  04                    TO RETURN-CODE.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
