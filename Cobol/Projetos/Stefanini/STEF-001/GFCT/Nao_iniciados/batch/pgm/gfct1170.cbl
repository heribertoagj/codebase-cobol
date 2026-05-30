*================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1170.

       AUTHOR.     JEFERSON PAULO DALPONTE.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1170                                    *
      *    PROGRAMADOR.:   JEFERSON PAULO DALPONTE - CPM PATO BRANCO   *
      *    ANALISTA CPM:   EDSON LUIZ DOS SANTOS   - CPM PATO BRANCO   *
      *    ANALISTA....:   RENATO TAMANAHA         - PWI / GRUPO 50    *
      *    DATA........:   06/01/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FORMATAR REGISTROS BAIXADOS DA TABELA       *
      *      GFCTB092 NO ARQUIVO DE COBRANCA.                          *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    ARQB092                   I#GFCTA3          *
      *                    ARQCOBR                   I#GFCTIL          *
      *                    ARQOCORR                  I#GFCTIL          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                TABLE                       INCLUDE/BOOK        *
      *                    DB2PRD.MOVTO_EVNTO_CRRTT  GFCTB092          *
      *                    DB2PRD.V0OBJE_CLIENTES    OBJEV000          *
      *                    DB2PRD.OBJE_CAD_POUPADOR  OBJEB010          *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA DE COMUNICACAO COM A POOL7100               *
      *    I#GFCT0M - AREA DE COMUNICACAO DE ERROS                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - MODULO PARA CONEXAO COM DB2                      *
      *    POOL0431 - MODULO PARA CALCULAR DIGITO DE CONTROLE          *
      *    POOL7100 - MODULO PARA TRATAMENTO DE ERROS                  *
      *    GFCT5525 - MODULO PARA ATUALIZAR SITUACAO EVENTO            *
      *    GFCT5526 - MODULO PARA BUSCAR AGENCIA E CONTA CENTRALIZADORA*
      *    GFCT5527 - MODULO RARA OBTER O CGC E CPF DO CLIENTE         *
      *    GFCT5528 - MODULO RARA OBTER RAZAO DO CLIENTE               *
      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *    ANALISTA CPM:   ILZA - MELHORIA DE PERFORMANCE              *
      *    DATA........:   25/08/2010                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:                                               *
      *      1 - SUBSTITUICAO DO MODULO GFCT5526 POR CURSOR DA TABELA  *
      *         GFCTB031 E INCLUSAO DE BALANCE LINE                   *
      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   JEFERSON PAULO DALPONTE - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE SOMMER           - CPM PATO BRANCO   *
      *    ANALISTA....:   RICARDO PEREIRA         - PROCKWORK GP. 48  *
      *    DATA........:   02/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUIR ARQUIVO ARQOCORR. INCLUIR MODULO    *
      *      GFCT5525 - ATUALIZA SITUACAO EVENTO.                      *
      *================================================================*
      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP.50   *
      *    DATA........:   02/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ACERTO LOGICO                               *
      *================================================================*
      *       M E L H O R I A   D E   P E R F O R M A N C E            *
      *----------------------------------------------------------------*
      *    ANALISTA....:   ZULU - CPM                                  *
      *    DATA........:   19/04/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   SUBSTITUIR O ACESSO AOS MODULOS GFCT5527 E  *
      *                    GFCT5528 POR ACESSO DIRETO A TABELA DE CLI- *
      *                    ENTES DB2PRD.V0OBJE_CLIENTES (OBJEV000).    *
      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP.50   *
      *    DATA........:   28/05/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVOS...:                                               *
      *      1 - ALTERAR SITUACAO DO EVENTO DA 92 NO UPDATE DA AGENCIA *
      *      2 - SO CHAMAR GFCT5525 SE SITUACAO EVENTO DIFERENTE DE 03 *
      *      3 - CHAMAR GFCT5525 COM CORRENTISTA = H (ATLZ. SOMENTE H4)*
      *================================================================*
PERJS *             U L T I M A   A L T E R A C A O                    *
PERJS *================================================================*
PERJS *                                                                *
PERJS *    ANALISTA....:   JOAO SERGIO    -  CPMBRAXIS                 *
PERJS *    DATA........:   SETEMBRO/2009  -  PERFORMANCE CPM           *
PERJS *    OBJETIVO....:   ALTERAR PROGRAMA PARA NO LUGAR DO UPDATE    *
PERJS *                    GRAVAR ARQUIVO SEQUENCIAL COM OS CAMPOS     *
PERJS *                    DO UPDATE ATUAL, SET E SELECT NA SEQUENCIA  *
PERJS *                    DO INDICE PARA UPDATE NO PROXIMO PROGRAMA   *
PERJS *                    DO JOB.                                     *
PERJS *                                                                *
PERJS *================================================================*
122010*             U L T I M A   A L T E R A C A O                    *
122010*================================================================*
122010*                                                                *
122010*    PROGRAMADOR.:   FABRICA          -  SONDA PROCWORK          *
122010*    ANALISTA....:   ANTONIO PAGNOCCA - SONDA PROCWORK           *
122010*    DATA........:   DEZ/2010                                    *
122010*    OBJETIVO....:   NA GRAVACAO DO ARQCOBR, PASSAR A VALORIZAR  *
122010*                    O CAMPO A3-CINDCD-TARIF-PCELD.              *
122010*                                                                *
122010*================================================================*
BI0813*================================================================*
BI0813*             U L T I M A   A L T E R A C A O                    *
BI0813*================================================================*
BI0813*                                                                *
BI0813*    PROGRAMADOR.:   UBIRAJARA        - SONDA PROCWORK           *
BI0813*    ANALISTA....:   UBIRAJARA        - SONDA PROCWORK           *
BI0813*    DATA........:   AGO/2013                                    *
BI0813*    OBJETIVO....:   NA GRAVACAO DO ARQCOBR, VALORIZAR O CAMPO   *
BI0813*                    NOVO  COBR-ULTIMO-REPIQUE COM 'S' OU 'N'.   *
BI0813*                                                                *
BI0813*================================================================*

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

           SELECT ARQB092  ASSIGN      TO UT-S-ARQB092
           FILE STATUS                 IS WRK-FS-ARQB092.

           SELECT ARQCOBR  ASSIGN      TO UT-S-ARQCOBR
           FILE STATUS                 IS WRK-FS-ARQCOBR.

           SELECT ARQOCORR ASSIGN      TO UT-S-ARQOCORR
           FILE STATUS                 IS WRK-FS-ARQOCORR.

PERJS      SELECT SAIB092  ASSIGN      TO UT-S-SAIB092
PERJS      FILE STATUS                 IS WRK-FS-SAIB092.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  BAIXA SEQUENCIAL DA TABELA GFCTB092                 *
      *            ORG. SEQUENCIAL     -   LRECL   =   132             *
      *----------------------------------------------------------------*

       FD  ARQB092
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY I#GFCTA3.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQ DE COBRANCA                                     *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ARQCOBR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY I#GFCTIL.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE ERROS DE PROCESSAMENTO - SAIDA           *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ARQOCORR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY I#GFCTIL.

PERJS *----------------------------------------------------------------*
PERJS *   OUTPUT:  GRAVA SEQUENCIAL DA TABELA GFCTB092                 *
PERJS *            ORG. SEQUENCIAL     -   LRECL   =   034             *
PERJS *----------------------------------------------------------------*

PERJS  FD  SAIB092
PERJS      RECORDING  MODE  F
PERJS      LABEL  RECORD    STANDARD
PERJS      BLOCK  CONTAINS  0  RECORDS.

PERJS  01  FD-GFCTB092                PIC  X(34).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '*** GFCT1170 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS                   PIC  9(011)         VALUE ZEROS.
       77  WRK-ACU-LID-TAB31           PIC  9(011) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS                PIC  9(011)         VALUE ZEROS.
       77  ACU-GRAVADOS-ARQOCORR       PIC  9(011)         VALUE ZEROS.
       77  ACU-COMMIT                  PIC  9(011)         VALUE ZEROS.
070528 77  ACU-ATU-H4                  PIC  9(011)         VALUE ZEROS.
070528 77  ACU-ATU-92          COMP-3  PIC  9(011)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-ARQB092              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQCOBR              PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQOCORR             PIC  X(002)         VALUE SPACES.
PERJS  77  WRK-FS-SAIB092              PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-MASCARA                 PIC  ZZ.ZZZ.ZZZ.ZZ9
PERJS                                       VALUE  ZEROS.
       77  WRK-CAG-DSTNO-MOVTO         PIC  9(005)         VALUE ZEROS.
       77  WRK-CCTA-DSTNO-MOVTO        PIC  9(013)         VALUE ZEROS.

       77  WRK-MSG04                   PIC  X(050)         VALUE
           'AGENCIA E CONTA NAO CADASTRADA'.

       01  WRK-RAZAO                   PIC  9(005)         VALUE ZEROS.
       01  WRK-RAZAO-R REDEFINES WRK-RAZAO.
           03  WRK-GRUPO-RAZAO         PIC  9(002).
           03  WRK-SUBGRUPO-RAZAO      PIC  9(002).
           03  FILLER                  PIC  X(001).

       01  WRK-MSG07.
           03  WRK-MSG07-DESC          PIC  X(033)         VALUE
               'ERRO NO ACESSO AO MODULO GFCT5527'.
           03  FILLER                  PIC  X(011)         VALUE
               ' - COD RET='.
           03  WRK-MSG07-RETORNO       PIC  9(003)         VALUE ZEROS.
           03  FILLER                  PIC  X(007)         VALUE
               ' - SQL='.
           03  WRK-MSG07-SQL           PIC  9(003)         VALUE ZEROS.

       01  WRK-DATA-INV.
           03  WRK-ANO-INV             PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-INV             PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-INV             PIC  9(002)         VALUE ZEROS.

       01  WRK-CPOS-ERRO.
             10 ERRO-SIM                PIC X(01).
             10 ERRO-COD-RETORNO        PIC X(03).
             10 ERRO-DESCRICAO          PIC X(50).

      *-->>  AREA DO REGISTRO GRAVADO  <-------------------------------*

PERJS  01  WRK-GFCTB092.
PERJS      05  WRK-CROTNA-ORIGE-MOVTO         PIC  X(04)  VALUE  SPACES.
PERJS      05  WRK-DENVIO-MOVTO-TARIF         PIC  X(10)  VALUE  SPACES.
PERJS      05  WRK-CNRO-ARQ-MOVTO     COMP-3  PIC S9(02)  VALUE   ZEROS.
PERJS      05  WRK-CSEQ-MOVTO         COMP-3  PIC S9(11)  VALUE   ZEROS.
PERJS      05  WRK-CAG-CTLZA-DEB      COMP-3  PIC S9(05)  VALUE   ZEROS.
PERJS      05  WRK-CCTA-CTLZA-DEB     COMP-3  PIC S9(13)  VALUE   ZEROS.
PERJS      05  WRK-CSIT-EVNTO-RECBD   COMP-3  PIC S9(02)  VALUE   ZEROS.

      *-->>  AREA DO REGISTRO GRAVADO  <-------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGEM DE ERRO ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-ARQUIVO             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL0431 ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-POOL0431.
           05  WRK-AGENCIA-0431        PIC  9(007)         VALUE ZEROS.
       01  WRK-TAMANHO-0431            PIC  9(002)         VALUE ZEROS.
       01  WRK-DIGITO-0431             PIC  X.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CHAVES ***'.
      *----------------------------------------------------------------*

ILZA ********* ALTEREI CHAVES PARA COMP-3 *************
       01  WRK-CHAVE-ATU.
           03  WRK-AGENCIA-ATU         PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-CONTA-ATU           PIC  9(013) COMP-3  VALUE ZEROS.

       01  WRK-CHAVE-ANT.
           03  WRK-AGENCIA-ANT         PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-CONTA-ANT           PIC  9(013) COMP-3  VALUE ZEROS.

ILZA   01  WRK-CHAVE-TAB31.
           03  WRK-TAB31-CAG           PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-TAB31-CTA           PIC  9(013) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA COMUNICACAO COM MODULOS ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-ENTRADA.
           03  WRK-AMBIENTE                PIC  X(001)     VALUE SPACES.
           03  WRK-CHAMADOR                PIC  X(008)     VALUE SPACES.
           03  WRK-AGENCIA-CLIENTE         PIC  9(005)     VALUE ZEROS.
           03  WRK-CONTA-CLIENTE           PIC  9(013)     VALUE ZEROS.

       01  WRK-5525-AREA-ENTRADA.
           03  WRK-5525-AMBIENTE       PIC  X(001)         VALUE SPACES.
           03  WRK-5525-CHAMADOR       PIC  X(008)         VALUE SPACES.
           03  WRK-5525-DENVIO-MOVTO   PIC  X(010)         VALUE SPACES.
           03  WRK-5525-CROTNA-ORIGE   PIC  X(004)         VALUE SPACES.
           03  WRK-5525-CNRO-ARQ       PIC  9(002)         VALUE ZEROS.
           03  WRK-5525-CSEQ-MOVTO     PIC  9(011)         VALUE ZEROS.
           03  WRK-5525-CSIT-EVNTO     PIC  9(002)         VALUE ZEROS.
           03  WRK-5525-CORRENTISTA    PIC  X(001)         VALUE SPACES.
      *        S - SIM
      *        N - NAO
070528*        H - GRAVA SOMENTE HISTORICO

       01  WRK-5525-AREA-SAIDA.
           03  WRK-5525-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
      *        00 - PROCESSAMENTO OK
      *        01 - AMBIENTE DIFERENTE DE 'B' E 'O'
      *        02 - REGISTRO NAO ENCONTRADO
      *        03 - PARAMETROS DE PESQUISA INVALIDOS
      *        04 - PROGRAMA CHAMADOR NAO INFORMADO
      *        99 - ERRO DB2
           03  WRK-5525-COD-SQL-ERRO   PIC S9(003)         VALUE ZEROS.
           03  WRK-5525-DESC-MSG       PIC  X(070)         VALUE SPACES.

       01  WRK-5526-AREA-SAIDA.
           03  WRK-5526-COD-RETORNO        PIC  9(002)     VALUE ZEROS.
      *        00 - PROCESSAMENTO OK
      *        01 - AMBIENTE DIFERENTE DE 'B' E 'O'
      *        02 - CLIENTE NAO POSSUI CONTA CENTRALIZADORA
      *        03 - PARAMETROS DE PESQUISA INVALIDOS
      *        04 - PROGRAMA CHAMADOR NAO INFORMADO
      *        99 - ERRO DB2
           03  WRK-5526-COD-SQL-ERRO       PIC S9(003)     VALUE ZEROS.
           03  WRK-5526-COD-MSG-GFCT       PIC  9(004)     VALUE ZEROS.
           03  WRK-5526-DESC-MSG           PIC  X(070)     VALUE SPACES.
           03  WRK-5526-QTD-OCORR          PIC  9(005)     VALUE ZEROS.
           03  WRK-5526-DADOS-RETORNO.
               05  WRK-5526-AGE-CENTRALIZADORA
                                           PIC  9(005)     VALUE ZEROS.
               05  WRK-5526-CONTA-CENTRALIZADORA
                                           PIC  9(013)     VALUE ZEROS.

       01  WRK-5527-AREA-SAIDA.
           03  WRK-5527-COD-RETORNO        PIC  9(002)     VALUE ZEROS.
      *        00 - PROCESSAMENTO OK
      *        01 - AMBIENTE DIFERENTE DE 'B' E 'O'
      *        02 - AGENCIA E CONTA NAO CADASTRADA
      *        03 - PARAMETROS DE PESQUISA INVALIDOS
      *        04 - PROGRAMA CHAMADOR NAO INFORMADO
      *        05 - CODIGO DE CLIENTE NAO CADASTRADO
      *        99 - ERRO DB2
           03  WRK-5527-COD-SQL-ERRO       PIC S9(003)     VALUE ZEROS.
           03  WRK-5527-COD-MSG-GFCT       PIC  9(004)     VALUE ZEROS.
           03  WRK-5527-DESC-MSG           PIC  X(070)     VALUE SPACES.
           03  WRK-5527-QTD-OCORR          PIC  9(005)     VALUE ZEROS.
           03  WRK-5527-DADOS-RETORNO.
ST25X6*        05  WRK-5527-CCGC-CPF       PIC  9(009)     VALUE ZEROS.
ST25X6         05  WRK-5527-CCGC-CPF       PIC  X(009)     VALUE SPACES.
ST25X6*        05  WRK-5527-CFLIAL-CGC     PIC  9(004)     VALUE ZEROS.
ST25X6         05  WRK-5527-CFLIAL-CGC     PIC  X(004)     VALUE SPACES.
               05  WRK-5527-CCTRL-CPF-CGC  PIC  9(002)     VALUE ZEROS.

       01  WRK-5528-AREA-SAIDA.
           03  WRK-5528-COD-RETORNO        PIC  9(002)     VALUE ZEROS.
      *        00 - PROCESSAMENTO OK
      *        01 - AMBIENTE DIFERENTE DE 'B' E 'O'
      *        02 - AGENCIA E CONTA NAO CADASTRADA
      *        03 - PARAMETROS DE PESQUISA INVALIDOS
      *        04 - PROGRAMA CHAMADOR NAO INFORMADO
      *        99 - ERRO DB2
           03  WRK-5528-COD-SQL-ERRO       PIC S9(003)     VALUE ZEROS.
           03  WRK-5528-COD-MSG-GFCT       PIC  9(004)     VALUE ZEROS.
           03  WRK-5528-DESC-MSG           PIC  X(070)     VALUE SPACES.
           03  WRK-5528-QTD-OCORR          PIC  9(005)     VALUE ZEROS.
           03  WRK-5528-DADOS-RETORNO.
               05  WRK-5528-CGRP-CTBIL     PIC  9(003)     VALUE ZEROS.
               05  WRK-5528-CSGRP-CTBIL    PIC  9(003)     VALUE ZEROS.

       COPY I#GFCT0M.

       01  WRK-IO-PCB.
           03  WRK-IO-TERM                 PIC  X(008)     VALUE SPACES.
           03  FILLER                      PIC  X(002)     VALUE SPACES.
           03  WRK-IO-STA                  PIC  X(002)     VALUE SPACES.
           03  FILLER                      PIC  X(012)     VALUE SPACES.
           03  WRK-IO-MODNAME              PIC  X(008)     VALUE SPACES.

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME             PIC  X(008)     VALUE SPACES.
           03  FILLER                      PIC  X(002)     VALUE SPACES.
           03  WRK-ALT-STATUS              PIC  X(002)     VALUE SPACES.
           03  FILLER                      PIC  X(012)     VALUE SPACES.
           03  WRK-ALT-MODNAME             PIC  X(008)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
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
               INCLUDE OBJEV000
           END-EXEC.

           EXEC SQL
               INCLUDE OBJEB010
           END-EXEC.

ILZA       EXEC SQL
               INCLUDE GFCTB031
           END-EXEC.

           EXEC SQL
ILZA        DECLARE CSR01-GFCTB031 CURSOR WITH HOLD FOR
            SELECT  CAG_EVNTO,
                    CCTA_EVNTO,
                    CAG_CTLZA_DEB,
                    CCTA_CTLZA_DEB
            FROM    DB2PRD.CTA_CTLZA_DEB
            WHERE
                  HEXCL_CTLZA        = :GFCTB031.HEXCL-CTLZA
            ORDER BY CAG_EVNTO, CCTA_EVNTO
ILZA       END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT1170 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

ILZA       MOVE '0001-01-01-01.01.01.000001'
                                       TO HEXCL-CTLZA OF GFCTB031.
           EXEC SQL
               OPEN CSR01-GFCTB031
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'CURSOR31'         TO WRK-ARQUIVO
               MOVE 'OPEN'             TO ERR-TEXTO
               MOVE 'CSR'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FS-ARQB092      EQUAL '10'.

           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  ARQB092
                OUTPUT ARQCOBR
                       ARQOCORR.

PERJS      OPEN  OUTPUT  SAIB092

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-ARQB092.

           PERFORM 112000-TESTAR-FS-ARQCOBR.

           PERFORM 113000-TESTAR-FS-ARQOCORR.

PERJS      PERFORM  1140-00-TESTAR-FS-SAIB092.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-TESTAR-FS-ARQB092        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQB092          NOT EQUAL '00'
               MOVE WRK-FS-ARQB092     TO WRK-FILE-STATUS
               MOVE 'ARQB092 '         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-ARQCOBR        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQCOBR          NOT EQUAL '00'
               MOVE WRK-FS-ARQCOBR     TO WRK-FILE-STATUS
               MOVE 'ARQCOBR '         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       113000-TESTAR-FS-ARQOCORR       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQOCORR         NOT EQUAL '00'
               MOVE WRK-FS-ARQOCORR    TO WRK-FILE-STATUS
               MOVE 'ARQOCORR'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

PERJS *----------------------------------------------------------------*
PERJS  1140-00-TESTAR-FS-SAIB092        SECTION.
PERJS *----------------------------------------------------------------*

PERJS      IF  WRK-FS-SAIB092  NOT  EQUAL  '00'
PERJS          MOVE  WRK-FS-SAIB092  TO  WRK-FILE-STATUS
PERJS          MOVE  'SAIB092 '      TO  WRK-ARQUIVO
PERJS          MOVE  WRK-MSG-FS      TO  ERR-TEXTO
PERJS          MOVE  'APL'           TO  ERR-TIPO-ACESSO
PERJS          PERFORM  999999-PROCESSAR-ROTINA-ERRO
PERJS      END-IF.

PERJS *----------------------------------------------------------------*
PERJS  1140-99-FIM.                  EXIT.
PERJS *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-ARQB092.

           IF  WRK-FS-ARQB092          EQUAL '10'
               DISPLAY '***************** GFCT1170 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO ARQB092 VAZIO           *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO ENCERRADO          *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1170 *****************'
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-LER-ARQB092              SECTION.
      *----------------------------------------------------------------*

           READ ARQB092.

           IF  WRK-FS-ARQB092          EQUAL '10'
               GO TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-ARQB092.

           MOVE A3-CAG-DSTNO-MOVTO     TO WRK-AGENCIA-ATU.
           MOVE A3-CCTA-DSTNO-MOVTO    TO WRK-CONTA-ATU.

           ADD 1                       TO ACU-LIDOS.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
ILZA   2400-LER-CSR31                  SECTION.
      *----------------------------------------------------------------*

ILZA       EXEC SQL
ILZA           FETCH CSR01-GFCTB031
               INTO
                      :GFCTB031.CAG-EVNTO,
                      :GFCTB031.CCTA-EVNTO,
                      :GFCTB031.CAG-CTLZA-DEB,
                      :GFCTB031.CCTA-CTLZA-DEB
ILZA       END-EXEC.

           IF  SQLCODE                 EQUAL +100
               MOVE 99999              TO WRK-TAB31-CAG
               MOVE 9999999999999      TO WRK-TAB31-CTA
           ELSE
            IF SQLCODE                 EQUAL 0
ILZA           ADD  1                  TO WRK-ACU-LID-TAB31
               MOVE CAG-EVNTO          TO WRK-TAB31-CAG
               MOVE CCTA-EVNTO         TO WRK-TAB31-CTA
            ELSE
             IF (SQLCODE                NOT EQUAL ZEROS) OR
                (SQLWARN0               EQUAL 'W')
                MOVE 'CURSOR31'         TO WRK-ARQUIVO
                MOVE 'FETCH'            TO ERR-TEXTO
                MOVE 'CSR'              TO ERR-TIPO-ACESSO
                PERFORM 999999-PROCESSAR-ROTINA-ERRO
             END-IF
            END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE REG-COBRANCA OF ARQCOBR.

PERJS      INITIALIZE  WRK-GFCTB092

      *------->> CAMPOS FIXOS
           MOVE WRK-GRAVACAO               TO WRK-OPERACAO.
           MOVE A3-DAGNDA-DEB-MOVTO(1:2)   TO WRK-DIA-INV.
           MOVE A3-DAGNDA-DEB-MOVTO(4:2)   TO WRK-MES-INV.
           MOVE A3-DAGNDA-DEB-MOVTO(7:4)   TO WRK-ANO-INV.
           MOVE WRK-DATA-INV               TO COBR-DATA-ENVIO
                                                             OF ARQCOBR.
           MOVE ZEROS                      TO COBR-DIG-CONTRATO
                                                             OF ARQCOBR.
           MOVE A3-DENVIO-MOVTO-TARIF      TO COBR-DATA-ORIG OF ARQCOBR.
           MOVE A3-CROTNA-ORIGE-MOVTO      TO COBR-ROTINA-ORIG
                                                             OF ARQCOBR.
           MOVE A3-CNRO-ARQ-MOVTO          TO COBR-NUM-ARQ-ORIG
                                                             OF ARQCOBR.
           MOVE A3-CSEQ-MOVTO              TO COBR-SEQ-ARQ-ORIG
                                                             OF ARQCOBR.
           MOVE A3-CSERVC-TARIF            TO COBR-TARIFA-ORIG
                                                             OF ARQCOBR.

           COMPUTE COBR-VALOR-DEBITAR OF ARQCOBR = A3-VTARIF-LIQ-MOVTO -
                                             A3-VTARIF-DEB-MOVTO.
           MOVE A3-CSIT-EVNTO-RECBD        TO COBR-SITUACAO-EVENTO
                                                             OF ARQCOBR.
           MOVE 2                          TO COBR-TIPO-SALDO
                                                             OF ARQCOBR.
           MOVE A3-CTPO-CTA-MOVTO          TO COBR-TIPO-CONTA
                                                             OF ARQCOBR.
           MOVE A3-CBCO-DSTNO-MOVTO        TO COBR-BANCO-ORIG
                                                             OF ARQCOBR.
           MOVE A3-CAG-DSTNO-MOVTO         TO COBR-AGENCIA-ORIG
                                                             OF ARQCOBR.
           MOVE A3-DOCOR-EVNTO(1:2)        TO WRK-DIA-INV.
           MOVE A3-DOCOR-EVNTO(4:2)        TO WRK-MES-INV.
           MOVE A3-DOCOR-EVNTO(7:4)        TO WRK-ANO-INV.
           MOVE WRK-DATA-INV               TO COBR-DATA-EVNTO-ORIG
                                                             OF ARQCOBR.

122010     MOVE A3-CINDCD-TARIF-PCELD      TO COBR-ACEITA-PARCELADO
122010                                                       OF ARQCOBR.
122010

BI0813     IF   A3-QTENTV-DEB-MOVTO   EQUAL   A3-QMAX-TENTV-DEB
BI0813          MOVE      'S'         TO      COBR-ULTIMO-REPIQUE
BI0813                                        OF ARQCOBR
BI0813     ELSE
BI0813          MOVE      'N'         TO      COBR-ULTIMO-REPIQUE
BI0813                                        OF ARQCOBR
BI0813     END-IF.

      *------->> ACESSA MODULOS NA QUEBRA
           IF  WRK-CHAVE-ATU               NOT EQUAL WRK-CHAVE-ANT
               PERFORM 310000-ACESSAR-MODULOS
               IF    ERRO-SIM      = 'S'
                     PERFORM 312000-GRAVAR-ARQOCORR
                     GO           TO  300000-LEITURA
               END-IF
           END-IF.

      *------->> MOVIMENTA DADOS DOS MODULOS
               MOVE 237                    TO COBR-BANCO-DEB OF ARQCOBR
               MOVE WRK-5526-AGE-CENTRALIZADORA
                                           TO COBR-AGENCIA-DEB
                                                             OF ARQCOBR
               MOVE WRK-5526-CONTA-CENTRALIZADORA
                                           TO COBR-CONTA-DEB OF ARQCOBR
               MOVE WRK-5527-CCGC-CPF      TO COBR-CGC-CPF-MUNERO
                                                             OF ARQCOBR
               MOVE WRK-5527-CFLIAL-CGC    TO COBR-CGC-CPF-FILIAL
                                                             OF ARQCOBR
               MOVE WRK-5527-CCTRL-CPF-CGC TO COBR-CGC-CPF-CONTROLE
                                                             OF ARQCOBR
               MOVE WRK-5528-CGRP-CTBIL    TO COBR-GRUPO-DEB OF ARQCOBR
               MOVE WRK-5528-CSGRP-CTBIL   TO COBR-SUBGRUPO-DEB
                                                             OF ARQCOBR
               MOVE WRK-DIGITO-0431     TO COBR-DIGITO-DEB  OF ARQCOBR.

0708           IF    ERRO-SIM      = 'S'
                     PERFORM 312000-GRAVAR-ARQOCORR
                     GO           TO  300000-LEITURA
0708           END-IF.

               WRITE REG-COBRANCA OF ARQCOBR
               PERFORM 112000-TESTAR-FS-ARQCOBR
               ADD 1        TO ACU-GRAVADOS

070528*------->> ALTERA SITUACAO DO EVENTO PARA 13 NA 92
070528         PERFORM 311000-ATUALIZAR-GFCTB092
070528         ADD 1        TO ACU-ATU-92

070528*------->> GRAVA HISTORICO  PARA SITUACAO DIFERENTE DE 3
070528     IF  A3-CSIT-EVNTO-RECBD   NOT = 3
070528         PERFORM 330000-ALTERAR-STATUS
070528         ADD 1        TO ACU-ATU-H4
070528     END-IF.

      *------>>  FAZ COMMIT A CADA 500 OCORRENCIAS
           IF    ACU-COMMIT    > 500
                 PERFORM 410000-EFETUAR-COMMIT
                 MOVE 1        TO  ACU-COMMIT
           ELSE
                 ADD  1        TO  ACU-COMMIT
           END-IF.

       300000-LEITURA.
           PERFORM 210000-LER-ARQB092.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-ACESSAR-MODULOS          SECTION.
      *----------------------------------------------------------------*
           INITIALIZE                   WRK-CPOS-ERRO.
           MOVE 'N'                     TO ERRO-SIM.
           MOVE WRK-CHAVE-ATU           TO WRK-CHAVE-ANT.
           MOVE 'GFCT5526'              TO WRK-MODULO.

      *------->> BUSCA CONTA CENTRALIZADORA

ILZA       PERFORM 2400-LER-CSR31 UNTIL
ILZA               (WRK-CHAVE-TAB31  NOT LESS   WRK-CHAVE-ATU) OR
ILZA               (WRK-TAB31-CAG    EQUAL      99999).

***********IF  WRK-5526-COD-RETORNO     EQUAL ZEROS
ILZA       IF  WRK-CHAVE-TAB31          EQUAL WRK-CHAVE-ATU
               MOVE 237                 TO COBR-BANCO-DEB   OF ARQCOBR
ILZA           MOVE CAG-CTLZA-DEB       TO
                                           COBR-AGENCIA-DEB OF ARQCOBR
ILZA                                       WRK-5526-AGE-CENTRALIZADORA
PERJS                                      WRK-CAG-CTLZA-DEB
ILZA           MOVE CCTA-CTLZA-DEB      TO
                                           COBR-CONTA-DEB   OF ARQCOBR
ILZA                                     WRK-5526-CONTA-CENTRALIZADORA
PERJS                                      WRK-CCTA-CTLZA-DEB
               MOVE 'S'                 TO COBR-CENTRALIZ   OF ARQCOBR
070528*        PERFORM 311000-ATUALIZAR-GFCTB092
           ELSE
               MOVE 237                 TO COBR-BANCO-DEB   OF ARQCOBR
               MOVE A3-CAG-DSTNO-MOVTO  TO WRK-5526-AGE-CENTRALIZADORA
                                           COBR-AGENCIA-DEB OF ARQCOBR
               MOVE A3-CCTA-DSTNO-MOVTO TO WRK-5526-CONTA-CENTRALIZADORA
                                           COBR-CONTA-DEB   OF ARQCOBR
               MOVE 'N'                 TO COBR-CENTRALIZ   OF ARQCOBR
PERJS          MOVE  A3-CAG-CTLZA-DEB
PERJS          TO    WRK-CAG-CTLZA-DEB
PERJS          MOVE  A3-CCTA-CTLZA-DEB
PERJS          TO    WRK-CCTA-CTLZA-DEB
           END-IF.

           MOVE A3-CAG-DSTNO-MOVTO      TO AGENCIA        OF  OBJEV000.
           MOVE A3-CCTA-DSTNO-MOVTO     TO CONTA-CORRENTE OF  OBJEV000.

           IF  A3-CCTA-DSTNO-MOVTO  LESS  1000000
               MOVE 'OBJE_CLIENTES'     TO ERR-DBD-TAB
               EXEC SQL
                    SELECT  NUM_CGCCPF,
                            FILIAL_CGCCPF,
                            CONTR_CGCCPF,
                            RAZAO,
ST25X6                      NUM_CGCCPF_ST,
ST25X6                      FILIAL_CGCCPF_ST,
ST25X6                      CONTR_CGCCPF_ST
                    INTO   :NUM-CGCCPF,
                           :FILIAL-CGCCPF,
                           :CONTR-CGCCPF,
                           :RAZAO,
ST25X6                     :OBJEV000.NUM-CGCCPF-ST,
ST25X6                     :OBJEV000.FILIAL-CGCCPF-ST,
ST25X6                     :OBJEV000.CONTR-CGCCPF-ST
                    FROM    DB2PRD.V0OBJE_CLIENTES
                    WHERE   AGENCIA        = :OBJEV000.AGENCIA
                    AND     CONTA_CORRENTE = :OBJEV000.CONTA-CORRENTE
               END-EXEC
           ELSE
               MOVE 'OBJE_CAD_POUPADOR' TO ERR-DBD-TAB
               EXEC SQL
                    SELECT  POUPADOR_CGC_CPF,
                            POUPADOR_FILIAL,
                            POUPADOR_CONTROLE,
                            POUPADOR_RAZAO,
ST25X6                      POUPADOR_CGC_CPF_ST,
ST25X6                      POUPADOR_FILIAL_ST,
ST25X6                      POUPADOR_CONTROLE_ST
                    INTO   :NUM-CGCCPF,
                           :FILIAL-CGCCPF,
                           :CONTR-CGCCPF,
                           :RAZAO,
ST25X6                     :OBJEV000.NUM-CGCCPF-ST,
ST25X6                     :OBJEV000.FILIAL-CGCCPF-ST,
ST25X6                     :OBJEV000.CONTR-CGCCPF-ST
                    FROM    DB2PRD.OBJE_CAD_POUPADOR
                    WHERE   POUPADOR_AGENCIA  = :OBJEV000.AGENCIA
                    AND     POUPADOR_CONTA    = :OBJEV000.CONTA-CORRENTE
               END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0001'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE  EQUAL  ZEROS
               MOVE ZEROS                TO WRK-5527-COD-RETORNO
                                            WRK-5528-COD-RETORNO
ST25X6*        MOVE NUM-CGCCPF           TO WRK-5527-CCGC-CPF
ST25X6*                                         COBR-CGC-CPF-MUNERO
ST25X6*                                      OF ARQCOBR
ST25X6*        MOVE FILIAL-CGCCPF        TO WRK-5527-CFLIAL-CGC
ST25X6*                                         COBR-CGC-CPF-FILIAL
ST25X6*                                      OF ARQCOBR
ST25X6*        MOVE CONTR-CGCCPF         TO WRK-5527-CCTRL-CPF-CGC
ST25X6*                                         COBR-CGC-CPF-CONTROLE
ST25X6*                                      OF ARQCOBR
               MOVE NUM-CGCCPF-ST OF OBJEV000   TO WRK-5527-CCGC-CPF
                                               COBR-CGC-CPF-MUNERO
                                            OF ARQCOBR
               MOVE FILIAL-CGCCPF-ST OF OBJEV000 TO WRK-5527-CFLIAL-CGC
                                               COBR-CGC-CPF-FILIAL
                                            OF ARQCOBR
               MOVE CONTR-CGCCPF-ST OF OBJEV000 
                                            TO WRK-5527-CCTRL-CPF-CGC
                                               COBR-CGC-CPF-CONTROLE
                                            OF ARQCOBR
               MOVE RAZAO OF OBJEV000    TO WRK-RAZAO
               MOVE WRK-GRUPO-RAZAO      TO WRK-5528-CGRP-CTBIL
               MOVE WRK-SUBGRUPO-RAZAO   TO WRK-5528-CSGRP-CTBIL
               MOVE WRK-5528-CGRP-CTBIL  TO COBR-GRUPO-DEB
                                         OF ARQCOBR
               MOVE WRK-5528-CSGRP-CTBIL TO COBR-SUBGRUPO-DEB
                                         OF ARQCOBR
           ELSE
ST25X6*        MOVE ZEROS              TO WRK-5527-CCGC-CPF
ST25X6*                                   WRK-5527-CFLIAL-CGC
ST25X6*                                   WRK-5527-CCTRL-CPF-CGC
ST25X6         MOVE SPACES             TO WRK-5527-CCGC-CPF
ST25X6                                    WRK-5527-CFLIAL-CGC
ST25X6         MOVE ZEROS              TO WRK-5527-CCTRL-CPF-CGC
               MOVE ZEROS              TO WRK-5528-CGRP-CTBIL
                                          WRK-5528-CSGRP-CTBIL
               MOVE 'GFCT5527'           TO COBR-RECURSO OF ARQCOBR
               MOVE 02                   TO WRK-5527-COD-RETORNO
                                            WRK-5528-COD-RETORNO
               MOVE WRK-MSG04            TO WRK-5527-DESC-MSG
               MOVE WRK-5527-COD-RETORNO TO ERRO-COD-RETORNO
               MOVE WRK-5527-DESC-MSG    TO ERRO-DESCRICAO
               MOVE 'S'                  TO ERRO-SIM
               GO                        TO 310000-99-FIM
           END-IF.

           PERFORM 320000-CALCULAR-DIGITO.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       311000-ATUALIZAR-GFCTB092       SECTION.
      *----------------------------------------------------------------*

PERJS      MOVE  A3-CROTNA-ORIGE-MOVTO  TO  WRK-CROTNA-ORIGE-MOVTO
PERJS      MOVE  A3-DENVIO-MOVTO-TARIF  TO  WRK-DENVIO-MOVTO-TARIF
PERJS      MOVE  A3-CNRO-ARQ-MOVTO      TO  WRK-CNRO-ARQ-MOVTO
PERJS      MOVE  A3-CSEQ-MOVTO          TO  WRK-CSEQ-MOVTO
PERJS      MOVE  13                     TO  WRK-CSIT-EVNTO-RECBD
PERJS      MOVE  WRK-GRAVACAO           TO  WRK-OPERACAO

PERJS      WRITE  FD-GFCTB092  FROM  WRK-GFCTB092

PERJS      PERFORM  1140-00-TESTAR-FS-SAIB092.

      *----------------------------------------------------------------*
       311000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       312000-GRAVAR-ARQOCORR          SECTION.
      *----------------------------------------------------------------*
           MOVE REG-COBRANCA         OF ARQCOBR
                                     TO REG-COBRANCA     OF ARQOCORR.

           MOVE 'GFCT0612'           TO COBR-JOB        OF ARQOCORR.
           MOVE 'GFCT1170'           TO COBR-PROGRAMA   OF ARQOCORR.
           MOVE WRK-MODULO           TO COBR-RECURSO    OF ARQOCORR.
           MOVE ERRO-COD-RETORNO     TO COBR-COD-RETORNO OF ARQOCORR.
           MOVE ERRO-DESCRICAO       TO COBR-DESCRICAO   OF ARQOCORR.
           MOVE 14                   TO COBR-SITUACAO-EVENTO
                                                         OF ARQOCORR.

           WRITE REG-COBRANCA OF ARQOCORR.

           PERFORM 113000-TESTAR-FS-ARQOCORR.

           ADD 1                           TO ACU-GRAVADOS-ARQOCORR.

      *----------------------------------------------------------------*
       312000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       320000-CALCULAR-DIGITO          SECTION.
      *----------------------------------------------------------------*

           MOVE COBR-CONTA-DEB         OF ARQCOBR
                                       TO WRK-AGENCIA-0431.
      *    MOVE  04                    TO WRK-TAMANHO-0431.
           MOVE  07                    TO WRK-TAMANHO-0431.
           MOVE  '7'                   TO WRK-DIGITO-0431.

           CALL 'POOL0431'             USING WRK-AGENCIA-0431
                                             WRK-DIGITO-0431
                                             WRK-TAMANHO-0431.

           IF  WRK-DIGITO-0431         EQUAL '.' OR 'P'
               MOVE '0'                TO WRK-DIGITO-0431
           END-IF.

           MOVE WRK-DIGITO-0431        TO COBR-DIGITO-DEB  OF ARQCOBR.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       330000-ALTERAR-STATUS           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5525-AREA-SAIDA.

           MOVE 'B'                    TO WRK-5525-AMBIENTE.
           MOVE 'GFCT1170'             TO WRK-5525-CHAMADOR.
           MOVE A3-DENVIO-MOVTO-TARIF  TO WRK-5525-DENVIO-MOVTO.
           MOVE A3-CROTNA-ORIGE-MOVTO  TO WRK-5525-CROTNA-ORIGE.
           MOVE A3-CNRO-ARQ-MOVTO      TO WRK-5525-CNRO-ARQ.
           MOVE A3-CSEQ-MOVTO          TO WRK-5525-CSEQ-MOVTO.
           MOVE 13                     TO WRK-5525-CSIT-EVNTO.
070528*    MOVE 'S'                    TO WRK-5525-CORRENTISTA.
070528     MOVE 'H'                    TO WRK-5525-CORRENTISTA.
           MOVE 'GFCT5525'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5525-AREA-ENTRADA
                                             WRK-5525-AREA-SAIDA
                                             GFCT0M-AREA-ERROS
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  WRK-5525-COD-RETORNO    NOT EQUAL ZEROS
               MOVE WRK-5525-COD-RETORNO TO ERRO-COD-RETORNO
               MOVE WRK-5525-DESC-MSG    TO ERRO-DESCRICAO
               PERFORM 312000-GRAVAR-ARQOCORR
           END-IF.


      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS               GREATER ZEROS
               PERFORM 410000-EFETUAR-COMMIT
           END-IF.

           CLOSE ARQB092
                 ARQCOBR
                 ARQOCORR.

PERJS      CLOSE  SAIB092

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           PERFORM 420000-EMITIR-DISPLAY.

           EXEC SQL
               CLOSE CSR01-GFCTB031
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'CURSOR31'         TO WRK-ARQUIVO
               MOVE 'CLOSE'            TO ERR-TEXTO
               MOVE 'CSR'              TO ERR-TIPO-ACESSO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       410000-EFETUAR-COMMIT           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               COMMIT
           END-EXEC.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'COMMIT'           TO ERR-DBD-TAB
                                          ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       420000-EMITIR-DISPLAY           SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS              TO WRK-MASCARA.

           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*              PROGRAMA GFCT1170                 *'.
           DISPLAY '*              -----------------                 *'.
           DISPLAY '*        PROCESSAMENTO EFETUADO COM SUCESSO      *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '* REGISTROS LIDOS EM ARQB092....: ' WRK-MASCARA
                                                                   ' *'.

ILZA       MOVE WRK-ACU-LID-TAB31      TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS TAB GFCTB031..: ' WRK-MASCARA
                                                                   ' *'.
           MOVE ACU-GRAVADOS           TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS EM ARQCOBR.: ' WRK-MASCARA
                                                                   ' *'.

070528     MOVE ACU-ATU-92             TO WRK-MASCARA.
070528
PERJS      DISPLAY '* REGISTROS GRAVADOS     SAIB092: ' WRK-MASCARA
070528                                                             ' *'.

070528     MOVE ACU-ATU-H4             TO WRK-MASCARA.
070528
070528     DISPLAY '* REGISTROS GRAVADOS GFCTB0H4   : ' WRK-MASCARA
070528                                                             ' *'.

           MOVE ACU-GRAVADOS-ARQOCORR  TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS EM ARQOCORR: ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*------------------------------------------------*'.

      *----------------------------------------------------------------*
       420000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1170'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

