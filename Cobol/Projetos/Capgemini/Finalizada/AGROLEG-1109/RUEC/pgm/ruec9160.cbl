       IDENTIFICATION DIVISION.

       PROGRAM-ID.    RUEC9160.
       AUTHOR.        DOUGLAS LIRA
      *--------------------------------------------------------------*
      *               RUEC9160        -       CPM SISTEMAS           *
      *                                                              *
      *      PROGRAMA   : RUEC9160                                   *
      *      PROGRAMADOR: DOUGLAS LIRA     -  CPM                    *
      *      ANALISTA   : RENE             -  CPM                    *
      *      DATA       : OUTUBRO / 1998                             *
      *                                                              *
      *------TABELAS DB2---------------------------------------------*
      *                                                              *
      *      NOME DA TABELA              VISAO     CHAVE DE ACESSO   *
      *      --------------              -----     ---------------   *
      *                                                              *
      *      DB2PRD.CONTRATO_RURAL       RUFIB002                    *
      *      DB2PRD.CTRL_HIST_IMPRE      RUECB033                    *
      *      DB2PRD.CTRL_HIST_IMPRE      RUECB034                    *
      *      DB2PRD.V01CEDULA_TEMPR      RUECV007                    *
      *      DB2PRD.V01PSSOA_CDULA       RUECV008                    *
      *      DB2PRD.V01INCON_CEDULA1     RUECV019                    *
      *      DB2PRD.DOCTO_ASSOC_RURAL    RURCB053
      *      DB2PRD.OBS_DEVLC_RURAL      RUECB045
      *                                                              *
      *------OBJETIVO------------------------------------------------*
      *                                                              *
      *      MENU DE IMPRESSAO.                                      *
      *                                                              *
      *--------------------------------------------------------------*
POL126*                 A  L  T  E  R  A  C  A  O                    *
POL126*==============================================================*
POL126*                                                              *
POL126*  ANALISTA......: MARINA FUJITA - ALTRAN                      *
POL126*  DATA..........: ABRIL/2006                                  *
POL126*  PROJETO.......: POL 05/0126-01  ANPR 2004/2149/5-00         *
POL126*                  HISTORICO DE CEDULAS                        *
POL126*  OBJETIVO......: SUBSTITUIR TABELA RUECB045 PELA NOVA        *
POL126*                  THIST_DEVLC_CONTR                           *
POL126*                                                              *
ACT181*              U L T I M A   A L T E R A C A O                  *
ACT181*===============================================================*
ACT181*                                                               *
ACT181*     ANALISTA ......: LEANDRO LIMA         - BRAD  / GP.70     *
ACT181*     ANALISTA ......: HEYDI RAFAELA MARIGO - ALTRAN            *
ACT181*     DATA ..........: 10/12/2007                               *
ACT181*                                                               *
ACT181*     OBJETIVO ......: VERIFICAR ULTIMO DIA DO ANO SE           *
ACT181*                      FERIADO / UTIL.                          *
ACT181*                                                               *
ACT181*     PROJETO .......: ACT 07/181-01  ANPR 2007/0164/3-03       *
ACT181*                      PROCEDIMENTO FINAL DE ANO                *
ACT181*                                                               *
ACT181*===============================================================*
ACT211*              U L T I M A   A L T E R A C A O                  *
ACT211*===============================================================*
ACT211*                                                               *
ACT211*     ANALISTA ......: LEANDRO LIMA         - BRAD  / GP.70     *
ACT211*     ANALISTA ......: HEYDI RAFAELA MARIGO - ALTRAN            *
ACT211*     DATA ..........: 22/02/2008                               *
ACT211*                                                               *
ACT211*                      PARA OPERACOES BNDES.                    *
ACT211*                                                               *
ACT211*     PROJETO .......: ACT 07/211-01  ANPR 2007/1226/5-00       *
ACT211*                      DECLARACAO OPERACOES BNDES               *
ACT211*                                                               *
ACT211*===============================================================*
ACT046*                        A L T E R A C A O                       *
ACT046*----------------------------------------------------------------*
ACT046*                                                                *
ACT046*  ANALISTA  : LEANDRO LIMA   - BRAD / GP.70                     *
ACT046*  ANALISTA  : HEYDI MARIGO   - ALTRAN                           *
ACT046*  DATA      : AGOSTO/2008                                       *
ACT046*                                                                *
ACT046*  PROJETO   : ACT 08/0046-01  ANPR 2006/2637/5-00               *
ACT046*              CONTROLE DA FORMALIZACAO DAS OPERACOES DE CREDITO *
ACT046*                                                                *
ACT046*  OBJETIVO(S): INCLUIR ACESSO AO MODULO FORP9120 PARA INCLUSAO  *
ACT046*               DE CONTRATO NO SISTEMA DE FORMALIZACAO.          *
ACT046*                                                                *
ACT046*================================================================*
PW7398*                        A L T E R A C A O                       *
PW7398*                                                                *
PW7398*  ANALISTA  : MARINA FUJITA - PROCWORK                          *
PW7398*  DATA      : MAIO/2009                                         *
PW7398*                                                                *
PW7398*  PROJETO   : PWT 08/7398-01  ANPR 2006/1809/5-00               *
PW7398*              AJUSTAR PROCESSO DE CONTRATACAO  BNDES            *
PW7398*                                                                *
PW7398*  OBJETIVO(S): PERMITIR REGERACAO IMPRESSAO 4 VIAS DO CONTRATO  *
PW7398*               SALVANDO VERSAO ANTERIOR                         *
PW7398*                                                                *
PW7398*================================================================*
PW3932*                        A L T E R A C A O                       *
PW3932*----------------------------------------------------------------*
PW3932*                                                                *
PW3932*  ANALISTA  : MARINA FUJITA - PROCWORK                          *
PW3932*  DATA      : SETEMBRO/2009                                     *
PW3932*                                                                *
PW3932*  PROJETO   : PWT 09/3932-01  ANPR 2009/XXXXX/5-01              *
PW3932*              PROGER - PROGRAMA DE GERACAO EMPREGO E CRED.RURAL *
PW3932*                                                                *
PW3932*  OBJETIVO(S): CHAMAR MODULO RUEC7770 PARA ENQUADRAMENTO DO     *
PW3932*                                                                *
PW3932*================================================================*
POL097*   ANALISTA      : MARINA FUJITA - POLEN TECHNOLOGIES          *
POL097*   DATA          : JULHO/2005 - REALTERADO EM ABRIL/2010       *
POL097*   PROJETO       : POL 0097/04 - RUEC X CADU                   *
POL097*   OBJETIVO      : SUBSTITUIR TABELAS E PROCEDIMENTOS FICA     *
POL097*                   POR CADU E CADASTRO RURAL RUEC              *
POL097*================================================================*
PW6062*                            A L T E R A C A O                   *
PW6062*================================================================*
PW6062*                                                                *
PW6062*     ANALISTA ......: MARINA FUJITA        - SONDA PROCWORK     *
PW6062*     DATA ..........: JANEIRO/2011                              *
PW6062*     PROJETO........: PWT 10/6062  2009/34565/5-09 10 11        *
PW6062*     OBJETIVO ......: ACESSO MULTIPLA JUNCAO DCO                *
PW6062*                      ADEQUAR/INCLUIR ACESSO MIDD9000           *
PW6062*================================================================*
PW2875*                            A L T E R A C A O                   *
PW2875*================================================================*
PW2875*                                                                *
PW2875*     ANALISTA ......: DANILO KIOROGLO      - SONDA PROCWORK     *
PW2875*                                                                *
PW2875*     OBJETIVO ......: MELHORIAS NO PROCESSO DE GERACAO          *
PW2875*                      RECOR/BACEN.                              *
PW2875*                      ENQUADRAMENTO NA PARAMETRIZACAO DE PRAZO  *
PW2875*                      E ACERTO NAS DATAS DAS PARCELAS           *
PW2875*================================================================*
PW9931*    ANALISTA....:   MARINA FUJITA           - SONDA PROCWORK    *
PW9931*    DATA........:   21/06/2012                                  *
PW9931*    PROJETO.....:   PWT 12/9931    2012/81536/5-01              *
PW9931*                    REFORMULACAO MENSAGERIA BACEN - RECOR       *
PW9931*    OBJETIVO....:   CHAMAR MODULO RUEC7764 E RUEC8808           *
PW9931*                    P/ GERAR PROPRIETARIOS NO RUECV014 E        *
PW9931*                    GERAR DADOS P/ ENVIO RECOR                  *
PW9931*----------------------------------------------------------------*
      *                                                                *
      * BSI TECNOLOGIA.                                                *
      *                                                                *
      * ALTERACAO: BS0250  - ARQUIVO FORNECEDORES VERSAO 4.0           *
      *                      ALTERAR MOVAS CONDICOES DO CSIT-MSGEM     *
      * DATA.....: OUTUBRO/2013                                        *
      * ANALISTA.: DANILO KIOROGLO                                     *
      *----------------------------------------------------------------*
BS0281*           U L T I M A    A L T E R A C A O                     *
BS0281*================================================================*
BS0281*                                                                *
BS0281* PROGRAMADOR  :  DANILO KIOROGLO    - B S I                     *
BS0281* ANALISTA     :  DANILO KIOROGLO    - B S I                     *
BS0281* DATA         :  12/2013                                        *
BS0281* PROJETO      :  BSI-13/0281                                    *
BS0281* ANPR         :  2013/00000/5-01                                *
BS0281*                                                                *
BS0281* OBJETIVO     :  ENQUADRAR OPERACOES PERTENCENTES AO            *
BS0281*                 SEMIARIDO - SUDENE                             *
      *----------------------------------------------------------------*
EG1115*================================================================*
EG1115*           U L T I M A    A L T E R A C A O                     *
EG1115*================================================================*
EG1115*                                                                *
EG1115* ANALISTA     :  EVANDRO GUIMARAES                              *
EG1115* DATA         :  24/11/2015                                     *
EG1115*                                                                *
EG1115* OBJETIVO     :  TRATAR NOVOS CODIGOS DE RETORNO DO MODULO      *
EG1115*                                                                *
EG1115*================================================================*
EG1115
EG1215*================================================================*
EG1215*                   B S I   T E C N O L O G I A                  *
EG1215*----------------------------------------------------------------*
EG1215*     PROGRAMADOR.: EVANDRO GUIMARAES                            *
EG1215*     ANALISTA....: EVANDRO GUIMARAES                            *
EG1215*     DATA........: 12/2015                                      *
EG1215*-0--------------------------------------------------------------*
EG1215*     OBJETIVO....: TRATAR COORDENADAS GEODESICAS DOS IMOVEIS    *
EG1215*----------------------------------------------------------------*
BS0216*================================================================*
BS0216*                   B S I   T E C N O L O G I A                  *
BS0216*----------------------------------------------------------------*
BS0216*     PROGRAMADOR.: EDSON MATSUMOTO                              *
BS0216*     ANALISTA....: EDSON MATSUMOTO
BS0216*     DATA........: 02/2016                                      *
BS0216*----------------------------------------------------------------*
BS0216*     OBJETIVO....: ALTERADO PARA MOSTRAR AS MSG DE INCONSISTEN  *
BS0216*                   CIAS DOS PRODUTO PERENE - MODULO RUEC8810    *
BS0216
BS0616*================================================================*
BS0616*                   B S I   T E C N O L O G I A                  *
BS0616*----------------------------------------------------------------*
BS0616*     PROGRAMADOR.: EDUARDO WATANABE                             *
BS0616*     DATA........: 06/2016                                      *
BS0616*----------------------------------------------------------------*
BS0616*     OBJETIVO....: ALTERACAO ORIGEM DE RECURSOS DOS CONTRATOS   *
BS0616*                   QUE SE ENQUADRAREM NAS REGRAS DO PROAGRO:    *
BS0616*                   - CUSTEIO AGRICOLA                           *
BS0616*                   - RECURSOS OBRIGATORIOS                      *
BS0616*                   - EMPREENDIMENTOS ESPECIFICOS                *
BS0616*                   - VALOR FINANCIADO MENOR OU IGUAL A 300.000  *
BS0616*----------------------------------------------------------------*
BS0616
BS0617*================================================================*
BS0617*                   B S I   T E C N O L O G I A                  *
BS0617*----------------------------------------------------------------*
BS0617*     PROGRAMADOR.: EDSON MATSUMOTO                              *
BS0617*     ANALISTA....: EDSON MATSUMOTO
BS0617*     DATA........: 06/2017                                      *
BS0617*     OBJETIVO....: VERIFICAR SE A PRODUTIVIDADE ESTA PREENCHIDA *
BS0617*                   PARA OPERACOES EXIGIDAS                      *
BS0617*----------------------------------------------------------------*
DS0620*===============================================================*
DS0620*                                                               *
DS0620*     ANALISTA : TANIA COSTA             - DS BRADESCO          *
DS0620*     DATA     : JUNHO / 2020                                   *
DS0620*     OBJETIVO : CORRECAO DO IN4146358                          *
DS0620*                NAO EXIGIR IMOVEL PARA O EMPREENDIMENTO CAFE   *
DS0620*                                                               *
DS0620*                CORRECAO DO IN4148000                          *
DS0620*                RETIRAR OS REGISTROS FIXOS DE PISCICULTURA     *
DS0620*                CONFORME SOLICITADO PELO GESTOR                *
DS0620*                ESTAMOS DESFAZENDO A CORRECAO DO INCIDENTE     *
DS0620*                IN3351458                                      *
DS0620*===============================================================*
WI0521*===============================================================*
WI0521*                                                               *
WI0521*     ANALISTA : MILTON JANUARIO         - WIPRO                *
WI0521*     DATA     : MAIO  / 2021                                   *
WI0521*     OBJETIVO : NAO EXIGIR IMOVEL PARA O EMPREENDIMENTO        *
WI0521*                                                               *
WI0521*===============================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
NAV506*  ANALISTA......:   NAVA                                        *
NAV506*  DATA..........:   MAIO/2023                                   *
NAV506*----------------------------------------------------------------*
NAV506*  OBJETIVO......:   ADEQUACOES SICOR 5.06                       *
NAV506*                    RECOMPILAR ALTERACAO I#RUEC65               *
NAV506*================================================================*
STE001*                ALTERACAO - STEFANINI -                         *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ANALISTA REPONSAVEL: STEFANINI                                 *
.     * DATA...............: MAIO / 2024                               *
STE001*                      MANEJO                                    *
STE001*----------------------------------------------------------------*
7C0325*================================================================*
.     * ANALISTA REPONSAVEL: EDSON/NIKSON                              *
.     * DATA...............: MARCO/2025                                *
.     * OBJETIVOS..........: FLASH - AGRO2-12236, VALIDAR DATA DA DAP, *
.     *                      SO PODERA SER MAIOR OU IGUAL A DATA ATUAL *
7C0325*================================================================*
7C0625*================================================================*
.     * ANALISTA REPONSAVEL: EDSON/NIKSON                              *
.     * DATA...............: junho/2025                                *
.     * OBJETIVOS..........: FLASH - AGRO7-441 / AGRO7-442 / AGRO7-435 *
.     *                      ALTERAR PARA ENVIAR A SOLICITACAO DAS     *
.     *                      RESPONSABILIDADES DOS COOPERADOS (ATUAL)  *
.     *                      E TAMBEM DE TODOS OS CLIENTES             *
7C0625*================================================================*
7C5.10*================================================================*
.     * ANALISTA REPONSAVEL: EDSON/NIKSON                              *
.     * DATA...............: AGOSTO/2025                               *
.     * OBJETIVOS..........: FLASH - AGRO7-000 - FORMATAR O CPF COM    *
.     *                      BRANCOS A DIREITA                         *
DPS01 * ANALISTA REPONSAVEL: DESIREE PESSOA                            *
.     * DATA...............: SETEMBRO/2025                             *
.     * OBJETIVOS..........: ASTERISCAR IF PARA PODER IMPRIMIR FORMULA-*
.     *                      RIO EM 4 VIAS DE CONTRATO COM MAIS DE 34  *
.     *                      PARCELAS.                                 *
DPS01 *================================================================*
7C1225* ANALISTA REPONSAVEL: VINICIUS MEIRELLES                        *
.     * DATA...............: DEZEMBRO/2025                             *
.     * OBJETIVOS..........: INCLUSAO DE MENSAGEM QUANDO RETORNAR QUE  *
.     *                      CLIENTE NAO POSSUI RENDA BRUTA NO CADU    *
.     *                      APOS MES DE JUNHO DE CADA ANO.            *
7C1225*================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
      *    CAMPOS UTILIZADOS PELA POOL5000                             *

HEXA   77  WRK-NULO-CCONTR                 PIC S9(04) COMP VALUE ZEROS.
7C0325 77  WRK-NULO-CDECLR-AGCLT           PIC S9(04) COMP VALUE ZEROS.
7C0325 77  WRK-NULO-DVALDD-DECLR           PIC S9(04) COMP VALUE ZEROS.
7C0325 77  WRK-DTDAP-OK                    PIC X(001)  VALUE SPACES.
       77  WRK-FUNCAO                      PIC X(004)  VALUE SPACES.
       77  WRK-TELA                        PIC X(008)  VALUE SPACES.
       77  WRK-OPCAO                       PIC 9(002)  VALUE ZEROS.
       77  WRK-COD-USER                    PIC X(007)  VALUE SPACES.
       77  WRK-OPCAO-ATUAL                 PIC 9(002)  VALUE ZEROS.
PW6062 77  WRK-PERM-OK                     PIC X(001)  VALUE SPACES.
       77  WRK-FIM-RUECB034                PIC X(001)  VALUE 'N'.
POL097 77  WRK-FIM-RUECB052                PIC X(001)  VALUE 'N'.
POL097 77  WRK-FIM-RUECB053                PIC X(001)  VALUE 'N'.
POL097 77  WRK-FIM-RUECB054                PIC X(001)  VALUE 'N'.
EG1215 77  WRK-FIM-RUECB040                PIC X(001)  VALUE 'N'.
BS0617 77  WRK-FIM-RUFIB002                PIC X(001)  VALUE 'N'.
EG1215 77  WRK-COORD-INCONSIS              PIC X(001)  VALUE 'N'.
       77  WRK-EXISTE-RUECB033             PIC X(001)  VALUE 'N'.
       77  WRK-EXISTE-RUECB034             PIC X(001)  VALUE 'N'.
PW9931 77  WRK-QT-PROPRIET                 PIC S9(09) COMP VALUE ZEROS.
EG1215 77  WRK-QTDE-COORDENADAS            PIC S9(09) COMP VALUE ZEROS.
MF0113 77  WRK-TOT-PARC                    PIC S9(13)V99 COMP-3 VALUE 0.
MF0113 77  WRK-TOT-LIBER                   PIC S9(13)V99 COMP-3 VALUE 0.
MF0113 77  WRK-EVNTO-INI                   PIC S9(04)    COMP-3 VALUE 0.
MF0113 77  WRK-EVNTO-FIM                   PIC S9(04)    COMP-3 VALUE 0.

BR1217 77  WRK-CMSGEM-RECOR                PIC X(009)      VALUE SPACES.
BR1217 77  WRK-CMSGEM-RECOR-E              PIC X(009)      VALUE SPACES.
BR1217 77  WRK-CMSGEM-RECOR-NULL           PIC S9(04) COMP VALUE ZEROS.
BR1217 77  WRK-RCOR                        PIC  X(01)      VALUE SPACES.

STEFAB 01  FILLER.
STEFAB     03  WRK-N05                 PIC +9(05)      VALUE ZEROS.
STEFAB     03  FILLER  REDEFINES  WRK-N05.
STEFAB         05  FILLER              PIC  X(01).
STEFAB         05  WRK-N05S            PIC  9(05).

BR1811 01  WRK-TIMES-1S                PIC X(26)         VALUE SPACES.
BR1811 01  WRK-TIMESTAMP-1S            PIC X(26)         VALUE SPACES.
BR1811
BR1811     03  WRK-PO-JULIANA          PIC 9(05) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-AAMMDD           PIC 9(07) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-AAAAMMDD         PIC 9(09) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-HHMMSS           PIC 9(07) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-HHMMSSMMMMMM     PIC 9(13) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-TIMESTAMP        PIC X(20)         VALUE SPACES.
BR1811

      *   DEFINICAO DA AREA DA PARA PROGRAMA DE IMPRESSAO - RUEC9171  *

PW7398 01  WRK-COD-USER-R.
PW7398     03  WRK-COD-USER-N              PIC 9(07)   VALUE ZEROS.

       01  WRK-COD-DEPTO.
HEXA       03  FILLER                      PIC X(002) VALUE '00'.
HEXA       03  WRK-COD-DEPTO-N             PIC 9(004) VALUE ZEROS.
       01  WRK-MENSAGEM.
HEXA       03  FILLER                      PIC X(0004) VALUE SPACES.
HEXA       03  WRK-TRANSACAO               PIC X(0008) VALUE SPACES.
HEXA       03  FILLER                      PIC X(1988) VALUE SPACES.


       77  WRK-GU                          PIC X(04) VALUE 'GU  '.
       77  WRK-ISRT                        PIC X(04) VALUE 'ISRT'.
       77  WRK-CHNG                        PIC X(04) VALUE 'CHNG'.

      *    CAMPOS AUXILIARES                                          *

       77  WRK-CONFIRM-1                   PIC  X(22) VALUE
           'CONFIRME REIMPRESSAO :'.

       77  WRK-CONFIRM-2                   PIC  X(24) VALUE
           '(DISPONIVEL PARA AMANHA)'.

POL097 77  WRK-RUEC7720                PIC X(008)    VALUE 'RUEC7720'.
POL097 01  WRK-RUEC7728                PIC X(008)    VALUE 'RUEC7728'.
ACT046 77  WRK-MODULO                  PIC X(008)    VALUE SPACES.
EG1215 77  WRK-RUEC7941                PIC X(008)    VALUE 'RUEC7941'.
PW9931 77  WRK-CSIT-MSGEM-NULL         PIC S9(04)  COMP VALUE ZEROS.
ACT046 77  WRK-CORIGE-REC-NULL         PIC S9(04)  COMP VALUE ZEROS.
ACT046 77  WRK-CATVDD-RURAL-NULL       PIC S9(04)  COMP VALUE ZEROS.
ACT046 77  WRK-CFNALD-RURAL-NULL       PIC S9(04)  COMP VALUE ZEROS.
BS0617 77  WRK-CUND-MEDD-FINCD-NULL    PIC S9(04)  COMP VALUE ZEROS.
       77  WRK-VSUPE-PARM-RURAL        PIC S9(04)  COMP VALUE ZEROS.

BS0617 01  WRK-EXIGE-PRODUT            PIC  X(01)          VALUE SPACES.
BS0617 01  WRK-QTD-EXIGE-PRODUT        PIC S9(09) COMP     VALUE ZEROS.
BS0617 01  WRK-NREGRA-PROTV-ADQUI      PIC S9(05) COMP     VALUE ZEROS.
BS0617 01  WRK-CORIGE-REC              PIC S9(03) COMP-3   VALUE ZEROS.
BS0617 01  WRK-CFNALD-RURAL            PIC S9(03) COMP-3   VALUE ZEROS.
BS0617 01  WRK-CATVDD-RURAL            PIC S9(03) COMP-3   VALUE ZEROS.
BS0617 01  WRK-VFINAN                  PIC S9(15)V9(2)
BS0617                                            COMP-3   VALUE ZEROS.

7C0325 01  WRK-DATAS-C.
.          03  WRK-DD-C                PIC 99     VALUE ZEROS.
.          03  FILLER                  PIC X      VALUE SPACES.
.          03  WRK-MM-C                PIC 99     VALUE ZEROS.
.          03  FILLER                  PIC X      VALUE SPACES.
.          03  WRK-AA-C                PIC 9999   VALUE ZEROS.
.
.      01  WRK-DATAS-D.
.          03  WRK-AA-D                PIC 9999   VALUE ZEROS.
.          03  WRK-DD-D                PIC 99     VALUE ZEROS.
7C0325 01  WRK-DATAS-D-R REDEFINES WRK-DATAS-D PIC 9(8).

ACT046 01  WRK-CCART5                  PIC  9(05)  VALUE ZEROS.
ACT046 01  FILLER        REDEFINES     WRK-CCART5.
ACT046     03  FILLER                  PIC 9(02).
ACT046     03  WRK-CCART3              PIC 9(03).
ACT046     03  WRK-CCART3-R  REDEFINES WRK-CCART3  PIC X(03).

PW9931 01  FILLER.
PW9931     03  WRK-HRMIN-4VIAS         PIC S9(13)V9(4)  VALUE ZEROS.
PW9931     03  FILLER  REDEFINES  WRK-HRMIN-4VIAS.
PW9931         05  FILLER              PIC  X(10).
PW9931         05  WRK-HMSMIN-4VIAS    PIC  9(07).

PW9931     03  WRK-HRMAX-4VIAS         PIC S9(13)V9(4)  VALUE ZEROS.
PW9931     03  FILLER  REDEFINES  WRK-HRMAX-4VIAS.
PW9931         05  FILLER              PIC  X(10).
PW9931         05  WRK-HMSMAX-4VIAS    PIC  9(07).

ACT046*----------------------------------------------------------------*
ACT046*----------------------------------------------------------------*

ACT046 01  WRK-COMU-DADOS-9015.
ACT046     05  COMU-ENTRADAS.
ACT046         10 WRK-COMU-NOME-PGM    PIC X(08)           VALUE SPACES.
ACT046         10 WRK-COMU-COD-RETORNO PIC 9(02)           VALUE ZEROS.
ACT046         10 WRK-COMU-EMPR        PIC 9(03)           VALUE ZEROS.
ACT046         10 WRK-COMU-DEPDC       PIC 9(05)           VALUE ZEROS.
ACT046     05  WRK-COMU-SAIDAS.
ACT046         10 WRK-COMU-DG-DEPDC    PIC X(01)           VALUE SPACES.
ACT046         10 WRK-COMU-NOME-DEPDC  PIC X(40)           VALUE SPACES.
ACT046         10 WRK-COMU-END-DEPDC   PIC X(30)           VALUE SPACES.
ACT046         10 WRK-COMU-DIRETORIA-REG
ACT046                                 PIC 9(05)           VALUE ZEROS.
ACT046         10 WRK-COMU-DIRETORIA-NOM
ACT046                                 PIC X(40)           VALUE SPACES.
ACT046         10 WRK-COMU-NOME-EMPR   PIC X(40)           VALUE SPACES.
ACT046         10 WRK-COMU-POLO-SERVICO
ACT046                                 PIC 9(05)           VALUE ZEROS.
ACT046         10 WRK-COMU-MUNIC-DEPDC PIC X(25)           VALUE SPACES.
ACT046         10 WRK-COMU-UF          PIC X(02)           VALUE SPACES.
ACT046         10 WRK-COMU-CEP-COMPL   PIC 9(03)           VALUE ZEROS.
ACT046         10 WRK-COMU-INDIC-DEPDC-AT
ACT046                                 PIC X(01)           VALUE SPACES.
ACT046         10 WRK-COMU-NATUREZA-DEPDC
ACT046                                 PIC X(01)           VALUE SPACES.
ACT046         10 WRK-COMU-COD-DISTR-CORR
ACT046                                 PIC 9(05)           VALUE ZEROS.
ACT046         10 WRK-COMU-DENCRR-DEPDC-BDSCO
ACT046                                 PIC X(10)           VALUE SPACES.
ACT046         10 WRK-COMU-FILLER      PIC X(35)           VALUE SPACES.
ACT046 01  WRK-COMU-POOL7100           PIC X(107)          VALUE SPACES.
ACT046 01  WRK-COMU-SQLCA              PIC X(136)          VALUE SPACES.

ACT046*----------------------------------------------------------------*
ACT046 77  FILLER PIC X(35) VALUE '* AREA DO MESU9023                *'.
ACT046*----------------------------------------------------------------*

ACT046 01  WRK-COMU-PARM-9023.
ACT046     03  WRK-COMU-ENTRADA-9023.
ACT046         05  WRK-COMU-NOME-PGM-9023       PIC X(08) VALUE SPACES.
ACT046         05  WRK-COMU-COD-RETORNO-9023    PIC 9(02) VALUE ZEROS.
ACT046         05  WRK-COMU-EMPRESA-9023        PIC 9(05) VALUE ZEROS.
ACT046         05  WRK-COMU-DEPDC-9023          PIC 9(05) VALUE ZEROS.
ACT046      03 WRK-COMU-SAIDAS-9023.
ACT046         05  WRK-COMU-EMPR-RELAC-9023     PIC 9(05) VALUE ZEROS.
ACT046         05  WRK-COMU-DEPTO-RELAC-9023    PIC 9(05) VALUE ZEROS.
ACT046         05  WRK-COMU-NOME-DEPDC-9023     PIC X(50) VALUE SPACES.
ACT046    01  WRK-COMU-AMBIENTE-9023            PIC X(01) VALUE SPACES.
ACT046    01  WRK-COMU-IO-PCB-9023              PIC X(32) VALUE SPACES.
ACT046    01  WRK-COMU-ALT-PCB-9023             PIC X(32) VALUE SPACES.
ACT046    01  WRK-COMU-POOL7100-9023            PIC X(107) VALUE SPACES.
ACT046    01  WRK-COMU-SQLCA-9023               PIC X(136) VALUE SPACES.

      *    CAMPOS AUXILIARES UTILIZADOS NA POOL7600                    *

       01  DATA-HORA.
HEXA       03  DT-JULIANA                  PIC 9(05) COMP-3 VALUE ZEROS.
HEXA       03  DT-AAMMDD                   PIC 9(07) COMP-3 VALUE ZEROS.
HEXA       03  DT-AAAAMMDD                 PIC 9(09) COMP-3 VALUE ZEROS.
HEXA       03  TI-HHMMSS                   PIC 9(07) COMP-3 VALUE ZEROS.
HEXA       03  TI-HHMMSSMMMMMM             PIC 9(13) COMP-3 VALUE ZEROS.
HEXA       03  TIMESTAMP                   PIC X(20)       VALUE SPACES.
PW2875 01  WRK-DATA-AUX-2.
PW2875     03  FILLER                      PIC 9(01) VALUE ZEROS.
PW2875     03  WRK-ANO-AUX-2               PIC X(04) VALUE ZEROS.
PW2875     03  WRK-MES-AUX-2               PIC X(02) VALUE ZEROS.
PW2875     03  WRK-DIA-AUX-2               PIC X(02) VALUE ZEROS.
PW2875 01  WRK-DATA-AUX-R2 REDEFINES WRK-DATA-AUX-2 PIC 9(09).

       01  WRK-DATA-AUX.
HEXA       03  WRK-ANO-AUX                 PIC 9(04) VALUE ZEROS.
HEXA       03  WRK-MES-AUX                 PIC 9(02) VALUE ZEROS.
HEXA       03  WRK-DIA-AUX                 PIC 9(02) VALUE ZEROS.
       01  WRK-DATA-AUX-R REDEFINES WRK-DATA-AUX PIC 9(08).

7C0325 01  WRK-DATA-DIA                    PIC 9(08) VALUE ZEROS.

       01  WRK-DATA-CERTA.
HEXA       03  WRK-DIA-AUX                 PIC 9(02) VALUE ZEROS.
           03  FILLER                      PIC X(01) VALUE '/'.
HEXA       03  WRK-MES-AUX                 PIC 9(02) VALUE ZEROS.
           03  FILLER                      PIC X(01) VALUE '/'.
HEXA       03  WRK-ANO-AUX                 PIC 9(04) VALUE ZEROS.
PW7398 01  WRK-CODIFIC.
PW7398     03   WRK-CJUNC-DEPDC        PIC 9(05)   VALUE ZEROS.
PW7398     03   WRK-CCDULA             PIC 9(09)   VALUE ZEROS.
PW7398     03   WRK-SIT-IMPRE          PIC X(02)   VALUE SPACES.
PW7398     03   FILLER                 PIC X(05)   VALUE SPACES.

ACT181 01  WRK-MENS01                  PIC  X(79)   VALUE SPACES.

ACT181 01      FILLER.
HEXA       03  WRK-DDMMAAAA-COR        PIC 9(08) VALUE ZEROS.
ACT181     03  FILLER  REDEFINES  WRK-DDMMAAAA-COR.
ACT181         05  DIA-C               PIC 9(02).
ACT181         05  MES-C               PIC 9(02).
ACT181         05  ANO-C               PIC 9(04).
HEXA       03  WRK-AAAAMMDD-FIM        PIC 9(08) VALUE ZEROS.
ACT181     03  FILLER  REDEFINES  WRK-AAAAMMDD-FIM.
ACT181         05  ANO-F               PIC 9(04).
ACT181         05  MES-F               PIC 9(02).
ACT181         05  DIA-F               PIC 9(02).
HEXA       03  WRK-DDMMAAAA-FIM        PIC 9(08) VALUE ZEROS.
ACT181     03  FILLER  REDEFINES  WRK-DDMMAAAA-FIM.
ACT181         05  F-MES               PIC 9(02).
ACT181         05  F-ANO               PIC 9(04).

ACT181*---------------------------------------------------------------*
ACT181*        AREA RESERVADA  PARA   POOL1205                        *
ACT181*---------------------------------------------------------------*

ACT181 01  WRK-DADOS-POOL1205.
ACT181     05  WRK-1205-DT-ENVIADA      PIC  9(009) VALUE ZEROS COMP-3.
ACT181     05  WRK-1205-OPCAO           PIC  X(001) VALUE SPACES.
ACT181     05  WRK-1205-DT-JULIANA      PIC  9(007) VALUE ZEROS COMP-3.
ACT181     05  WRK-1205-DT-EDITADA      PIC  X(010) VALUE SPACES.
ACT181     05  WRK-1205-DT-GREGORI      PIC  9(008) VALUE ZEROS.
ACT181     05  WRK-1205-DIA-SEMANA      PIC  X(013) VALUE SPACES.
ACT181     05  WRK-1205-MES-EXTENSO     PIC  X(009) VALUE SPACES.
ACT181     05  WRK-1205-DT-UT-ANTER     PIC  9(008) VALUE ZEROS.
ACT181     05  WRK-1205-DT-UT-POSTE     PIC  9(008) VALUE ZEROS.

ACT181 01  WRK-1205-MENSAGEM           PIC  X(50)    VALUE SPACES.

      *    MENSAGENS ENVIADAS AO USUARIO                               *
       77  WRK-M1                          PIC  X(79)   VALUE
           'PF INVALIDA'.

       77  WRK-M2                          PIC  X(79)   VALUE
           'SELECIONE COM "X" AS OPCOES DESEJADAS E TECLE <ENTER> PARA I
      -    'MPRIMIR'.

       77  WRK-M3                          PIC  X(79)   VALUE
           'SELECIONE SOMENTE UMA OPCAO DE IMPRESSAO COM UM ''X'''.

       77  WRK-M4                          PIC  X(79)   VALUE
           'DIGITE "X" NA OPCAO DE IMPRESSAO DESEJADA'.

       77  WRK-M5                          PIC  X(79)   VALUE
           'OPCAO SELECIONADA CONFLITANTE COM TODOS DOCUMENTOS'.

       77  WRK-M6                          PIC  X(79)   VALUE
           'SELECIONE OPCAO: 1 VIA OU 4 VIAS - EXCLUSIVAMENTE'.

       77  WRK-M7                          PIC  X(79) VALUE
           'CEDULA INCOMPLETA OU INEXISTENTE'.
       77  WRK-M8                          PIC  X(79) VALUE
           'PREENCHA OS DADOS E SELECIONE UMA OPCAO PARA IMPRESSAO'.

       77  WRK-M9                          PIC  X(79) VALUE
           'CAMPO(S) BRILHANTE(S) INCONSISTENTE(S)'.

       77  WRK-M10                         PIC  X(79) VALUE
           'IMPRESSAO EFETUADA COM SUCESSO'.

       77  WRK-M11                         PIC  X(79) VALUE
           'IMPRESSORA NAO DISPONIVEL'.

       77  WRK-M12                         PIC  X(79) VALUE
           'HOUVE PROBLEMAS NA IMPRESSAO'.

       77  WRK-M13                         PIC  X(79) VALUE
           'CEDULA INCONSISTENTE. DISPONIVEL SOMENTE IMPRESSAO DE CONFER
      -    'ENCIA'.

       01  WRK-M14.
           03  FILLER                      PIC  X(32) VALUE
HEXA       03  WRK-DATA-M14                PIC  X(10) VALUE SPACES.
           03  FILLER                      PIC  X(35) VALUE
              ' - CONFIRME REIMPRESSAO COM UM "X"'.

       01  WRK-M15                         PIC  X(79) VALUE
           'A FICHA CADASTRAL FOI ALTERADA, INICIE O PROCESSO NOVAMENTE
      -    ' '.

       01  WRK-M16                         PIC  X(79) VALUE
           'AGENCIA / CEDULA INCONSISTENTE.'.

       01  WRK-M17                         PIC  X(79) VALUE
           'IMPRESSAO NAO EFETUADA.'.

MAGA   01  WRK-M18                         PIC  X(79) VALUE
ATOS       'SELECAO DE "PROPOSTA" E "ANEXOS", INVALIDOS PARA CEDULAS DO
ATOS  -    'BNDES'.

MAGA   01  WRK-M19                         PIC  X(79) VALUE
MAGA       'CHAMADA ROTINA DE IMPRESSAO INVALIDA'.

           'ERRO IMPRESSAO - NAO EXISTE TAB. DE HISTORICO MAS A LOCALIZA
      -    'CAO EH IGUAL A 1'.

       01  WRK-M21                         PIC  X(79) VALUE
           'ERRO IMPRESSAO - EXISTE TAB. DE HISTORICO MAS NAO EXISTE A T
      -    'AB. DE LOCALIZACAO'.

       01  WRK-M22                         PIC  X(79) VALUE
           'ERRO IMPRESSAO - EXISTE TABELA DE HISTORICO MAS O INDICADOR
      -    'DE LOCALIZACAO EH 2'.

       01  WRK-M23                         PIC  X(79) VALUE
           'SOLICITACAO EFETUADA - A CEDULA ESTARA DISPONIVEL PARA IMPRE
      -    'SSAO AMANHA'.

MAGA   01  WRK-M24                         PIC  X(79) VALUE
ATOS       'SELECAO DE "PROPOSTA", INVALIDA PARA CEDULAS DE BNDES'.

ATOS   01  WRK-M25                         PIC  X(79) VALUE
ATOS       'SELECAO DE "ANEXOS", INVALIDA PARA CEDULAS DE BNDES'.

ATOS       'SELECAO DE "ANEXOS", INVALIDA PARA CEDULAS DE CUSTEIO/INVEST
ATOS  -    'IMENTO'.

ATOS***01  WRK-M27                         PIC  X(79) VALUE
ATOS***    'NAO EXISTEM DOCUMENTOS ANEXOS CADASTRADOS PARA O GARANTIDOR
ATOS***-    'DA CEDULA'.

ACT211 01  WRK-M27                         PIC  X(79) VALUE
ACT211     'NAO EXISTEM DOCUMENTOS ANEXOS / DECLARACOES PARA A CEDULA IN
ACT211-    'FORMADA'.

CPM    01  WRK-M28                         PIC  X(79) VALUE
CPM        'NAO PERMITIDA A IMPRESSAO DE 4 VIAS PARA CEDULA REJEITADA'.

CPM    01  WRK-M29                         PIC  X(79) VALUE
CPM        'NAO PERMITIDA A IMPRESSAO PARA CEDULA REJEITADA'.

ACT211 01  WRK-M30                         PIC  X(79) VALUE
ACT211     'ERRO IMPRESSAO - NAO EXISTEM INFORMACOES NA TABELA DE HISTOR
ACT211-    'ICO'.

ACT211     'ERRO IMPRESSAO - INCONSISTENCIA NA CHAMADA DO MODULO RUEC775
ACT211-    '2'.

ACT211 01  WRK-M32                         PIC  X(79) VALUE
ACT211     'ERRO IMPRESSAO - INCONSISTENCIA NO TEXTO - PALAVRA COM + DE
ACT211-    '40 POSICOES'.

ACT211 01  WRK-M33                         PIC  X(79) VALUE
ACT211     'ERRO IMPRESSAO - INCONSISTENCIA NO TEXTO - PROBLEMA NA POOL
ACT211-    'DE ALINHAMENTO'.

ACT211 01  WRK-M34                         PIC  X(79) VALUE
ACT211     'ERRO IMPRESSAO - INCONSISTENCIA NA CHAMADA DAS FUNCOES IMS O
ACT211-    'U POOL0660'.

ACT211 01  WRK-M35                         PIC  X(79) VALUE
ACT211     'ERRO IMPRESSAO - INCONSISTENCIA NA CHAMADA DO MODULO MESU901
ACT211-    '5'.

ACT211 01  WRK-M36                         PIC  X(79) VALUE
ACT211     'ERRO IMPRESSAO - INCONSISTENCIA NA CHAMADA DA POOL DE VALORE

ACT046 01  WRK-M37                         PIC  X(79) VALUE
ACT046     'ERRO FORP - NAO EXISTE EXIGENCIA PARA PRODUTO OU GARANTIA'.

ACT046 01  WRK-M38                         PIC  X(79) VALUE
ACT046     'ERRO FORP - CHAVE NAO ENCONTRADA'.

ACT046 01  WRK-M39                         PIC  X(79) VALUE
ACT046     'ERRO FORP - PROBLEMAS NO ACESSO A TABELA'.

ACT046 01  WRK-M40                         PIC  X(79) VALUE
ACT046     'ERRO FORP - PARAMETRO INFORMADO NO MODULO DIFERENTE DE <I> <
ACT046-    'U> <S> <D>'.

PW7398 01  WRK-M41                         PIC  X(79) VALUE
PW7398     'OPCAO NAO PERMITIDA'.

PW7398 01  WRK-M42                         PIC  X(79) VALUE
PW7398     'REMONTAGEM 4 VIAS EFETUADA. TECLE "X" PARA IMPRESSAO'.

PW7398 01  WRK-M43                         PIC  X(79) VALUE

PW3932 01  WRK-M44                         PIC  X(79) VALUE
PW3932     'INCONSISTENCIA NA CHAMADA DO MODULO RUEC7770'.

PW3932 01  WRK-M45                         PIC  X(79) VALUE
PW3932     'CEDULA NAO ENCONTRADA NO MODULO RUEC7770'.

PW3932 01  WRK-M46                         PIC  X(79) VALUE
PW3932     'CARTEIRA NAO PARAMETRIZADA NO RURC (RUEC7770)'.

PW3932 01  WRK-M47                         PIC  X(79) VALUE
PW3932     'TAXA NAO ENCONTRADA (RUEC7770)'.

POL097 01  WRK-M48                         PIC  X(79) VALUE
POL097     'INCONSISTENCIA NA CHAMADA AO MODULO RUEC7720 - VERIFICA CEDU
POL097-    'LA CORROMPIDA'.

POL097 01  WRK-M49                         PIC  X(79) VALUE
POL097     'CCLUB NAO ENCONTRADO NO SISTEMA CADU'.

POL097 01  WRK-M50                         PIC  X(79) VALUE

PW2875 01  WRK-M51                         PIC  X(79) VALUE
PW2875     'ENQUADRAMENTO DE PRAZO INCONSISTENTE'.

PW6062 01  WRK-M54                         PIC  X(79) VALUE
PW6062     'USUARIO NAO AUTORIZADO A ACESSAR ESTA AGENCIA'.

DK0312 01  WRK-M55                         PIC  X(79) VALUE
DK0312     'INCONSISTENCIA NA CHAMADA DO MODULO RUEC7796'.

DK0312 01  WRK-M56                         PIC  X(79) VALUE
DK0312     'CEDULA NAO ENCONTRADA NO MODULO RUEC7796'.

PW9931 01  WRK-M57                         PIC  X(79) VALUE
PW9931     'INCONSISTENCIA NA CHAMADA DO MODULO RUEC7764'.

PW9931 01  WRK-M58.
PW9931     03  FILLER                      PIC  X(79) VALUE
PW9931     'EMPREENDIMENTO RECOR ALTERADO NO RURC. REENQUADRAR NA OPCAO
PW9931-    '09-DADOS FINANCIAM.'.

PW9931     03  FILLER                      PIC  X(52) VALUE
PW9931     'INCONSISTENCIA NA CHAMADA DO MODULO RUEC8810 - RC = '.
PW9931     03  WRK-M59-RC                  PIC  9(02) VALUE ZEROS.

PW9931*01  WRK-M60                         PIC  X(79) VALUE
PW9931*    'CONTRATO NAO ENCONTRADO NO MODULO RUEC8810'.

PW9931 01  WRK-M61                         PIC  X(79) VALUE
PW9931     'ENVIADO AO BACEN PARA REGISTRO RECOR - TECLE ENTER'.

PW9931*01  WRK-M62                         PIC  X(79) VALUE
PW9931*    'PROBLEMA NO ENVIO AO BACEN NO SISTEMA RCOR - TENTE NOVAMENTE
PW9931*    ''.

PW9931 01  WRK-M63                         PIC  X(79) VALUE
PW9931     'AGUARDANDO RETORNO BACEN - TECLE ENTER'.

PW9931 01  WRK-M64                         PIC  X(79) VALUE
PW9931     'INCONSISTENCIA NO RETORNO BACEN - CONSULTE ERRO NA OPC 27; C
PW9931-    'ORRECAO NA OPC 09'.

PW9931     'SITUACAO RECOR NAO PREVISTA - ENTRE EM CONTATO COM DDS'.

PW9931 01  WRK-M66                         PIC  X(79) VALUE
PW9931     'PROBLEMAS NO ENVIO RECOR/BACEN - UTILIZAR CONTINGENCIA'.

MF0113 01  WRK-M67                         PIC  X(79) VALUE
MF0113     'PROBLEMAS NA CEDULA SEM VALORES DE PRESTACAO - ENTRE COM COR
MF0113-    'RECAO NA OPCAO 09'.

MF0113 01  WRK-M68                         PIC  X(79) VALUE
MF0113     'PROBLEMAS NA CEDULA SEM VALORES DE LIBERACAO - ENTRE COM COR
MF0113-    'RECAO NA OPCAO 09'.

MF0113 01  WRK-M69                         PIC  X(79) VALUE
MF0113     'CEDULA C/ VL. DIVERG: LIBER X PGTO X FINANC. - ENTRE COM COR
MF0113-    'RECAO NA OPCAO 09'.

BS0250 01  WRK-M70                         PIC  X(79) VALUE
BS0250     'AGUARDANDO ENVIO/RETORNO DE FORNECEDORES AO SICOR'.

BS0281 01  WRK-M71                         PIC  X(79) VALUE

BS0281 01  WRK-M72                         PIC  X(79) VALUE
BS0281     'CEDULA NAO ENCONTRADA NO MODULO RUEC7799'.

BS0281 01  WRK-M73                         PIC  X(79) VALUE
BS0281     'CARTEIRA NAO PARAMETRIZADA NO RURC (RUEC7799)'.

BS0281 01  WRK-M74                         PIC  X(79) VALUE
BS0281     'TAXA NAO ENCONTRADA (RUEC7799)'.

EG1115 01  WRK-M75                         PIC  X(79) VALUE
EG1115     'ERRO. RENDA BRUTA ANUAL NAO ENCONTRADA NO CADASTRO DO CLIENT
EG1115-    'E'.
EG1115 01  WRK-M76                         PIC  X(79) VALUE
EG1115     'ERRO. A RENDA BRUTA ANUAL DECLARADA ESTA MENOR DO QUE A REND
EG1115-    'A AGROPECUARIA'.

BS0617 01  WRK-M77                         PIC  X(79) VALUE
BS0617     'CEDULA NAO POSSUI COORDENADAS GEODESICAS'.
BS0617
CET    01  WRK-M78                         PIC  X(79) VALUE
CET   -    'ACAO DA OPERACAO.'.

7C0325 01  WRK-M79                         PIC  X(79) VALUE
.          'DATA DE VALIDADE DA DAP INFERIOR à DATA CORRENTE, FAVOR ALTE
7C0325-    'RAR             .'.

7C1225 01  WRK-M80                         PIC  X(79) VALUE
7C1225     'CADASTRO DESATUALIZADO NO CADU (PSDC)'.

EG1215
EG1215*----------------------------------------------------------------*
EG1215 01  FILLER                      PIC  X(032)         VALUE
EG1215     '*** AREA DO MODULO RUEC7941  ***'.
EG1215*----------------------------------------------------------------*
EG1215
EG1215     COPY 'RUECWGEO'.
EG1215
      *    DEFINICAO DE ATRIBUTOS                                     *

       77  WRK-209                     PIC S9(08) COMP VALUE +209.
       77  WRK-225                     PIC S9(08) COMP VALUE +225.
       77  WRK-49353                   PIC S9(08) COMP VALUE +49353.
       77  WRK-49361                   PIC S9(08) COMP VALUE +49361.
       77  WRK-49369                   PIC S9(08) COMP VALUE +49369.

      *    DEFIN. DA AREA PARA RETURN-CODE DIFERENTE DE ZERO (POOL0660)*

       01  WRK-ERRO-POOL0660.
           03  FILLER                      PIC X(47) VALUE
              '*  RETURN-CODE DIFERENTE DE ZERO NA POOL0660 *'.
           03  FILLER                      PIC X(13) VALUE
              '*  RET.COD = '.
HEXA       03  WRK-RETURN-CODE             PIC 9(02)BB VALUE ZEROS.
           03  FILLER                      PIC X(08) VALUE
              'LOCAL = '.
           03  WRK-LOCAL-ERRO              PIC 9(02) VALUE ZEROS.

ACT046*----------------------------------------------------------------*
ACT046 77  FILLER PIC X(35) VALUE '* AREA DE COMUNICACAO COM O FORP  *'.
ACT046*----------------------------------------------------------------*

ACT046 COPY I#FORP14.
PW3932*----------------------------------------------------------------*
PW3932 77  FILLER PIC X(35) VALUE '* AREA COMUNICACAO RUEC7770/7799 *'.
PW3932*----------------------------------------------------------------*

PW3932 COPY I#RUEC48.

      *    DEFINICAO DA AREA PASSADA PELO PGM RUEC9160                *

       COPY I#RUEC19.

POL097*    DEFINICAO DA AREA PASSADA PELO PGM RUEC7720

POL097 COPY I#RUEC21.

POL097*  ***  AREA DE COMUNICACAO COM MODULO RUEC7728

POL097 COPY I#RUEC35.

PW9931*  ***  AREA DE COMUNICACAO COM MODULO RUEC7764

PW9931 COPY I#RUEC40.
PW9931*  ***  AREA DE COMUNICACAO COM MODULO RUEC8808

PW9931 COPY I#RUEC62.

PW9931*  ***  AREA DE COMUNICACAO COM MODULO RUEC8810

PW9931 COPY I#RUEC65.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** AREA DO MODULO RUEC8840 ***'.
      *---------------------------------------------------------------*

       01  WRK-MODULO-RUEC8840               PIC  X(08) VALUE
                                                             'RUEC8840'.
       01  WRK-LKG-AREA.
           05 WRK-ENTRADA.
              10 WRK-PGM-CHAMADOR            PIC  X(008) VALUE SPACES.
              10 WRK-AGENCIA                 PIC  9(005) VALUE ZEROS.
              10 WRK-CEDULA                  PIC  9(009) VALUE ZEROS.
              10 WRK-TIPO-PESSOA             PIC  X(001) VALUE SPACES.
              10 WRK-CGC                     REDEFINES WRK-IDENTIF.
                  15  WRK-NUMCGC             PIC  9(009).
                  15  WRK-FILIAL             PIC  9(004).
                  15  WRK-CTRL-CGC           PIC  9(002).
              10  WRK-CPF                    REDEFINES WRK-IDENTIF.
                  15  WRK-NUMCPF             PIC  9(009).
                  15  WRK-CTRL-CPF           PIC  9(002).
7C5.10            15  WRK-FILLER             PIC  X(004).
7C0625        10 WRK-NUM-ENVIO               PIC  9(001) VALUE ZEROS.
7C0625        10 FILLER                      PIC  X(061) VALUE SPACES.
           05 WRK-RETORNO.
              10 WRK-COD-RET                 PIC  9(002) VALUE ZEROS.
     **
     **=======  00 - OK
     **=======  01 - DADOS INCONSISTENTES
     **=======  99 - ERRO DB2
              10 WRK-MENSAGEM-R              PIC  X(079) VALUE SPACES.
              10 WRK-CONTROLE-BACEN          PIC  X(020) VALUE SPACES.
              10 FILLER                      PIC  X(030) VALUE SPACES.
              10 WRK-SQLCA                   PIC  X(136) VALUE SPACES.
              10 WRK-ERRO-AREA               PIC  X(107) VALUE SPACES.

PW6062*---------------------------------------------------------------*
PW6062***          AREA DE COMUNICACAO COM MOD. MIDD9000
PW6062*---------------------------------------------------------------*

PW6062 COPY I#MIDD01.

PW6062*---------------------------------------------------------------*
PW6062* AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
PW6062*---------------------------------------------------------------*
PW6062*
PW6062     COPY I#HEXA01.

PW2875*    DEFINICAO DA AREA PASSADA PELO MODULO RUEC7786

PW2875 COPY I#RUEC53.

PW2875*    DEFINICAO DA AREA PASSADA PELO MODULO RUEC7788

PW2875 COPY I#RUEC54.

       01  AREA-INPUT-T916.
HEXA       03  FILLER                      PIC  X(18) VALUE SPACES.
HEXA       03  INP-PFK-T916                PIC  X(01) VALUE SPACES.
HEXA       03  INP-SENHAS-T916             PIC  X(37) VALUE SPACES.
HEXA       03  INP-OPCAO-T916              PIC  9(02) VALUE ZEROS.
           03  INP-DADOS-TELA-T916.
               05  INP-AGENCIA-T916-X.
HEXA               07  INP-AGENCIA-T916    PIC  9(04) VALUE ZEROS.
               05  INP-CEDULA-T916-X.
HEXA               07  INP-CEDULA-T916     PIC  9(09) VALUE ZEROS.
HEXA           05  INP-IMP-CEDULA-T916     PIC  X(01) VALUE SPACES.
BR1217         05  INP-CTRBAC-T916         PIC  X(20) VALUE SPACES.
RE0517         05  INP-IMP-CGEODE-T916     PIC  X(01) VALUE SPACES.
HEXA           05  INP-IMP-PROPOSTA-T916   PIC  X(01) VALUE SPACES.
HEXA           05  INP-IMP-FICHA-T916      PIC  X(01) VALUE SPACES.
HEXA           05  INP-IMP-ANEXOS-T916     PIC  X(01) VALUE SPACES.
PW8123         05  INP-IMP-COMCZ-T916      PIC  X(01) VALUE  SPACES.
HEXA           05  INP-IMP-OPERAC-T916     PIC  X(01) VALUE SPACES.
CET            05  INP-FLG-CET-T916        PIC  X(01) VALUE SPACES.
CET            05  INP-IMP-CET-T916        PIC  X(01) VALUE SPACES.
HEXA           05  INP-IMP-TODOS-T916      PIC  X(01) VALUE SPACES.
HEXA           05  INP-CONF1-T916          PIC  X(21) VALUE SPACES.
HEXA           05  INP-REIMPR-T916         PIC  X(01) VALUE SPACES.
HEXA           05  INP-CONF2-T916          PIC  X(24) VALUE SPACES.

      *    TELA RUECT916 - OUTPUT                                     *

       01  AREA-OUTPUT-T916.
RE0517     03  OUT-LL-T916                 PIC S9(04) COMP VALUE +248.
HEXA       03  OUT-ZZ-T916                 PIC  9(04) COMP VALUE ZEROS.
HEXA       03  OUT-SENHA-T916              PIC  X(37) VALUE SPACES.
HEXA       03  OUT-NOPCAO-T916             PIC  X(02) VALUE SPACES.
           03  OUT-DATA-T916               PIC  X(10) VALUE SPACES.
           03  OUT-DADOS-T916.
HEXA           05  OUT-AGENCIA-ATTR-T916      PIC  9(04) COMP VALUE 0.
               05  OUT-AGENCIA-T916-X.
HEXA               07  OUT-AGENCIA-T916       PIC  9(04) VALUE ZEROS.
HEXA           05  OUT-CEDULA-ATTR-T916       PIC  9(04) COMP VALUE 0.
               05  OUT-CEDULA-T916-X.
HEXA               07  OUT-CEDULA-T916        PIC  9(09) VALUE ZEROS.
HEXA           05  OUT-IMP-CEDULA-ATTR-T916   PIC  9(04) COMP VALUE 0.
HEXA           05  OUT-IMP-CEDULA-T916        PIC  X(01) VALUE SPACES.
RE0517         05  OUT-IMP-CGEODE-ATTR-T916   PIC  9(04) COMP VALUE 0.
RE0517         05  OUT-IMP-CGEODE-T916        PIC  X(01) VALUE SPACES.
HEXA           05  OUT-IMP-PROPOSTA-ATTR-T916 PIC  9(04) COMP VALUE 0.
HEXA           05  OUT-IMP-PROPOSTA-T916      PIC  X(01) VALUE SPACES.
HEXA           05  OUT-IMP-FICHA-ATTR-T916    PIC  9(04) COMP VALUE 0.
HEXA           05  OUT-IMP-FICHA-T916         PIC  X(01) VALUE SPACES.
HEXA           05  OUT-IMP-ANEXOS-ATTR-T916   PIC  9(04) COMP VALUE 0.
HEXA           05  OUT-IMP-ANEXOS-T916        PIC  X(01) VALUE SPACES.
PW8123         05  OUT-IMP-COMCZ-ATTR-T916   PIC 9(04) COMP VALUE ZEROS.
PW8123         05  OUT-IMP-COMCZ-T916        PIC X(01)     VALUE SPACES.
HEXA           05  OUT-IMP-OPERAC-ATTR-T916   PIC  9(04) COMP VALUE 0.
HEXA           05  OUT-IMP-OPERAC-T916        PIC  X(01) VALUE SPACES.
CET            05  OUT-FLG-CET-T916           PIC  X(01) VALUE SPACES.
CET            05  OUT-IMP-CET-ATTR-T916      PIC  9(04) COMP VALUE 0.
CET            05  OUT-IMP-CET-T916           PIC  X(01) VALUE SPACES.
HEXA           05  OUT-IMP-TODOS-ATTR-T916    PIC  9(04) COMP VALUE 0.
HEXA           05  OUT-IMP-TODOS-T916         PIC  X(01) VALUE SPACES.
HEXA           05  OUT-IMP-4VIAS-ATTR-T916    PIC  9(04) COMP VALUE 0.
HEXA           05  OUT-IMP-4VIAS-T916         PIC  X(01) VALUE SPACES.
HEXA           05  OUT-CONF1-T916             PIC  X(21) VALUE SPACES.
HEXA           05  OUT-REIMPR-ATTR-T916       PIC  9(04) COMP VALUE 0.
HEXA           05  OUT-CONF2-T916             PIC  X(24) VALUE SPACES.
HEXA           05  OUT-MENSAG-T916            PIC  X(79) VALUE SPACES.

      /---------------------------------------------------------------*
      *  AREA DA POOL0660 COM MASCARA DA TELA RUECT916 - SEGMENTO 00  *
       01  660-RUECT916-SEG00.
RE0517     05  660-LL-AREA-T916           PIC  9(04)  COMP  VALUE 0046.
RE0517     05  660-LL-MENSAGEM-T916       PIC  9(04)  COMP  VALUE 0248.
           05  660-SENHAS-T916            PIC  9(04)  COMP  VALUE 0037.
           05  660-OPCAO-T916             PIC  9(04)  COMP  VALUE 1002.
           05  660-DATA-T916              PIC  9(04)  COMP  VALUE 0010.
           05  660-AGENCIA-T916           PIC  9(04)  COMP  VALUE 2006.
           05  660-CEDULA-T916            PIC  9(04)  COMP  VALUE 3011.
           05  660-IMP-CEDULA-T916        PIC  9(04)  COMP  VALUE 3003.
           05  660-CTRBAC-T916            PIC  9(04)  COMP  VALUE 2020.
RE0517     05  660-IMP-CGEODE-T916        PIC  9(04)  COMP  VALUE 3003.
           05  660-IMP-PROPOSTA-T916      PIC  9(04)  COMP  VALUE 3003.
           05  660-IMP-FICHA-T916         PIC  9(04)  COMP  VALUE 3003.
           05  660-IMP-ANEXOS-T916        PIC  9(04)  COMP  VALUE 3003.
PW8123     05  660-IMP-COMCZ-T916         PIC  9(04)  COMP  VALUE 3003.
           05  660-IMP-OPERAC-T916        PIC  9(04)  COMP  VALUE 3003.
CET        05  660-IMP-CET-T916           PIC  9(04)  COMP  VALUE 3003.
           05  660-IMP-TODOS-T916         PIC  9(04)  COMP  VALUE 3003.
           05  660-IMP-4VIAS-T916         PIC  9(04)  COMP  VALUE 3003.
           05  660-CONF1-T916             PIC  9(04)  COMP  VALUE 0021.
           05  660-REIMPR-T916            PIC  9(04)  COMP  VALUE 2003.
           05  660-CONF2-T916             PIC  9(04)  COMP  VALUE 0024.
           05  660-MENSAG-T916            PIC  9(04)  COMP  VALUE 0079.
      *    INC PARA ABENDAR PROGRAMA                                  *

        COPY POL7100C.

       COPY I#RUEC00.

BR0718*---------------------------------------------------------------*
BR0718 01  FILLER                      PIC X(32)           VALUE
BR0718     '*** BOOK I#RUEC58             **'.
BR0718*---------------------------------------------------------------*
BR0718
BR0718 COPY 'I#RUEC58'.
BR0718
BR0718 01  FILLER.
BR0718     03  WRK-RUEC8800            PIC X(10)       VALUE 'RUEC8800'.
BR0718
      *    INCLUDE DAS TABELAS DB2                                    *

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
                INCLUDE RUECV007
           END-EXEC.

MF0113     EXEC SQL
MF0113          INCLUDE RUECV010
MF0113     END-EXEC.

ACT046     EXEC SQL
ACT046     END-EXEC.

PW9931     EXEC SQL
PW9931          INCLUDE RUECV016
PW9931     END-EXEC.

           EXEC SQL
                INCLUDE RUECV019
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB033
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB034
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB042
           END-EXEC.
POL097     EXEC SQL
POL097          INCLUDE RUECB052
POL097     END-EXEC.

POL097     EXEC SQL
POL097          INCLUDE RUECB053
POL097     END-EXEC.

POL097     EXEC SQL
POL097          INCLUDE RUECB054
POL097     END-EXEC.

PW9931     EXEC SQL
PW9931          INCLUDE RUECB09B
PW9931     END-EXEC.

BR1217     EXEC SQL
BR1217          INCLUDE RUECB09C
BR1217     END-EXEC.

ACT046     EXEC SQL
ACT046     END-EXEC.

ACT046     EXEC SQL
ACT046          INCLUDE RURCV040
ACT046     END-EXEC.

ACT046     EXEC SQL
ACT046          INCLUDE RUCAB002
ACT046     END-EXEC.

BS0616*----------------------------------------------------------------*
BS0616 01  FILLER                      PIC  X(032)         VALUE
BS0616     '*** INCLUDE RURCB003         ***'.
BS0616*----------------------------------------------------------------*
BS0616
BS0616     EXEC SQL
BS0616       INCLUDE RURCB003
BS0616     END-EXEC.
BS0616
EG1215*----------------------------------------------------------------*
EG1215 01  FILLER                      PIC  X(032)         VALUE
EG1215*----------------------------------------------------------------*
EG1215
EG1215     EXEC SQL
EG1215       INCLUDE RUECB040
EG1215     END-EXEC.
EG1215
EG1215*----------------------------------------------------------------*
EG1215 01  FILLER                      PIC  X(032)         VALUE
EG1215     '*** INCLUDE RUECB09N         ***'.
EG1215*----------------------------------------------------------------*
EG1215
EG1215     EXEC SQL
EG1215       INCLUDE RUECB09N
EG1215     END-EXEC.
EG1215
EW0617*----------------------------------------------------------------*
EW0617 01  FILLER                      PIC  X(032)         VALUE
EW0617     '*** INCLUDE RUFIV002         ***'.
EW0617*----------------------------------------------------------------*
EW0617
EW0617     EXEC SQL
EW0617     END-EXEC.
EW0617
BS0617/---------------------------------------------------------------*
BS0617*           LAY-OUT DA TABELA DE INCONSISTENCIAS DO CONTRATO     *
BS0617*---------------------------------------------------------------*
BS0617
BS0617     EXEC  SQL
BS0617           INCLUDE RURCB099
BS0617     END-EXEC.
EG1215*----------------------------------------------------------------*
EG1215 01  FILLER                      PIC  X(032)         VALUE
EG1215     '*** CSR01-RUECB040           ***'.
EG1215*----------------------------------------------------------------*
EG1215
EG1215     EXEC SQL
EG1215       DECLARE CSR01-RUECB040 CURSOR WITH HOLD FOR
EG1215       SELECT
EG1215         CCONTR
EG1215       , CSEQ_MATR_RURAL
EG1215       , VALUE ( CINDCD_AREA_CIRC , 0 )
EG1215       FROM  DB2PRD.MATR_CONTR_RURAL
EG1215     END-EXEC.
EG1215
PW7398/===============================================================*
PW7398*    DEFINICAO DO CURSOR PARA ACESSO A TABELA RUECB034          *
PW7398*===============================================================*

PW7398     EXEC SQL
PW7398         DECLARE CSR01-RUECB034 CURSOR WITH HOLD FOR
PW7398          SELECT  CLIN_FIS_TEXTO,
PW7398                  RLIN_MAX,
PW7398                  CPAG
PW7398          FROM    DB2PRD.HIST_IMPRE_CDULA
PW7398          WHERE   CJUNC_DEPDC   = :RUECB034.CJUNC-DEPDC
PW7398            AND   CCDULA        = :RUECB034.CCDULA
PW7398            AND   CLIN_FIS_TEXTO > 0
PW7398     END-EXEC.

POL097/===============================================================*
POL097*    DEFINICAO DO CURSOR PARA ACESSO A TABELA RUECB052          *
POL097*===============================================================*

POL097         DECLARE CSR01-RUECB052 CURSOR WITH HOLD FOR
POL097          SELECT  CCONTR,
POL097                  CSEQ_HIST_IMOV
POL097          FROM    DB2PRD.THIST_IMOV_RURAL
POL097          WHERE   CCONTR        = :RUECB052.CCONTR
POL097     END-EXEC.

POL097/===============================================================*
POL097*    DEFINICAO DO CURSOR PARA ACESSO A TABELA RUECB053          *
POL097*===============================================================*

POL097     EXEC SQL
POL097         DECLARE CSR01-RUECB053 CURSOR WITH HOLD FOR
POL097          SELECT  CCONTR,
POL097                  CPRODT_RURAL,
POL097                  CSEQ_HIST_RBNHO
POL097          FROM    DB2PRD.THIST_RBNHO_CONTR
POL097          WHERE   CCONTR        = :RUECB053.CCONTR
POL097     END-EXEC.

POL097/===============================================================*
POL097*===============================================================*

POL097     EXEC SQL
POL097         DECLARE CSR01-RUECB054 CURSOR WITH HOLD FOR
POL097          SELECT  CCONTR,
POL097                  CPRODT_RURAL
POL097          FROM    DB2PRD.THIST_RENDA_AGROP
POL097          WHERE   CCONTR        = :RUECB054.CCONTR
POL097     END-EXEC.

BS0617/===============================================================*
BS0617*    DEFINICAO DO CURSOR PARA ACESSO A TABELA RUFIB002/RUECV014 *
BS0617*===============================================================*
BS0617
           EXEC SQL
               DECLARE CSR01-RUFIB002-JN CURSOR WITH HOLD FOR
                 SELECT A.CCONTR
                 ,      A.CSEQ_PSSOA
                 ,      B.CORIGE_REC
                 ,      B.CFNALD_RURAL
                 ,      B.CATVDD_RURAL
                 ,      B.VFINAN
                 ,      B.QPROTV_ADQUI
                 ,      B.CCDULA
                 ,      B.CREFT_BACEN
                 ,      B.CUND_MEDD_FINCD
                 ,      B.CJUNC_DEPDC
                 ,      B.CCONTR
                 ,      B.DIMPRE_CONTR_RURAL
                 FROM   DB2PRD.PESSOA_CONTR_RURAL A
                 ,      DB2PRD.CONTRATO_RURAL     B
                 ,      DB2PRD.RUCA_CAD_ROOT      C
                 WHERE  A.CCGC_CPF        = :RUECV014.CCGC-CPF
                 AND    A.CFLIAL_CGC      = :RUECV014.CFLIAL-CGC
                 AND    A.CCTRL_CPF_CGC   = :RUECV014.CCTRL-CPF-CGC
                 AND    A.CCONTR          = B.CCONTR
                 AND    C.B003_CH_CONTR   = B.CCONTR
                 AND    A.CTPO_RESP_PSSOA =  'T'
                 AND  ( B.DVCTO_CONTR    <= CURRENT DATE
                 OR     SUBSTR ( HEX ( B003_CONTROL ) , 1 , 1 )
                                         IN ( '8' , '9', 'A' , 'B'
                                            , 'C' , 'D', 'E' , 'F' ))
                  ,        A.CSEQ_PSSOA
POL097     END-EXEC.
BS0617*
       LINKAGE SECTION.

       01  IO-PCB.
           02  IO-LTERM                    PIC X(08).
           02  FILLER                      PIC X(02).
           02  IO-STA                      PIC X(02).
           02  FILLER                      PIC X(12).
           02  IO-MODNAME                  PIC X(08).

       01  ALT-PCB.
           02  ALT-TERM                    PIC X(08).
           02  FILLER                      PIC X(02).
           02  ALT-STA                     PIC X(02).

       PROCEDURE DIVISION.

           ENTRY  'DLITCBL'             USING    IO-PCB
                                                 ALT-PCB.
PW6062*---------------------------------------------------------------*
PW6062* AREA DE MOVIMENTACAO DA ROTINA MESU                           *
PW6062*---------------------------------------------------------------*
PW6062*
PW6062     MOVE   'MESU9410'               TO    WRK-MODULO-MESUX0.

           MOVE    WRK-GU                  TO    WRK-FUNCAO.

       000-00-PROCESSA-TRANSACAO SECTION.

           CALL   'POOL5000'            USING    WRK-FUNCAO
                                                 IO-PCB
                                                 ALT-PCB
                                                 WRK-MENSAGEM
                                                 WRK-TELA
                                                 WRK-OPCAO
                                                 WRK-COD-USER
                                                 WRK-OPCAO-ATUAL
                                                 WRK-COD-DEPTO.

           IF RETURN-CODE EQUAL 04 OR 12

           PERFORM 010-00-ROTINA-PRINCIPAL

           GO TO 000-00-PROCESSA-TRANSACAO.

       000-99-PROCESSA-TRANSACAO. EXIT.

       010-00-ROTINA-PRINCIPAL SECTION.

           MOVE    SPACES                  TO    OUT-DADOS-T916

           CALL   'POOL7600'            USING    DATA-HORA.

           MOVE    DT-AAAAMMDD             TO    WRK-DATA-AUX-R.
7C0325     MOVE    WRK-DATA-AUX-R          TO    WRK-DATA-DIA.
           MOVE    CORR WRK-DATA-AUX       TO    WRK-DATA-CERTA.
           MOVE    WRK-DATA-CERTA          TO    OUT-DATA-T916.

           MOVE  'RUECT916'  TO WRK-TELA.

           IF WRK-TRANSACAO NOT EQUAL 'RUEC9160'
           ELSE
              PERFORM 030-00-PROCESSA-RUECT916.

           IF WRK-TELA EQUAL 'RUECT916'
              PERFORM 040-00-POOL0660.

       010-99-ROTINA-PRINCIPAL. EXIT.

       020-00-RECEBE-CONTROLE SECTION.

           MOVE WRK-MENSAGEM TO COMU-AREA-IMPRESSAO
           MOVE COMU-SENHAS  TO INP-SENHAS-T916

           PERFORM   245-00-DELETE-RUECB042

           IF WRK-TRANSACAO  EQUAL 'RUEC8600'
              MOVE  COMU-AGENCIA  TO  OUT-AGENCIA-T916
              MOVE  COMU-CEDULA   TO  OUT-CEDULA-T916
ACT211        EVALUATE COMU-COD-RETORNO
EG0417            WHEN LOW-VALUES
                  WHEN ZEROS
POL097                 PERFORM 470-GERA-HISTORICO
                  WHEN '2'
                       MOVE  WRK-M27 TO OUT-MENSAG-T916
                  WHEN '3'
                       MOVE  WRK-M30 TO OUT-MENSAG-T916
                  WHEN '4'
                       MOVE  WRK-M31 TO OUT-MENSAG-T916
                  WHEN '5'
                       MOVE  WRK-M32 TO OUT-MENSAG-T916
                  WHEN '6'
                       MOVE  WRK-M33 TO OUT-MENSAG-T916
                  WHEN '7'
                       MOVE  WRK-M34 TO OUT-MENSAG-T916
                  WHEN '8'
                       MOVE  WRK-M35 TO OUT-MENSAG-T916
                  WHEN '9'
                       MOVE  WRK-M36 TO OUT-MENSAG-T916
                  WHEN 'A'
                       MOVE  WRK-M11 TO OUT-MENSAG-T916
PW7398            WHEN 'B'
PW7398                 MOVE SPACES         TO WRK-CODIFIC
PW7398                 MOVE COMU-CEDULA    TO WRK-CCDULA
PW7398                 MOVE 'FR'           TO WRK-SIT-IMPRE
PW7398                 PERFORM 2040-INSERIR-RUCAB002
POL097                 PERFORM 470-GERA-HISTORICO
POL097                 PERFORM 2090-INSERIR-RUECB033
PW7398                 MOVE  WRK-M42 TO OUT-MENSAG-T916
BS0617            WHEN 'C'
BS0617                 MOVE  WRK-M77 TO OUT-MENSAG-T916
CET               WHEN 'T'
CET                    MOVE  WRK-M10 TO OUT-MENSAG-T916
CET                    MOVE 'X'      TO INP-FLG-CET-T916
CET                                     OUT-FLG-CET-T916
                  WHEN OTHER
                       MOVE  WRK-M12 TO OUT-MENSAG-T916
ACT211        END-EVALUATE
           ELSE
              MOVE      WRK-M2  TO OUT-MENSAG-T916
           END-IF.

       020-99-RECEBE-CONTROLE. EXIT.


           PERFORM 090-00-MANTEM-TELA.

           IF INP-PFK-T916 EQUAL '*'
              MOVE    WRK-M1            TO    OUT-MENSAG-T916
              IF WRK-COD-DEPTO-N EQUAL 4253 OR 4130 OR 4510
                 MOVE WRK-49345 TO OUT-IMP-PROPOSTA-ATTR-T916
                 IF INP-CONF1-T916 NOT  EQUAL SPACES AND LOW-VALUES
                    MOVE    WRK-49345   TO    OUT-REIMPR-ATTR-T916
                    MOVE    WRK-225     TO OUT-IMP-CEDULA-ATTR-T916
RE0517                                     OUT-IMP-CGEODE-ATTR-T916
                                           OUT-IMP-PROPOSTA-ATTR-T916
                                           OUT-IMP-FICHA-ATTR-T916
                                           OUT-IMP-ANEXOS-ATTR-T916
PW8123                                     OUT-IMP-COMCZ-ATTR-T916
                                           OUT-IMP-OPERAC-ATTR-T916
CET                                        OUT-IMP-CET-ATTR-T916
                                           OUT-IMP-TODOS-ATTR-T916
                                           OUT-IMP-4VIAS-ATTR-T916
                 ELSE
                    NEXT  SENTENCE
                 MOVE    WRK-209        TO    OUT-AGENCIA-ATTR-T916
                 IF INP-CONF1-T916 NOT  EQUAL SPACES AND LOW-VALUES
                    MOVE    WRK-49345   TO    OUT-REIMPR-ATTR-T916
                    MOVE    WRK-225     TO OUT-IMP-CEDULA-ATTR-T916
RE0517                                     OUT-IMP-CGEODE-ATTR-T916
                                           OUT-IMP-PROPOSTA-ATTR-T916
                                           OUT-IMP-FICHA-ATTR-T916
                                           OUT-IMP-ANEXOS-ATTR-T916
PW8123                                     OUT-IMP-COMCZ-ATTR-T916
                                           OUT-IMP-OPERAC-ATTR-T916
CET                                        OUT-IMP-CET-ATTR-T916
                                           OUT-IMP-TODOS-ATTR-T916
                                           OUT-IMP-4VIAS-ATTR-T916
                 ELSE
                    NEXT  SENTENCE
           ELSE
              PERFORM 100-00-CONSISTENCIA.

       030-99-PROCESSA-RUECT916. EXIT.

       040-00-POOL0660 SECTION.
           CALL   'POOL0660'            USING    AREA-OUTPUT-T916
                                                 660-RUECT916-SEG00.

           IF RETURN-CODE NOT EQUAL ZEROS
              MOVE   'APL'                 TO    ERR-TIPO-ACESSO
              MOVE    RETURN-CODE          TO    WRK-RETURN-CODE
              MOVE    1                    TO    WRK-LOCAL-ERRO
              MOVE    WRK-ERRO-POOL0660    TO    ERR-TEXTO
              PERFORM 9000-ROTINA-ERRO.

           MOVE    WRK-ISRT                TO    WRK-FUNCAO
           MOVE    AREA-OUTPUT-T916        TO    WRK-MENSAGEM.

       040-99-POOL0660. EXIT.

       050-00-ACESSA-RUECB042 SECTION.

           MOVE    IO-LTERM                TO    CTERM OF RUECB042

           EXEC SQL
                SELECT
                INTO
                      :RUECB042.WNAVG-CREDT-RURAL
                FROM
                       DB2PRD.NAVEGACAO_RURAL
                WHERE
                       CTERM = :RUECB042.CTERM
           END-EXEC.

           IF SQLCODE NOT EQUAL ZEROS AND +100 OR
              SQLWARN0    EQUAL 'W'
              MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
              MOVE   'NAVEGACAO_RURAL   '  TO    ERR-DBD-TAB
              MOVE   'SELECT'              TO    ERR-FUN-COMANDO
              MOVE    SQLCODE              TO    ERR-SQL-CODE
              MOVE    0001                 TO    ERR-LOCAL
              MOVE    SPACES               TO    ERR-SEGM
              PERFORM 9000-ROTINA-ERRO
           ELSE
              MOVE    WNAVG-CREDT-RURAL-TEXT      TO  NAVEG-RURAL
              MOVE    AGENCIA OF NAVEGACAO-RURAL  TO  INP-AGENCIA-T916
                                                      OUT-AGENCIA-T916
                                                      OUT-CEDULA-T916.

       050-99-ACESSA-RUECB042. EXIT.

       060-00-ACESSA-RUFIB002 SECTION.

           MOVE    INP-AGENCIA-T916        TO    CJUNC-DEPDC OF RUFIB002
           MOVE    INP-CEDULA-T916         TO    CCDULA      OF RUFIB002

           EXEC SQL
                SELECT CCONTR,
                       CSTTUS_CONTR_RURAL
                INTO  :RUFIB002.CCONTR,
                      :RUFIB002.CSTTUS-CONTR-RURAL
                FROM   DB2PRD.CONTRATO_RURAL
                WHERE (CJUNC_DEPDC = :RUFIB002.CJUNC-DEPDC  AND
                       CCDULA      = :RUFIB002.CCDULA)      AND
                       CORIGE_REC <> 81
           END-EXEC.

      *======> PARA EVITAR CEDULAS BATCH E CEDULAS COM 7 DIGITOS
           IF SQLCODE     EQUAL ZEROS
              IF  CCDULA OF RUFIB002  NOT LESS 199900000 AND
                  CSTTUS-CONTR-RURAL OF RUFIB002 NOT EQUAL ZEROS
                  NEXT SENTENCE
              ELSE
                  MOVE    WRK-M7    TO    OUT-MENSAG-T916
                  GO TO 060-99-ACESSA-RUFIB002.


           IF SQLCODE NOT EQUAL ZEROS AND +100 OR
              SQLWARN0    EQUAL 'W'
              MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
              MOVE   'CONTRATO_RURAL '     TO    ERR-DBD-TAB
              MOVE   'SELECT'              TO    ERR-FUN-COMANDO
              MOVE    SQLCODE              TO    ERR-SQL-CODE
              MOVE    0002                 TO    ERR-LOCAL
              MOVE    SPACES               TO    ERR-SEGM
              PERFORM 9000-ROTINA-ERRO
           ELSE
           IF SQLCODE EQUAL ZEROS
              MOVE    WRK-M2               TO    OUT-MENSAG-T916
              MOVE    WRK-M7               TO    OUT-MENSAG-T916.

       060-99-ACESSA-RUFIB002. EXIT.

POL097 065-00-VERIFICA-RUEC7720   SECTION.

POL097     INITIALIZE  R21-AREA-R7720.
           PERFORM 160-00-ACESSA-RUECV007

7C0325     IF CDECLR-AGCLT-FAMLR OF RUECV007    NOT EQUAL SPACES
.              MOVE  DVALDD-DECLR-FAMLR   OF RUECV007
.                                         TO  WRK-DATAS-C
.              MOVE  WRK-DD-C             TO  WRK-DD-D
.              MOVE  WRK-MM-C             TO  WRK-MM-D
.              MOVE  WRK-AA-C             TO  WRK-AA-D
.              IF WRK-DATAS-D-R  LESS WRK-DATA-DIA
.                 MOVE 'N'    TO WRK-DTDAP-OK
.                 GO  TO  065-99-VERIFICA-RUEC7720
.              END-IF
7C0325     END-IF
POL097     IF  (CAPLIC-RURAL OF RUECV007 NOT EQUAL 1 AND 2)  OR
MF0112         (CFNALD-RURAL OF RUECV007 EQUAL    57 OR 59)
POL097          GO  TO  065-99-VERIFICA-RUEC7720
POL097     END-IF

BR0718     PERFORM 7000-CHECAR-EXIGE-IMOVEL
BR0718
BR0718     IF  WRK-EXIGE-IMOVEL        EQUAL 'N'
POL097         GO  TO  065-99-VERIFICA-RUEC7720
BR0718     END-IF
BR0718
POL097     MOVE  'RUEC9160'           TO  R21-PGMCHMD-R7720.
POL097     MOVE  'C'                  TO  R21-FUNCAO-R7720.
POL097     MOVE  CCLI-UNIC-BDSCO      TO  R21-CCLUB-R7720.
POL097     MOVE  DULT-VALDC-CDULA     TO  R21-DCEDULA-R7720.
POL097     MOVE  CINDCD-FONE-RESID OF RUECV007 TO
POL097                                    R21-CIND-TRES-ENV-R7720
POL097     MOVE  CINDCD-FONE-COML  OF RUECV007 TO
POL097                                    R21-CIND-TCOM-ENV-R7720
POL097     MOVE  CINDCD-ENDER-RESID OF RUECV007 TO
POL097                                    R21-CIND-ERES-ENV-R7720
POL097     MOVE  CINDCD-ENDER-COML OF RUECV007 TO
POL097     MOVE  CINDCD-QUALF-PROFS OF RUECV007 TO
POL097                                    R21-CIND-QPFS-ENV-R7720
POL097     MOVE  QBLOCO-PATRM-IMOV OF RUECV007 TO
POL097                                    R21-QPATR-IMOV-ENV-R7720
POL097     MOVE  QBLOCO-PATRM-VEIC OF RUECV007 TO
POL097                                    R21-QPATR-VEIC-ENV-R7720
POL097     MOVE  QBLOCO-LSNG-CLI   OF RUECV007 TO
POL097                                    R21-QLSNG-ENV-R7720
POL097     MOVE  QBLOCO-SEGUR-CLI  OF RUECV007 TO
POL097                                    R21-QSEGUR-ENV-R7720
POL097     MOVE  QBLOCO-QUADR-SCIAL OF RUECV007 TO
POL097                                    R21-QQDR-SCIAL-ENV-R7720

POL097     CALL  'POOL0081'   USING   WRK-RUEC7720
POL097                                R21-AREA-R7720
POL097                                IO-PCB
POL097                                ALT-PCB

POL097     IF  R21-RET-CODE-R7720   EQUAL  99
POL097         MOVE  R21-SQLCA-R7720         TO  SQLCA
POL097         MOVE  R21-ERRO-AREA-R7720     TO  ERRO-AREA

POL097     IF  R21-RET-CODE-R7720   EQUAL 01
POL097         MOVE  WRK-M48              TO     OUT-MENSAG-T916
POL097     ELSE
POL097     IF  R21-RET-CODE-R7720   EQUAL 02
POL097         MOVE  WRK-M49              TO     OUT-MENSAG-T916.

POL097 065-99-VERIFICA-RUEC7720. EXIT.

       090-00-MANTEM-TELA SECTION.

           MOVE    WRK-MENSAGEM            TO    AREA-INPUT-T916
           MOVE   'RUECT916'               TO    WRK-TELA

           MOVE    INP-IMP-CEDULA-T916     TO    OUT-IMP-CEDULA-T916
RE0517     MOVE    INP-IMP-CGEODE-T916     TO    OUT-IMP-CGEODE-T916
           MOVE    INP-IMP-PROPOSTA-T916   TO    OUT-IMP-PROPOSTA-T916
           MOVE    INP-IMP-ANEXOS-T916     TO    OUT-IMP-ANEXOS-T916
PW8123     MOVE    INP-IMP-COMCZ-T916      TO    OUT-IMP-COMCZ-T916
           MOVE    INP-IMP-OPERAC-T916     TO    OUT-IMP-OPERAC-T916
           MOVE    INP-IMP-FICHA-T916      TO    OUT-IMP-FICHA-T916
CET        MOVE    INP-IMP-CET-T916        TO    OUT-IMP-CET-T916
           MOVE    INP-IMP-TODOS-T916      TO    OUT-IMP-TODOS-T916
           MOVE    INP-IMP-4VIAS-T916      TO    OUT-IMP-4VIAS-T916
           MOVE    INP-CONF1-T916          TO    OUT-CONF1-T916
           MOVE    INP-CONF2-T916          TO    OUT-CONF2-T916
           MOVE    INP-REIMPR-T916         TO    OUT-REIMPR-T916
           MOVE    INP-CTRBAC-T916         TO    OUT-CTRBAC-T916

           MOVE    INP-AGENCIA-T916-X      TO    OUT-AGENCIA-T916-X.
           IF INP-AGENCIA-T916-X NOT EQUAL SPACES
              INSPECT INP-AGENCIA-T916-X REPLACING LEADING ' ' BY '0'.

           MOVE    INP-CEDULA-T916-X       TO    OUT-CEDULA-T916-X.
           IF INP-CEDULA-T916-X NOT EQUAL SPACES
              INSPECT INP-CEDULA-T916-X REPLACING LEADING ' ' BY '0'.

       090-99-FIM. EXIT.

       100-00-CONSISTENCIA SECTION.

           IF INP-AGENCIA-T916-X EQUAL SPACES AND
              MOVE    WRK-49369            TO    OUT-AGENCIA-ATTR-T916
                                                 OUT-CEDULA-ATTR-T916
              MOVE    WRK-M8               TO    OUT-MENSAG-T916
              GO  TO  100-99-CONSISTENCIA.

           IF INP-AGENCIA-T916-X IS NOT NUMERIC OR
              INP-AGENCIA-T916      NOT GREATER ZEROS
              MOVE    WRK-49369            TO    OUT-AGENCIA-ATTR-T916
              MOVE    WRK-M9               TO    OUT-MENSAG-T916
              GO  TO  100-99-CONSISTENCIA.

           IF INP-CEDULA-T916-X  IS NOT NUMERIC OR
              INP-CEDULA-T916       NOT GREATER ZEROS
              MOVE    WRK-49369            TO    OUT-CEDULA-ATTR-T916
              MOVE    WRK-M9               TO    OUT-MENSAG-T916
              GO  TO  100-99-CONSISTENCIA.

PW6062     IF  INP-AGENCIA-T916      NUMERIC
PW6062         MOVE WRK-COD-USER           TO    WRK-COD-USER-R
PW6062         IF  WRK-COD-DEPTO-N NOT EQUAL 4510 AND 4130 AND 4253
PW6062             MOVE SPACES             TO    WRK-PERM-OK
PW6062             IF  WRK-PERM-OK  NOT EQUAL 'S'
PW6062                 MOVE  WRK-49369     TO    OUT-AGENCIA-ATTR-T916
PW6062                 MOVE  LNK-MENSAGEM  TO    OUT-MENSAG-T916
PW6062                 GO  TO  100-99-CONSISTENCIA
PW6062             END-IF
PW6062         END-IF
PW6062     END-IF.

MMF        PERFORM 160-00-ACESSA-RUECV007.

MF0611     IF  SQLCODE  EQUAL  +100
MF0611         MOVE    WRK-49369      TO    OUT-AGENCIA-ATTR-T916
MF0611         MOVE    WRK-49369      TO    OUT-CEDULA-ATTR-T916
MF0611         MOVE    WRK-M7         TO    OUT-MENSAG-T916
MF0611         GO  TO  100-99-CONSISTENCIA
MF0611     END-IF.

           EXEC SQL
                SELECT CCONTR,
                       CJUNC_DEPDC,
                       CCDULA,
                INTO  :RUFIB002.CCONTR,
                      :RUFIB002.CJUNC-DEPDC,
                      :RUFIB002.CCDULA,
                      :RUFIB002.CSTTUS-CONTR-RURAL
                FROM   DB2PRD.CONTRATO_RURAL
MMF             WHERE  CCONTR       = :RUECV007.CCONTR       AND
MMF                     CORIGE_REC <> 81
           END-EXEC.

           IF SQLCODE NOT EQUAL ZEROS AND +100 OR
              SQLWARN0    EQUAL 'W'
              MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
              MOVE   'CONTRATO_RURAL '     TO    ERR-DBD-TAB
              MOVE   'SELECT'              TO    ERR-FUN-COMANDO
              MOVE    SQLCODE              TO    ERR-SQL-CODE
              MOVE    0004                 TO    ERR-LOCAL
              MOVE    SPACES               TO    ERR-SEGM
              PERFORM 9000-ROTINA-ERRO.

      *======> PARA EVITAR CEDULAS BATCH E CEDULAS COM 7 DIGITOS

              IF CCDULA OF RUFIB002  NOT  LESS  199900000  AND
                 CSTTUS-CONTR-RURAL OF RUFIB002 NOT = 0
                 NEXT SENTENCE
              ELSE
                 MOVE    WRK-49369      TO    OUT-AGENCIA-ATTR-T916
                 MOVE    WRK-49369      TO    OUT-CEDULA-ATTR-T916
                 MOVE    WRK-M7         TO    OUT-MENSAG-T916
                 GO  TO  100-99-CONSISTENCIA
           ELSE
                 MOVE    WRK-49369      TO    OUT-AGENCIA-ATTR-T916
                 MOVE    WRK-49369      TO    OUT-CEDULA-ATTR-T916
                 MOVE    WRK-M7         TO    OUT-MENSAG-T916
                 GO  TO  100-99-CONSISTENCIA.

POL097     IF  INP-IMP-4VIAS-T916 NOT EQUAL 'X'
POL097         IF  CCLI-UNIC-BDSCO           EQUAL ZEROS
POL097             MOVE    WRK-M50           TO    OUT-MENSAG-T916
POL097             GO  TO  100-99-CONSISTENCIA
POL097         END-IF
POL097     END-IF.

MF0113     IF  WRK-TOT-PARC  EQUAL  ZEROS
MF0113         MOVE    WRK-M67           TO    OUT-MENSAG-T916
MF0113         GO  TO  100-99-CONSISTENCIA
MF0113     END-IF.

MF0113     PERFORM  4910-SOMA-LIBERACAO
MF0113     IF  WRK-TOT-LIBER EQUAL  ZEROS
MF0113         MOVE    WRK-M68           TO    OUT-MENSAG-T916
MF0113         GO  TO  100-99-CONSISTENCIA
MF0113     END-IF.

MF0113     IF  WRK-TOT-LIBER NOT EQUAL WRK-TOT-PARC
MF0113         MOVE    WRK-M69           TO    OUT-MENSAG-T916
MF0113         GO  TO  100-99-CONSISTENCIA
MF0113     END-IF.

MF0113     IF  WRK-TOT-LIBER NOT EQUAL VFINAN OF RUECV007
MF0113         MOVE    WRK-M69           TO    OUT-MENSAG-T916
MF0113         GO  TO  100-99-CONSISTENCIA
MF0113     END-IF.

RE0517        INP-IMP-CGEODE-T916   EQUAL SPACES AND
              INP-IMP-PROPOSTA-T916 EQUAL SPACES AND
              INP-IMP-FICHA-T916    EQUAL SPACES AND
CET           INP-IMP-CET-T916      EQUAL SPACES AND
              INP-IMP-TODOS-T916    EQUAL SPACES AND
              INP-IMP-ANEXOS-T916   EQUAL SPACES AND
PW8123        INP-IMP-COMCZ-T916    EQUAL SPACES AND
              INP-IMP-OPERAC-T916   EQUAL SPACES AND
              INP-IMP-4VIAS-T916    EQUAL SPACES
              MOVE    WRK-M2        TO    OUT-MENSAG-T916
              MOVE WRK-49345 TO OUT-IMP-CEDULA-ATTR-T916
           ELSE
            IF INP-IMP-CEDULA-T916 NOT EQUAL SPACES AND 'X'
              MOVE WRK-49353 TO OUT-IMP-CEDULA-ATTR-T916
              MOVE    WRK-M4               TO    OUT-MENSAG-T916
            ELSE
RE0517       IF INP-IMP-CGEODE-T916 NOT EQUAL SPACES AND 'X'
RE0517        MOVE WRK-49353 TO OUT-IMP-CGEODE-ATTR-T916
              MOVE    WRK-M4               TO    OUT-MENSAG-T916
             ELSE
              IF INP-IMP-PROPOSTA-T916 NOT EQUAL SPACES AND 'X'
               MOVE    WRK-M4               TO    OUT-MENSAG-T916
              ELSE
               IF INP-IMP-FICHA-T916 NOT EQUAL SPACES AND 'X'
                MOVE WRK-49353  TO OUT-IMP-FICHA-ATTR-T916
                MOVE    WRK-M4  TO OUT-MENSAG-T916
               ELSE
                IF INP-IMP-ANEXOS-T916 NOT EQUAL SPACES AND 'X'
                 MOVE WRK-49353    TO OUT-IMP-ANEXOS-ATTR-T916
                 MOVE    WRK-M4    TO OUT-MENSAG-T916
                ELSE
                 IF INP-IMP-OPERAC-T916 NOT EQUAL SPACES AND 'X'
                  MOVE WRK-49353 TO OUT-IMP-OPERAC-ATTR-T916
                  MOVE    WRK-M4               TO    OUT-MENSAG-T916
                 ELSE
CET               IF INP-IMP-CET-T916 NOT EQUAL SPACES AND 'X'
CET                MOVE WRK-49353 TO OUT-IMP-CET-ATTR-T916
CET                MOVE    WRK-M4               TO    OUT-MENSAG-T916
                  ELSE
                  IF INP-IMP-TODOS-T916 NOT EQUAL SPACES AND 'X'
                   MOVE WRK-49353 TO OUT-IMP-TODOS-ATTR-T916
                   MOVE    WRK-M4               TO    OUT-MENSAG-T916
                   IF INP-IMP-4VIAS-T916 NOT EQUAL SPACES AND 'X'
                    MOVE WRK-49353 TO OUT-IMP-4VIAS-ATTR-T916
                    MOVE    WRK-M4               TO    OUT-MENSAG-T916
                   ELSE
                    IF (INP-IMP-4VIAS-T916    EQUAL 'X') AND
                       (INP-IMP-CEDULA-T916   EQUAL 'X'  OR
RE0517                  INP-IMP-CGEODE-T916   EQUAL 'X'  OR
                        INP-IMP-PROPOSTA-T916 EQUAL 'X'  OR
                        INP-IMP-FICHA-T916    EQUAL 'X'  OR
                        INP-IMP-ANEXOS-T916   EQUAL 'X'  OR
PW8123                  INP-IMP-COMCZ-T916    EQUAL 'X'  OR
                        INP-IMP-OPERAC-T916   EQUAL 'X'  OR
CET                     INP-IMP-CET-T916      EQUAL 'X'  OR
                        INP-IMP-TODOS-T916    EQUAL 'X')
                     MOVE   WRK-49353      TO    OUT-IMP-4VIAS-ATTR-T916
                     MOVE    WRK-M6         TO    OUT-MENSAG-T916
                    ELSE
                     IF (INP-IMP-TODOS-T916    EQUAL 'X') AND
                        (INP-IMP-CEDULA-T916   EQUAL 'X'  OR
RE0517                   INP-IMP-CGEODE-T916   EQUAL 'X'  OR
                         INP-IMP-PROPOSTA-T916 EQUAL 'X'  OR
                         INP-IMP-ANEXOS-T916   EQUAL 'X'  OR
PW8123                   INP-IMP-COMCZ-T916    EQUAL 'X'  OR
                         INP-IMP-OPERAC-T916   EQUAL 'X'  OR
CET                      INP-IMP-CET-T916      EQUAL 'X')
                      MOVE  WRK-49353      TO    OUT-IMP-TODOS-ATTR-T916
                      MOVE    WRK-M5       TO    OUT-MENSAG-T916
                     ELSE
                      IF (INP-CONF1-T916  NOT EQUAL SPACES
                                                         AND LOW-VALUES)
                       IF  (INP-REIMPR-T916 NOT EQUAL 'X' AND ' ')
                        MOVE    WRK-49353 TO OUT-REIMPR-ATTR-T916
                        MOVE    WRK-M4    TO OUT-MENSAG-T916
                        MOVE    WRK-225   TO OUT-IMP-CEDULA-ATTR-T916
RE0517                                       OUT-IMP-CGEODE-ATTR-T916
                                             OUT-IMP-PROPOSTA-ATTR-T916
                                             OUT-IMP-FICHA-ATTR-T916
                                             OUT-IMP-ANEXOS-ATTR-T916
PW8123                                       OUT-IMP-COMCZ-ATTR-T916
                                             OUT-IMP-OPERAC-ATTR-T916
CET                                          OUT-IMP-CET-ATTR-T916
                                             OUT-IMP-TODOS-ATTR-T916
                       ELSE
                        PERFORM 110-00-TRATA-IMPRESSAO
                       END-IF
                      ELSE
PW8123                 IF INP-IMP-COMCZ-T916 NOT EQUAL SPACES AND 'X'
PW8123                  MOVE WRK-49353 TO OUT-IMP-COMCZ-ATTR-T916
PW8123                  MOVE WRK-M4    TO OUT-MENSAG-T916
PW8123                 ELSE
                        PERFORM 110-00-TRATA-IMPRESSAO
                       END-IF
                      END-IF
                      END-IF
                     END-IF
                    END-IF
                   END-IF
                  END-IF
                 END-IF
                END-IF
               END-IF
              END-IF
             END-IF
           END-IF

PW9931     IF  INP-IMP-4VIAS-T916     EQUAL 'R' OR 'B'
PW7398         IF  WRK-COD-DEPTO-N EQUAL 4253 OR 4130 OR 4510
PW7398             MOVE    SPACES         TO    OUT-MENSAG-T916
PW7398             PERFORM 110-00-TRATA-IMPRESSAO
PW7398         ELSE
PW7398             MOVE    WRK-49353      TO    OUT-IMP-4VIAS-ATTR-T916
PW7398             MOVE    WRK-M41        TO    OUT-MENSAG-T916
PW7398         END-IF
PW7398     END-IF.

       100-99-CONSISTENCIA. EXIT.

      *---------------------------------------------------------------*
PW6062 1001-ACESSAR-MIDD9000   SECTION.
      *---------------------------------------------------------------*

PW6062     MOVE WRK-COD-DEPTO-N        TO WRK-MESUX0-AGENCIA
PW6062     PERFORM 9990-10-CHAMA-ROTINA-FX-AG
PW6062     IF  NOT  WRK-EH-FX-AG
PW6062             MOVE 'N'           TO WRK-PERM-OK
PW6062             MOVE WRK-M54       TO LNK-MENSAGEM
PW6062             GO  TO  1001-99-FIM
PW6062         ELSE
PW6062             MOVE 'S'           TO WRK-PERM-OK
PW6062             GO  TO  1001-99-FIM
PW6062         END-IF
PW6062     END-IF.
PW6062     MOVE 237                    TO LNK-BANCO-FUNC.
PW6062     MOVE WRK-COD-DEPTO-N        TO LNK-DEPTO-FUNC.
PW6062     MOVE WRK-COD-USER-N         TO LNK-COD-FUNC.
PW6062     MOVE INP-AGENCIA-T916       TO LNK-AGENCIA-PESQUISA.
PW6062     MOVE ZEROS                  TO LNK-STATUS
PW6062                                    LNK-RETORNO.
PW6062     MOVE SPACES                 TO LNK-MENSAGEM.
PW6062     MOVE 'RUEC'                 TO LNK-CENTRO-CUSTO.
PW6062     MOVE 'MIDD9000'             TO WRK-MODULO.

PW6062     CALL WRK-MODULO             USING IO-PCB
PW6062                                       ALT-PCB
PW6062                                       LNK-AREA-MIDD01.
PW6062         WHEN LNK-STATUS         EQUAL 99 AND
PW6062              LNK-RETORNO        EQUAL 99
PW6062              GOBACK
PW6062         WHEN LNK-STATUS         EQUAL ZEROS AND
PW6062              LNK-RETORNO        EQUAL ZEROS
PW6062              MOVE 'S'           TO WRK-PERM-OK
PW6062         WHEN LNK-STATUS         EQUAL 1 AND
PW6062              LNK-RETORNO        EQUAL 4
PW6062              MOVE 'N'           TO WRK-PERM-OK
PW6062              MOVE WRK-M54       TO LNK-MENSAGEM
PW6062         WHEN LNK-STATUS         EQUAL 2 AND
PW6062              LNK-RETORNO        EQUAL 4
PW6062              MOVE 'N'           TO WRK-PERM-OK
PW6062     END-EVALUATE.

      *---------------------------------------------------------------*
PW6062 1001-99-FIM. EXIT.
      *---------------------------------------------------------------*

       110-00-TRATA-IMPRESSAO SECTION.

CPM           (INP-IMP-CEDULA-T916              = 'X'  OR
RE0517         INP-IMP-CGEODE-T916              = 'X'  OR
CPM            INP-IMP-PROPOSTA-T916            = 'X'  OR
CPM            INP-IMP-FICHA-T916               = 'X'  OR
CPM            INP-IMP-ANEXOS-T916              = 'X'  OR
PW8123         INP-IMP-COMCZ-T916               = 'X'  OR
CPM            INP-IMP-OPERAC-T916              = 'X'  OR
CET            INP-IMP-CET-T916                 = 'X'  OR
CPM            INP-IMP-TODOS-T916               = 'X')

               MOVE    WRK-49369      TO    OUT-AGENCIA-ATTR-T916
               MOVE    WRK-49369      TO    OUT-CEDULA-ATTR-T916
               MOVE    WRK-M29        TO    OUT-MENSAG-T916
               GO  TO  110-99-TRATA-IMPRESSAO.

CPM        IF  INP-IMP-4VIAS-T916  EQUAL 'X'         AND
               CSTTUS-CONTR-RURAL OF RUFIB002   =  3

               MOVE    WRK-49369      TO    OUT-AGENCIA-ATTR-T916
               MOVE    WRK-49369      TO    OUT-CEDULA-ATTR-T916
               MOVE    WRK-M28        TO    OUT-MENSAG-T916

           IF (INP-IMP-4VIAS-T916  EQUAL 'X')
POL126**      PERFORM 232-00-ACESSAR-RUECB045
POL126**      IF SQLCODE EQUAL ZEROS OR -811
POL126        IF CSTTUS-CONTR-RURAL OF RUFIB002   =  6
                 PERFORM 234-00-INSERT-RUECB042
                 IF SQLCODE EQUAL -803
                    PERFORM 235-00-UPDATE-RUECB042
                 END-IF
                 PERFORM 233-00-PASSA-CTRL-RUEC2020
                 GO TO 110-99-TRATA-IMPRESSAO
              END-IF
           END-IF

********* CONSISTENCIA DE SELECAO PARA CEDULAS BNDES

ATOS       IF (CAPLIC-RURAL OF RUECV007 EQUAL 1 OR 2) AND
MAGA          (CORIGE-REC   OF RUECV007 > 59          AND
MAGA           CORIGE-REC   OF RUECV007 < 81)
MAGA             IF (INP-IMP-PROPOSTA-T916 EQUAL 'X' )
MAGA                 MOVE WRK-M24      TO  OUT-MENSAG-T916
                     GO TO 110-99-TRATA-IMPRESSAO.

           IF INP-IMP-4VIAS-T916   EQUAL 'X'  OR
              INP-REIMPR-T916 EQUAL 'X'
              PERFORM 150-00-ACESSA-RUECV019
              IF SQLCODE EQUAL ZEROS OR -811
                 MOVE  WRK-49353   TO   OUT-IMP-4VIAS-ATTR-T916
                 MOVE  WRK-M13     TO   OUT-MENSAG-T916
                 GO TO 110-99-TRATA-IMPRESSAO
              END-IF
           END-IF.

           PERFORM 237-00-VERIFICA-HISTORICO
           IF  OUT-MENSAG-T916  NOT EQUAL SPACES
               GO TO 110-99-TRATA-IMPRESSAO.

POL097     IF  INP-IMP-4VIAS-T916   EQUAL 'X'  OR
POL097         INP-REIMPR-T916 EQUAL 'X'
7C0325         MOVE 'S'       TO WRK-DTDAP-OK
POL097         PERFORM 065-00-VERIFICA-RUEC7720
7C0325         IF WRK-DTDAP-OK EQUAL 'N'
.                 MOVE WRK-M79        TO    OUT-MENSAG-T916
.                 GO TO 110-99-TRATA-IMPRESSAO
7C0325         END-IF
POL097         IF  WRK-EXISTE-RUECB033 NOT EQUAL 'S'
POL097             IF  R21-RET-CODE-R7720  EQUAL  01 OR 02
POL097                MOVE WRK-49369      TO    OUT-CEDULA-ATTR-T916
POL097                GO TO 110-99-TRATA-IMPRESSAO
POL097             ELSE
POL097             IF  R21-RET-CODE-R7720  NOT  EQUAL  ZEROS
POL097                 MOVE WRK-49369      TO    OUT-CEDULA-ATTR-T916
POL097                 MOVE WRK-M15        TO    OUT-MENSAG-T916
POL097                 GO TO 110-99-TRATA-IMPRESSAO
POL097             END-IF
POL097             END-IF
POL097         END-IF
POL097     END-IF.

POL097     IF  (INP-IMP-4VIAS-T916 EQUAL 'X')
POL097     AND (WRK-EXISTE-RUECB033 NOT EQUAL 'S')
POL097         IF  CCLI-UNIC-BDSCO           EQUAL ZEROS
POL097             MOVE    WRK-M50           TO    OUT-MENSAG-T916
POL097         END-IF
POL097     END-IF.

           PERFORM 170-00-OBTEM-IMPRESSORA

           IF RETURN-CODE NOT EQUAL ZEROS
MF0112        IF  WRK-COD-USER  NOT  EQUAL '0000001'
                  MOVE    WRK-M11              TO    OUT-MENSAG-T916
                  GO TO 110-99-TRATA-IMPRESSAO
MF0112        ELSE
MF0112            IF  INP-IMP-4VIAS-T916  NOT EQUAL 'X' AND 'R'
MF0112                MOVE    WRK-M11          TO    OUT-MENSAG-T916
MF0112                GO TO 110-99-TRATA-IMPRESSAO
MF0112            ELSE
MF0112                MOVE SPACES              TO    COMU-IMPRESSORA
MF0112            END-IF
MF0112        END-IF
           END-IF.

ACT181     IF  INP-IMP-4VIAS-T916  EQUAL 'X'
ACT181         PERFORM 238-00-VERIFICAR-FIM-ANO
ACT181             MOVE  WRK-MENS01    TO  OUT-MENSAG-T916
ACT181             GO                  TO  110-99-TRATA-IMPRESSAO.

PW9931     IF  (INP-IMP-4VIAS-T916   EQUAL 'B')
PW9931         IF  (CIMPRE-CDULA-TEMPR   EQUAL  2 )
PW9931             PERFORM  3010-ACESSAR-RUFIB002
PW9931             IF CREFT-RECOR OF RUFIB002 NOT EQUAL SPACES AND ZEROS
PW9931                MOVE WRK-M41     TO    OUT-MENSAG-T916
PW9931                GO TO 110-99-TRATA-IMPRESSAO
PW9931             END-IF
PW9931         ELSE
PW9931             MOVE WRK-M41     TO    OUT-MENSAG-T916
PW9931             GO TO 110-99-TRATA-IMPRESSAO
PW9931         END-IF
PW9931     END-IF.

PW9931     IF  INP-IMP-4VIAS-T916  EQUAL 'X' OR 'B'
PW9931         PERFORM 4890-VER-HORARIO-BACEN
PW9931         IF  (WRK-MENS01    NOT  EQUAL           SPACES)
PW9931             MOVE  WRK-MENS01    TO  OUT-MENSAG-T916
PW9931             GO                  TO  110-99-TRATA-IMPRESSAO
PW9931     END-IF.

EG1215     IF  INP-IMP-4VIAS-T916          EQUAL 'X' OR 'B'
EG1215         PERFORM 4920-COORDENADA-OBRIGATORIA
EG1215         IF  RUECWGEO-COORD-OBRIG    EQUAL 'S'
EG1215             PERFORM 4921-CONSISTIR-COORDENADAS
EG1215             IF  WRK-COORD-INCONSIS  EQUAL 'S'
EG1215                 GO TO 110-99-TRATA-IMPRESSAO
EG1215             END-IF
EG1215         END-IF
EG1215     END-IF.
EG1215

BS0617     IF  INP-IMP-4VIAS-T916          EQUAL 'X' OR 'B'
BS0617         PERFORM 4951-EXIGE-PRODUTIVIDADE
BS0617         IF  WRK-EXIGE-PRODUT    EQUAL 'S'
BS0617             MOVE 'EXISTE CONTRATO COM PENDENCIA DE PRODUTIVIDADE'
BS0617                                 TO OUT-MENSAG-T916
BS0617             GO TO 110-99-TRATA-IMPRESSAO
BS0617         END-IF
BS0617     END-IF.
PW9931         (CIMPRE-CDULA-TEMPR      EQUAL  0 )     AND
PW3932         (WRK-EXISTE-RUECB033     NOT EQUAL 'S')

BS0281         MOVE 'RUEC9160'          TO RUEC48-PGM-CHAMADOR
BS0281         MOVE  INP-AGENCIA-T916   TO RUEC48-AGENCIA
BS0281         MOVE  INP-CEDULA-T916    TO RUEC48-CEDULA
BS0281         MOVE 'RUEC7799'          TO WRK-MODULO
BS0281         CALL WRK-MODULO USING       RUEC48-AREA

BS0281         EVALUATE RUEC48-COD-RET
BS0281             WHEN 01
BS0281                 MOVE WRK-M71     TO    OUT-MENSAG-T916
BS0281                 GO TO 110-99-TRATA-IMPRESSAO
BS0281             WHEN 02
BS0281                 MOVE WRK-M72     TO    OUT-MENSAG-T916
BS0281                 GO TO 110-99-TRATA-IMPRESSAO
BS0281             WHEN 04
BS0281                 MOVE WRK-M73     TO    OUT-MENSAG-T916
BS0281                 GO TO 110-99-TRATA-IMPRESSAO
BS0281             WHEN 05
BS0281                 MOVE WRK-M74     TO    OUT-MENSAG-T916
BS0281             WHEN 99
BS0281                 MOVE RUEC48-SQLCA TO   SQLCA
BS0281                 MOVE RUEC48-ERRO-AREA TO ERRO-AREA
BS0281                 PERFORM 9000-ROTINA-ERRO
BS0281                 GO TO 110-99-TRATA-IMPRESSAO
BS0281         END-EVALUATE

PW3932         MOVE 'RUEC9160'          TO RUEC48-PGM-CHAMADOR
PW3932         MOVE  INP-AGENCIA-T916   TO RUEC48-AGENCIA
PW3932         MOVE  INP-CEDULA-T916    TO RUEC48-CEDULA
PW3932         MOVE 'RUEC7770'          TO WRK-MODULO
PW3932         CALL WRK-MODULO USING       RUEC48-AREA

PW3832         EVALUATE RUEC48-COD-RET
PW3932             WHEN 01
PW3932                 MOVE WRK-M44     TO    OUT-MENSAG-T916
PW3932                 GO TO 110-99-TRATA-IMPRESSAO
PW3932             WHEN 02
PW3932                 MOVE WRK-M45     TO    OUT-MENSAG-T916
PW3932                 GO TO 110-99-TRATA-IMPRESSAO
PW3932             WHEN 04
PW3932                 GO TO 110-99-TRATA-IMPRESSAO
PW3932             WHEN 05
PW3932                 MOVE WRK-M47     TO    OUT-MENSAG-T916
PW3932                 GO TO 110-99-TRATA-IMPRESSAO
EG1115             WHEN 06
EG1115                 MOVE WRK-M75     TO    OUT-MENSAG-T916
EG1115                 GO TO 110-99-TRATA-IMPRESSAO
EG1115             WHEN 07
EG1115                 MOVE WRK-M76     TO    OUT-MENSAG-T916
EG1115                 GO TO 110-99-TRATA-IMPRESSAO
AF0517             WHEN 08
EG0817                 STRING 'CLIENTE NAO ELEGIVEL PARA PRONAMP - '
EG0817                        RUEC48-MSG-MOTIVO
EG0817                        DELIMITED BY SIZE INTO OUT-MENSAG-T916
AF0517                 GO TO 110-99-TRATA-IMPRESSAO
7C1225             WHEN 09
7C1225                 MOVE WRK-M80     TO    OUT-MENSAG-T916
7C1225                 GO TO 110-99-TRATA-IMPRESSAO
PW3932             WHEN 99
PW3932                 MOVE RUEC48-SQLCA TO   SQLCA
PW3932                 MOVE RUEC48-ERRO-AREA TO ERRO-AREA
PW3932                 GO TO 110-99-TRATA-IMPRESSAO
PW3932         END-EVALUATE

BS0616         PERFORM 4930-REGRAS-PROAGRO
BS0616
DK0312         INITIALIZE RUEC48-AREA
DK0312         MOVE 'RUEC7796'          TO WRK-MODULO
DK0312         MOVE 'RUEC9160'          TO RUEC48-PGM-CHAMADOR
DK0312         MOVE  INP-AGENCIA-T916   TO RUEC48-AGENCIA
DK0312         MOVE  INP-CEDULA-T916    TO RUEC48-CEDULA
DK0312         CALL WRK-MODULO USING       RUEC48-AREA
DK0312         EVALUATE RUEC48-COD-RET
DK0312             WHEN 01
DK0312                 MOVE WRK-M55     TO    OUT-MENSAG-T916
DK0312                 GO TO 110-99-TRATA-IMPRESSAO
DK0312             WHEN 02
DK0312                 MOVE WRK-M56     TO    OUT-MENSAG-T916
DK0312                 GO TO 110-99-TRATA-IMPRESSAO
DK0312             WHEN 99
DK1312                 MOVE RUEC48-SQLCA TO   SQLCA
DK0312                 MOVE RUEC48-ERRO-AREA TO ERRO-AREA
DK0312                 GO TO 110-99-TRATA-IMPRESSAO
DK0312         END-EVALUATE
PW3932     END-IF

ACT046     IF  (INP-IMP-4VIAS-T916      EQUAL 'X')     AND
PW9931         (CIMPRE-CDULA-TEMPR      EQUAL  0 )     AND
ACT046         (WRK-EXISTE-RUECB033     NOT EQUAL 'S')
ACT046         PERFORM 3000-ACESSO-FORP
ACT046         EVALUATE FORP14-RC-RET
ACT046             WHEN 00
ACT046                 PERFORM 3040-ATUALIZAR-RUECV007
ACT046             WHEN 01
ACT046                 MOVE WRK-M37     TO    OUT-MENSAG-T916
ACT046                 GO TO 110-99-TRATA-IMPRESSAO
ACT046             WHEN 02
ACT046                 MOVE WRK-M38     TO    OUT-MENSAG-T916
ACT046                 GO TO 110-99-TRATA-IMPRESSAO
ACT046             WHEN 88
ACT046                 MOVE WRK-M39     TO    OUT-MENSAG-T916
ACT046                 GO TO 110-99-TRATA-IMPRESSAO
ACT046             WHEN 99
ACT046                 GO TO 110-99-TRATA-IMPRESSAO
ACT046         END-EVALUATE
ACT046     END-IF

PW9931*****SE  INP-IMP-4VIAS-T916   EQUAL 'R'
PW9931*********PERFORMA 3010-ACESSAR-RUFIB002
PW9931*********SE  CREFT-RECOR OF RUFIB002 NOT EQUAL SPACES AND ZEROS
PW9931*************MOVER WRK-M41     TO    OUT-MENSAG-T916
PW9931*************IR P/ 110-99-TRATA-IMPRESSAO
PW9931*********TEND-IF
PW9931*****TEND-IF.

PW7398     IF INP-IMP-4VIAS-T916   EQUAL 'R'
PW7398        PERFORM 2000-ABRIR-CSR01-RUECB034
PW7398        MOVE 'N'               TO WRK-FIM-RUECB034
PW7398        PERFORM 2010-FETCH-CSR01-RUECB034
PW7398        PERFORM 2020-PROCESSAR-CSR01-RUECB034 UNTIL
PW7398                               WRK-FIM-RUECB034  EQUAL 'S'
PW7398        PERFORM 2030-CLOSE-CSR01-RUECB034
PW7398        PERFORM 2050-DELETAR-ANTERIOR
PW7398        MOVE SPACES             TO WRK-CODIFIC
PW7398        MOVE INP-CEDULA-T916    TO WRK-CCDULA
PW7398        MOVE 'IR'               TO WRK-SIT-IMPRE
PW7398        PERFORM 2040-INSERIR-RUCAB002
POL097        PERFORM 2060-DELETAR-HIST-PESSOA
PW7398     END-IF.

PW9931     MOVE ZEROS              TO RUEC54-CODRET.

PW2875     IF (INP-IMP-4VIAS-T916   EQUAL 'X'   )     AND
PW9931        (CIMPRE-CDULA-TEMPR      EQUAL  0 )     AND
BR0217        (INP-CTRBAC-T916      EQUAL SPACES)
PW2875        IF WRK-EXISTE-RUECB033  EQUAL 'N'
PW2875           PERFORM 2500-AJUSTAR-PARCELAS
PW2875        ELSE
PW2875           MOVE DIMPRE OF RUECB033(1:2) TO WRK-DIA-AUX-2
PW2875           MOVE DIMPRE OF RUECB033(4:2) TO WRK-MES-AUX-2
PW2875           MOVE DIMPRE OF RUECB033(7:4) TO WRK-ANO-AUX-2
PW2875           IF CIMPRE-CDULA-TEMPR OF RUECV007 EQUAL ZEROS AND
PW2875              DT-AAAAMMDD        GREATER WRK-DATA-AUX-R2 AND
BR0218              INP-CTRBAC-T916    EQUAL SPACES
PW2875              MOVE DIMPRE OF RUECB033 TO DCDULA OF RUECV007
PW4875           END-IF
PW2875        END-IF
PW2875     END-IF
PW2875
PW2875     IF RUEC54-CODRET     EQUAL 01
PW2875        GO TO 110-99-TRATA-IMPRESSAO
PW2875     END-IF.

PW9931     IF  (INP-IMP-4VIAS-T916      EQUAL 'X')     AND
PW9931         (CIMPRE-CDULA-TEMPR      EQUAL  0 )     AND
PW9931         (WRK-EXISTE-RUECB033     NOT EQUAL 'S')
PW9931         PERFORM 4892-VER-RUEC7764
PW9931         IF  (WRK-MENS01    NOT  EQUAL           SPACES)
PW9931             MOVE  WRK-MENS01    TO  OUT-MENSAG-T916
PW9931             GO                  TO  110-99-TRATA-IMPRESSAO
PW9931         END-IF
PW9931     END-IF.

PW9931     IF  (INP-IMP-4VIAS-T916      EQUAL 'X' OR 'B')     AND
PW9931         (WRK-EXISTE-RUECB033     NOT EQUAL 'S')
DPS01***********F I*P-FLG-CET-T916      N*T *QUAL 'X'
DPS01*************G*                    T*  110-99-TR*TA-IMPRESSAO
DPS01***********ND-*F
PW9931         PERFORM 4894-TRATAR-RECOR
PW9931         IF  (WRK-MENS01    NOT  EQUAL           SPACES)
PW9931             MOVE  WRK-MENS01    TO  OUT-MENSAG-T916
PW9931             GO                  TO  110-99-TRATA-IMPRESSAO
PW9931         END-IF
PW9931     END-IF.

PW9931     IF  (INP-IMP-4VIAS-T916   EQUAL 'B')
PW9931         GO TO 110-99-TRATA-IMPRESSAO
PW9931     END-IF.

           PERFORM   236-00-PASSA-CTRL-RUEC8600.

       110-99-TRATA-IMPRESSAO. EXIT.

PW7398*---------------------------------------------------------------*
PW7398*    ABRIR CURSOR 01 DO RUECB034
PW7398*---------------------------------------------------------------*
PW7398 2000-ABRIR-CSR01-RUECB034 SECTION.

PW7398     EXEC SQL
PW7398          OPEN CSR01-RUECB034
PW7398     END-EXEC.

PW7398     IF (SQLCODE                 NOT EQUAL ZEROS) OR
PW7398        (SQLWARN0                EQUAL 'W')
PW7398         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
PW7398         MOVE 'HIST_IMPRE_CDULA' TO  ERR-DBD-TAB
PW7398         MOVE 'OPEN  '           TO  ERR-FUN-COMANDO
PW7398         MOVE SQLCODE            TO  ERR-SQL-CODE
PW7398         MOVE '0030'             TO  ERR-LOCAL
PW7398         MOVE SPACES             TO  ERR-SEGM
PW7398         PERFORM 9000-ROTINA-ERRO
PW7398     END-IF.

PW7398*
PW7398 2000-99-EXIT.
PW7398         EXIT.

PW7398*---------------------------------------------------------------*
PW7398*---------------------------------------------------------------*
PW7398 2010-FETCH-CSR01-RUECB034 SECTION.
PW7398*---------------------------------------------------------------*

PW7398     EXEC SQL
PW7398          FETCH CSR01-RUECB034
PW7398          INTO  :RUECB034.CLIN-FIS-TEXTO,
PW7398                :RUECB034.RLIN-MAX,
PW7398                :RUECB034.CPAG
PW7398     END-EXEC.

PW7398     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
PW7398        (SQLWARN0                EQUAL 'W')
PW7398         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
PW7398         MOVE 'HIST_IMPRE_CDULA' TO  ERR-DBD-TAB
PW7398         MOVE 'FETCH '           TO  ERR-FUN-COMANDO
PW7398         MOVE SQLCODE            TO  ERR-SQL-CODE
PW7398         MOVE '0031'             TO  ERR-LOCAL
PW7398         MOVE SPACES             TO  ERR-SEGM
PW7398         PERFORM 9000-ROTINA-ERRO
PW7398     END-IF.
PW7398     IF  SQLCODE                 EQUAL  +100
PW7398         MOVE 'S'                TO   WRK-FIM-RUECB034
PW7398     END-IF.
PW7398*
PW7398 2010-99-EXIT.
PW7398         EXIT.

PW7398*---------------------------------------------------------------*
PW7398* PROCESSAR CURSOR 01 DO RUECB034
PW7398*---------------------------------------------------------------*
PW7398 2020-PROCESSAR-CSR01-RUECB034 SECTION.
PW7398*---------------------------------------------------------------*

PW7398     IF  WRK-EXISTE-RUECB034-REC  EQUAL  'S'
PW7398     AND WRK-EXISTE-RUECB034      EQUAL  'S'
PW7398         GO  TO  2020-10-DELETE
PW7398     END-IF.

PW7398     COMPUTE CLIN-FIS-TEXTO = CLIN-FIS-TEXTO * -1.
PW7398     EXEC SQL
PW7398          INSERT INTO DB2PRD.HIST_IMPRE_CDULA
PW7398                   CCDULA,
PW7398                   CLIN_FIS_TEXTO,
PW7398                   RLIN_MAX,
PW7398                   CPAG)
PW7398          VALUES (:RUECB034.CJUNC-DEPDC,
PW7398                  :RUECB034.CCDULA,
PW7398                  :RUECB034.CLIN-FIS-TEXTO,
PW7398                  :RUECB034.RLIN-MAX,
PW7398                  :RUECB034.CPAG)
PW7398     END-EXEC.

PW7398     IF (SQLCODE                 NOT EQUAL ZEROS)
PW7398         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
PW7398         MOVE 'HIST_IMPRE_CDULA' TO  ERR-DBD-TAB
PW7398         MOVE 'INSERT'           TO  ERR-FUN-COMANDO
PW7398         MOVE SQLCODE            TO  ERR-SQL-CODE
PW7398         MOVE '0032'             TO  ERR-LOCAL
PW7398         MOVE SPACES             TO  ERR-SEGM
PW7398         PERFORM 9000-ROTINA-ERRO
PW7398     END-IF.

PW7398* DELETAR   CURSOR 01 DO RUECB034
PW7398*---------------------------------------------------------------*
PW7398 2020-10-DELETE.

PW7398     EXEC SQL
PW7398          DELETE FROM DB2PRD.HIST_IMPRE_CDULA
PW7398             WHERE CURRENT OF CSR01-RUECB034
PW7398     END-EXEC.

PW7398     IF (SQLCODE                 NOT EQUAL ZEROS)
PW7398         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
PW7398         MOVE 'HIST_IMPRE_CDULA' TO  ERR-DBD-TAB
PW7398         MOVE 'DELETE'           TO  ERR-FUN-COMANDO
PW7398         MOVE SQLCODE            TO  ERR-SQL-CODE
PW7398         MOVE '0033'             TO  ERR-LOCAL
PW7398         MOVE SPACES             TO  ERR-SEGM
PW7398         PERFORM 9000-ROTINA-ERRO
PW7398     END-IF.

PW7398     PERFORM 2010-FETCH-CSR01-RUECB034.
PW7398*
PW7398         EXIT.

PW7398*---------------------------------------------------------------*
PW7398*   FECHAR CURSOR 01 DO RUECB034
PW7398*---------------------------------------------------------------*
PW7398 2030-CLOSE-CSR01-RUECB034 SECTION.
PW7398*---------------------------------------------------------------*

PW7398     EXEC SQL
PW7398          CLOSE CSR01-RUECB034
PW7398     END-EXEC.

PW7398     IF (SQLCODE                 NOT EQUAL ZEROS) OR
PW7398        (SQLWARN0                EQUAL 'W')
PW7398         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
PW7398         MOVE 'HIST_IMPRE_CDULA' TO  ERR-DBD-TAB
PW7398         MOVE 'CLOSE '           TO  ERR-FUN-COMANDO
PW7398         MOVE SQLCODE            TO  ERR-SQL-CODE
PW7398         MOVE '0034'             TO  ERR-LOCAL
PW7398         MOVE SPACES             TO  ERR-SEGM
PW7398         PERFORM 9000-ROTINA-ERRO
PW7398*
PW7398 2030-99-EXIT.
PW7398         EXIT.

PW7398*---------------------------------------------------------------*
PW7398 2040-INSERIR-RUCAB002     SECTION.
PW7398*---------------------------------------------------------------*
PW7398
PW7398     COMPUTE B002-DATA = DT-JULIANA + 2000000.
PW7398
PW7398     MOVE TI-HHMMSS            TO B002-HORA.
PW7398     MOVE WRK-COD-USER         TO WRK-COD-USER-R.
PW7398     MOVE WRK-COD-USER-N       TO B002-COD-FUNC.
PW7398     MOVE 94                   TO B002-CARTAO.
PW7398     MOVE WRK-CODIFIC          TO B002-CODIFIC.
PW7398
PW7398     MOVE ZEROS                TO B002-LIB-PAG
PW7398                                  B002-PARCELA.
PW7398     MOVE SPACES               TO B002-DADOS-ENTRD.
PW7398
PW7398     EXEC SQL
PW7398              (B002_DATA,
PW7398               B002_HORA,
PW7398               B002_COD_FUNC,
PW7398               B002_CARTAO,
PW7398               B002_CODIFIC,
PW7398               B002_LIB_PAG,
PW7398               B002_PARCELA,
PW7398               B002_DADOS_ENTRD)
PW7398        VALUES
PW7398              (:RUCAB002.B002-DATA,
PW7398               :RUCAB002.B002-HORA,
PW7398               :RUCAB002.B002-COD-FUNC,
PW7398               :RUCAB002.B002-CARTAO,
PW7398               :RUCAB002.B002-CODIFIC,
PW7398               :RUCAB002.B002-LIB-PAG,
PW7398               :RUCAB002.B002-PARCELA,
PW7398               :RUCAB002.B002-DADOS-ENTRD)
PW7398     END-EXEC.
PW7398
PW7398     IF (SQLCODE                 NOT EQUAL ZEROS) OR
PW7398        (SQLWARN0                EQUAL 'W')
PW7398         MOVE 'RUCA_MOVTO'       TO  ERR-DBD-TAB
PW7398         MOVE 'INSERT'           TO  ERR-FUN-COMANDO
PW7398         MOVE SQLCODE            TO  ERR-SQL-CODE
PW7398         MOVE '0036'             TO  ERR-LOCAL
PW7398         MOVE SPACES             TO  ERR-SEGM
PW7398         PERFORM 9000-ROTINA-ERRO
PW7398     END-IF.
PW7398*
PW7398 2040-99-EXIT.
PW7398         EXIT.

PW7398*---------------------------------------------------------------*
PW7398 2050-DELETAR-ANTERIOR     SECTION.
PW7398*---------------------------------------------------------------*
PW7398
PW7398     MOVE 94                   TO B002-CARTAO.
PW7398     MOVE SPACES               TO WRK-CODIFIC.
PW7398     MOVE INP-AGENCIA-T916     TO WRK-CJUNC-DEPDC.
PW7398     MOVE INP-CEDULA-T916      TO WRK-CCDULA.
PW7398     MOVE 'IR'                 TO WRK-SIT-IMPRE.
PW7398     MOVE WRK-CODIFIC          TO B002-CODIFIC.
PW7398                                  B002-PARCELA.
PW7398
PW7398     EXEC SQL
PW7398         DELETE FROM DB2PRD.RUCA_MOVTO
PW7398          WHERE B002_CARTAO  = :B002-CARTAO
PW7398            AND B002_CODIFIC = :B002-CODIFIC
PW7398            AND B002_LIB_PAG = :B002-LIB-PAG
PW7398            AND B002_PARCELA = :B002-PARCELA
PW7398     END-EXEC.
PW7398
PW7398     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
PW7398        (SQLWARN0                EQUAL 'W')
PW7398         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
PW7398         MOVE 'RUCA_MOVTO'       TO  ERR-DBD-TAB
PW7398         MOVE 'DELETE'           TO  ERR-FUN-COMANDO
PW7398         MOVE SQLCODE            TO  ERR-SQL-CODE
PW7398         MOVE '0037'             TO  ERR-LOCAL
PW7398         MOVE SPACES             TO  ERR-SEGM
PW7398         PERFORM 9000-ROTINA-ERRO
PW7398     END-IF.
PW7398
PW7398     MOVE WRK-CODIFIC          TO B002-CODIFIC.
PW7398
PW7398     EXEC SQL
PW7398         DELETE FROM DB2PRD.RUCA_MOVTO
PW7398          WHERE B002_CARTAO  = :B002-CARTAO
PW7398            AND B002_CODIFIC = :B002-CODIFIC
PW7398            AND B002_LIB_PAG = :B002-LIB-PAG
PW7398            AND B002_PARCELA = :B002-PARCELA
PW7398     END-EXEC.
PW7398
PW7398     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
PW7398        (SQLWARN0                EQUAL 'W')
PW7398         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
PW7398         MOVE 'RUCA_MOVTO'       TO  ERR-DBD-TAB
PW7398         MOVE 'DELETE'           TO  ERR-FUN-COMANDO
PW7398         MOVE SQLCODE            TO  ERR-SQL-CODE
PW7398         MOVE '0038'             TO  ERR-LOCAL
PW7398         MOVE SPACES             TO  ERR-SEGM
PW7398         PERFORM 9000-ROTINA-ERRO
PW7398     END-IF.
PW7398*
PW7398         EXIT.

POL097*---------------------------------------------------------------*
POL097 2060-DELETAR-HIST-PESSOA  SECTION.
POL097*---------------------------------------------------------------*

POL097        PERFORM 2065-ABRIR-CSR01-RUECB052
POL097        MOVE 'N'               TO WRK-FIM-RUECB052
POL097        PERFORM 2066-FETCH-CSR01-RUECB052
POL097        PERFORM 2067-DELETAR-CSR01-RUECB052 UNTIL
POL097                               WRK-FIM-RUECB052  EQUAL 'S'
POL097        PERFORM 2068-CLOSE-CSR01-RUECB052.
POL097
POL097        PERFORM 2069-ABRIR-CSR01-RUECB053
POL097        MOVE 'N'               TO WRK-FIM-RUECB053
POL097        PERFORM 2070-FETCH-CSR01-RUECB053
POL097        PERFORM 2071-DELETAR-CSR01-RUECB053 UNTIL
POL097                               WRK-FIM-RUECB053  EQUAL 'S'
POL097        PERFORM 2072-CLOSE-CSR01-RUECB053.
POL097
POL097        PERFORM 2073-ABRIR-CSR01-RUECB054
POL097        PERFORM 2074-FETCH-CSR01-RUECB054
POL097        PERFORM 2075-DELETAR-CSR01-RUECB054 UNTIL
POL097                               WRK-FIM-RUECB054  EQUAL 'S'
POL097        PERFORM 2076-CLOSE-CSR01-RUECB054.
POL097
POL097 2060-99-EXIT.
POL097         EXIT.

POL097*---------------------------------------------------------------*
POL097*    ABRIR CURSOR 01 DO RUECB052
POL097*---------------------------------------------------------------*
POL097 2065-ABRIR-CSR01-RUECB052 SECTION.
POL097*---------------------------------------------------------------*

POL097     MOVE  CCONTR OF RUFIB002    TO CCONTR OF RUECB052.

POL097     EXEC SQL
POL097          OPEN CSR01-RUECB052
POL097     END-EXEC.

POL097     IF (SQLCODE                 NOT EQUAL ZEROS) OR
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_IMOV_RURAL' TO  ERR-DBD-TAB
POL097         MOVE 'OPEN  '           TO  ERR-FUN-COMANDO
POL097         MOVE SQLCODE            TO  ERR-SQL-CODE
POL097         MOVE '0045'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.

POL097*
POL097 2065-99-EXIT.
POL097         EXIT.

POL097*---------------------------------------------------------------*
POL097*    LER   CURSOR 01 DO RUECB052
POL097*---------------------------------------------------------------*
POL097 2066-FETCH-CSR01-RUECB052 SECTION.
POL097*---------------------------------------------------------------*

POL097     EXEC SQL
POL097          FETCH CSR01-RUECB052
POL097                :RUECB052.CSEQ-HIST-IMOV
POL097     END-EXEC.

POL097     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
POL097        (SQLWARN0                EQUAL 'W')
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_IMOV_RURAL' TO  ERR-DBD-TAB
POL097         MOVE 'FETCH '           TO  ERR-FUN-COMANDO
POL097         MOVE SQLCODE            TO  ERR-SQL-CODE
POL097         MOVE '0046'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.

POL097     IF  SQLCODE                 EQUAL  +100
POL097         MOVE 'S'                TO   WRK-FIM-RUECB052
POL097     END-IF.

POL097 2066-99-EXIT.
POL097         EXIT.

POL097* DELETAR   CURSOR 01 DO RUECB052
POL097*---------------------------------------------------------------*
POL097 2067-DELETAR-CSR01-RUECB052 SECTION.

POL097     EXEC SQL
POL097          DELETE FROM DB2PRD.THIST_IMOV_RURAL
POL097             WHERE CURRENT OF CSR01-RUECB052
POL097     END-EXEC.

POL097     IF (SQLCODE                 NOT EQUAL ZEROS)
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_IMOV_RURAL' TO  ERR-DBD-TAB
POL097         MOVE 'DELETE'           TO  ERR-FUN-COMANDO
POL097         MOVE SQLCODE            TO  ERR-SQL-CODE
POL097         MOVE '0047'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.

POL097     PERFORM 2066-FETCH-CSR01-RUECB052.
POL097*
POL097         EXIT.

POL097*---------------------------------------------------------------*
POL097*   FECHAR CURSOR 01 DO RUECB052
POL097*---------------------------------------------------------------*
POL097 2068-CLOSE-CSR01-RUECB052 SECTION.
POL097*---------------------------------------------------------------*

POL097     EXEC SQL
POL097          CLOSE CSR01-RUECB052
POL097     END-EXEC.

POL097     IF (SQLCODE                 NOT EQUAL ZEROS) OR
POL097        (SQLWARN0                EQUAL 'W')
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_IMOV_RURAL' TO  ERR-DBD-TAB
POL097         MOVE 'CLOSE '           TO  ERR-FUN-COMANDO
POL097         MOVE SQLCODE            TO  ERR-SQL-CODE
POL097         MOVE '0048'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097*
POL097 2068-99-EXIT.
POL097         EXIT.

POL097*---------------------------------------------------------------*
POL097*    ABRIR CURSOR 01 DO RUECB053
POL097*---------------------------------------------------------------*
POL097 2069-ABRIR-CSR01-RUECB053 SECTION.
POL097*---------------------------------------------------------------*

POL097     MOVE  CCONTR OF RUFIB002    TO CCONTR OF RUECB053.

POL097     EXEC SQL
POL097          OPEN CSR01-RUECB053
POL097     END-EXEC.

POL097     IF (SQLCODE                 NOT EQUAL ZEROS) OR
POL097        (SQLWARN0                EQUAL 'W')
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_RBNHO_CONT' TO  ERR-DBD-TAB
POL097         MOVE 'OPEN  '           TO  ERR-FUN-COMANDO
POL097         MOVE '0049'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.
POL097*
POL097 2069-99-EXIT.
POL097         EXIT.

POL097*---------------------------------------------------------------*
POL097*    LER   CURSOR 01 DO RUECB053
POL097*---------------------------------------------------------------*
POL097 2070-FETCH-CSR01-RUECB053 SECTION.
POL097*---------------------------------------------------------------*

POL097     EXEC SQL
POL097          FETCH CSR01-RUECB053
POL097          INTO  :RUECB053.CCONTR,
POL097                :RUECB053.CPRODT-RURAL,
POL097                :RUECB053.CSEQ-HIST-RBNHO
POL097     END-EXEC.

POL097        (SQLWARN0                EQUAL 'W')
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_RBNHO_CONT' TO  ERR-DBD-TAB
POL097         MOVE 'FETCH '           TO  ERR-FUN-COMANDO
POL097         MOVE SQLCODE            TO  ERR-SQL-CODE
POL097         MOVE '0050'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.

POL097     IF  SQLCODE                 EQUAL  +100
POL097         MOVE 'S'                TO   WRK-FIM-RUECB053
POL097     END-IF.

POL097 2070-99-EXIT.
POL097         EXIT.

POL097*---------------------------------------------------------------*
POL097* DELETAR   CURSOR 01 DO RUECB053
POL097*---------------------------------------------------------------*
POL097 2071-DELETAR-CSR01-RUECB053 SECTION.
POL097     EXEC SQL
POL097          DELETE FROM DB2PRD.THIST_RBNHO_CONTR
POL097             WHERE CURRENT OF CSR01-RUECB053
POL097     END-EXEC.

POL097     IF (SQLCODE                 NOT EQUAL ZEROS)
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_RBNHO_CONT' TO  ERR-DBD-TAB
POL097         MOVE 'DELETE'           TO  ERR-FUN-COMANDO
POL097         MOVE SQLCODE            TO  ERR-SQL-CODE
POL097         MOVE '0051'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.

POL097     PERFORM 2070-FETCH-CSR01-RUECB053.
POL097*
POL097 2071-99-EXIT.
POL097         EXIT.

POL097*---------------------------------------------------------------*
POL097*---------------------------------------------------------------*
POL097 2072-CLOSE-CSR01-RUECB053 SECTION.
POL097*---------------------------------------------------------------*

POL097     EXEC SQL
POL097          CLOSE CSR01-RUECB053
POL097     END-EXEC.

POL097     IF (SQLCODE                 NOT EQUAL ZEROS) OR
POL097        (SQLWARN0                EQUAL 'W')
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_RBNHO_CONT' TO  ERR-DBD-TAB
POL097         MOVE 'CLOSE '           TO  ERR-FUN-COMANDO
POL097         MOVE SQLCODE            TO  ERR-SQL-CODE
POL097         MOVE '0052'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.
POL097*
POL097 2072-99-EXIT.
POL097         EXIT.
POL097*---------------------------------------------------------------*
POL097*    ABRIR CURSOR 01 DO RUECB054
POL097*---------------------------------------------------------------*
POL097 2073-ABRIR-CSR01-RUECB054 SECTION.
POL097*---------------------------------------------------------------*

POL097     MOVE  CCONTR OF RUFIB002    TO CCONTR OF RUECB054.

POL097     EXEC SQL
POL097          OPEN CSR01-RUECB054
POL097     END-EXEC.

POL097     IF (SQLCODE                 NOT EQUAL ZEROS) OR
POL097        (SQLWARN0                EQUAL 'W')
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_RENDA_AGRO' TO  ERR-DBD-TAB
POL097         MOVE 'OPEN  '           TO  ERR-FUN-COMANDO
POL097         MOVE SQLCODE            TO  ERR-SQL-CODE
POL097         MOVE '0053'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097*
POL097 2073-99-EXIT.
POL097         EXIT.

POL097*---------------------------------------------------------------*
POL097*    LER   CURSOR 01 DO RUECB054
POL097*---------------------------------------------------------------*
POL097 2074-FETCH-CSR01-RUECB054 SECTION.
POL097*---------------------------------------------------------------*

POL097     EXEC SQL
POL097          FETCH CSR01-RUECB054
POL097          INTO  :RUECB054.CCONTR,
POL097                :RUECB054.CPRODT-RURAL
POL097     END-EXEC.

POL097     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
POL097        (SQLWARN0                EQUAL 'W')
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_RENDA_AGRO' TO  ERR-DBD-TAB
POL097         MOVE 'FETCH '           TO  ERR-FUN-COMANDO
POL097         MOVE '0054'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.

POL097     IF  SQLCODE                 EQUAL  +100
POL097         MOVE 'S'                TO   WRK-FIM-RUECB054
POL097     END-IF.

POL097 2074-99-EXIT.
POL097         EXIT.

POL097*---------------------------------------------------------------*
POL097* DELETAR   CURSOR 01 DO RUECB054
POL097*---------------------------------------------------------------*
POL097 2075-DELETAR-CSR01-RUECB054 SECTION.

POL097     EXEC SQL
POL097          DELETE FROM DB2PRD.THIST_RENDA_AGROP
POL097             WHERE CURRENT OF CSR01-RUECB054
POL097     END-EXEC.
POL097     IF (SQLCODE                 NOT EQUAL ZEROS)
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_RENDA_AGRO' TO  ERR-DBD-TAB
POL097         MOVE 'DELETE'           TO  ERR-FUN-COMANDO
POL097         MOVE SQLCODE            TO  ERR-SQL-CODE
POL097         MOVE '0055'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.

POL097     PERFORM 2074-FETCH-CSR01-RUECB054.
POL097*
POL097 2075-99-EXIT.
POL097         EXIT.

POL097*---------------------------------------------------------------*
POL097*   FECHAR CURSOR 01 DO RUECB054
POL097*---------------------------------------------------------------*
POL097 2076-CLOSE-CSR01-RUECB054 SECTION.
POL097*---------------------------------------------------------------*

POL097          CLOSE CSR01-RUECB054
POL097     END-EXEC.

POL097     IF (SQLCODE                 NOT EQUAL ZEROS) OR
POL097        (SQLWARN0                EQUAL 'W')
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'THIST_RENDA_AGRO' TO  ERR-DBD-TAB
POL097         MOVE 'CLOSE '           TO  ERR-FUN-COMANDO
POL097         MOVE SQLCODE            TO  ERR-SQL-CODE
POL097         MOVE '0056'             TO  ERR-LOCAL
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.
POL097*
POL097 2076-99-EXIT.
POL097         EXIT.

POL097*---------------------------------------------------------------*
POL097 2090-INSERIR-RUECB033     SECTION.
POL097*---------------------------------------------------------------*

POL097     MOVE COMU-CEDULA  TO CCDULA         OF RUECB033.
POL097     MOVE  1           TO CINDCD-LOC-FIS OF RUECB033.

POL097     EXEC SQL
POL097          INSERT INTO DB2PRD.CTRL_HIST_IMPRE
POL097                 ( CJUNC_DEPDC,
POL097                   CCDULA,
POL097                   DIMPRE,
POL097                   CINDCD_LOC_FIS )
POL097          VALUES (:RUECB033.CJUNC-DEPDC,
POL097                  :RUECB033.CCDULA,
POL097                  CURRENT DATE,
POL097                  :RUECB033.CINDCD-LOC-FIS)
POL097     END-EXEC.

POL097     IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
POL097        (SQLWARN0                EQUAL 'W')
POL097         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
POL097         MOVE 'CTRL_HIST_IMPRE'  TO  ERR-DBD-TAB
POL097         MOVE 'INSERT'           TO  ERR-FUN-COMANDO
POL097         MOVE SQLCODE            TO  ERR-SQL-CODE
POL097         MOVE SPACES             TO  ERR-SEGM
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.
POL097*
POL097 2090-99-EXIT.
POL097         EXIT.

PW2875*------------------------------------*
PW2875 2500-AJUSTAR-PARCELAS  SECTION.
PW2875*------------------------------------*

           INITIALIZE RUEC53-AREA  RUEC54-AREA.

           MOVE CORIGE-REC   OF RUECV007 TO RUEC53-ORIGEM.
           MOVE CFNALD-RURAL OF RUECV007 TO RUEC53-FINALIDADE.
           MOVE CATVDD-RURAL OF RUECV007 TO RUEC53-ATIVIDADE.
           MOVE CPRODT-RURAL OF RUECV007 TO RUEC53-PRODUTO.

           MOVE 'RUEC7786'               TO WRK-MODULO.
           CALL WRK-MODULO   USING       RUEC53-AREA.

               MOVE RUEC53-ERRO-AREA     TO ERRO-AREA
               MOVE RUEC53-SQLCA         TO SQLCA
               PERFORM 9000-ROTINA-ERRO
           END-IF.

           IF  RUEC53-CODRET    EQUAL 01
               MOVE WRK-M51              TO OUT-MENSAG-T916
           END-IF.
           IF  RUEC53-CODRET    EQUAL 02
               MOVE 'S'                  TO RUEC54-ALTERA-VENCTO
               MOVE '31.12.9999'         TO RUEC54-DATA-LIMITE
               MOVE ZEROS                TO RUEC54-DIA-FIXO
               PERFORM 2600-EXECUTAR-RUEC7788
           END-IF.
           IF  RUEC53-CODRET    EQUAL 00 AND
               RUEC53-ALTERA-VENCTO EQUAL 'S'
               MOVE RUEC53-ALTERA-VENCTO TO RUEC54-ALTERA-VENCTO
               MOVE RUEC53-DATA-LIMITE   TO RUEC54-DATA-LIMITE
               MOVE RUEC53-DIA-FIXO      TO RUEC54-DIA-FIXO
               PERFORM 2600-EXECUTAR-RUEC7788
           END-IF.
       2500-99-FIM. EXIT.

PW2875*------------------------------------*
PW2875 2600-EXECUTAR-RUEC7788 SECTION.
PW2875*------------------------------------*

           MOVE INP-AGENCIA-T916     TO RUEC54-AGENCIA
           MOVE INP-CEDULA-T916      TO RUEC54-CEDULA.
           MOVE CCONTR OF RUECV007   TO RUEC54-CCONTR.
           MOVE DCDULA OF RUECV007   TO RUEC54-DT-CEDULA.

           MOVE 'RUEC7788'           TO WRK-MODULO.

           CALL WRK-MODULO   USING   RUEC54-AREA.

           IF RUEC54-CODRET   EQUAL 99
              MOVE RUEC54-ERRO-AREA  TO ERRO-AREA
              MOVE RUEC54-SQLCA      TO SQLCA
              PERFORM 9000-ROTINA-ERRO
           END-IF.

              MOVE RUEC54-MSGRET     TO OUT-MENSAG-T916
           END-IF.

       2600-99-FIM. EXIT.

ACT046*---------------------------------------------------------------*
ACT046*    INCLUI CONTRATO NO SISTEMA DE FORMALIZACAO
ACT046*----------------------------------------------------------------*
ACT046 3000-ACESSO-FORP              SECTION.
ACT046*----------------------------------------------------------------*

ACT046     INITIALIZE FORP14
ACT046          REPLACING  ALPHANUMERIC BY SPACES
ACT046                     NUMERIC      BY ZEROS.

ACT046     PERFORM 3010-ACESSAR-RUFIB002.
ACT046     PERFORM 3020-ACESSAR-RURCV040.
ACT046     PERFORM 3030-ACESSAR-RURCV037.
ACT046     PERFORM 3070-ACESSAR-RUECV014.

ACT046     PERFORM 3050-ACESSAR-MESU9015.

ACT046     MOVE 237                        TO  FORP14-CBCO-FORML.
ACT046     MOVE 612                        TO  FORP14-CPRODT-FORML.
ACT046     MOVE CJUNC-DEPDC   OF RUFIB002  TO  FORP14-CAG-FORML.
ACT046     MOVE CCTA-CORR     OF RUFIB002  TO  FORP14-CCTA-FORML.
ACT046     MOVE CCART-RESP    OF RURCV037  TO  WRK-CCART5
ACT046     MOVE WRK-CCART3-R               TO  FORP14-CCART-FORML.
ACT046     MOVE 'RUEC'                     TO  FORP14-CCTRO-FORML.
ACT046     MOVE CCONTR        OF RUFIB002  TO  FORP14-CCONTR-FORML.
ACT046     MOVE VFINAN        OF RUFIB002  TO  FORP14-VCONTR-FORML.
ACT046     MOVE CCGC-CPF      OF RUECV014  TO  FORP14-CCPF-FORML.
ACT046     MOVE CFLIAL-CGC    OF RUECV014  TO  FORP14-CFLIAL-CPF.
ACT046     MOVE CCTRL-CPF-CGC OF RUECV014  TO  FORP14-CCTRL-CPF-FORML.
ACT046     MOVE 'FORP9120'                 TO  WRK-MODULO.

ACT046     MOVE ZEROS                      TO  FORP14-RC-RET.

ACT046     CALL WRK-MODULO USING       FORP14.

ACT046*----------------------------------------------------------------*
ACT046 3000-99-FIM. EXIT.

ACT046*----------------------------------------------------------------*
ACT046*    ACESSAR TABELA RUFIB002
ACT046*----------------------------------------------------------------*
ACT046 3010-ACESSAR-RUFIB002           SECTION.
ACT046*----------------------------------------------------------------*

ACT046     MOVE INP-AGENCIA-T916       TO  CJUNC-DEPDC     OF RUFIB002.
ACT046     MOVE INP-CEDULA-T916        TO  CCDULA          OF RUFIB002.

ACT046     EXEC SQL
ACT046          SELECT
ACT046                CCONTR,
ACT046                CJUNC_DEPDC,
ACT046                CCTA_CORR,
ACT046                CATVDD_RURAL,
ACT046                CFNALD_RURAL,
ACT046                CORIGE_REC,
ACT046                VFINAN
PW9931              , CREFT_RECOR
ACT046            INTO
ACT046                :RUFIB002.CJUNC-DEPDC,
ACT046                :RUFIB002.CCTA-CORR,
ACT046                :RUFIB002.CATVDD-RURAL
ACT046                     :WRK-CATVDD-RURAL-NULL,
ACT046                :RUFIB002.CFNALD-RURAL
ACT046                     :WRK-CFNALD-RURAL-NULL,
ACT046                :RUFIB002.CORIGE-REC
ACT046                     :WRK-CORIGE-REC-NULL,
ACT046                :RUFIB002.VFINAN
PW9931              , :RUFIB002.CREFT-RECOR
ACT046          FROM  DB2PRD.CONTRATO_RURAL
ACT046          WHERE CJUNC_DEPDC = :RUFIB002.CJUNC-DEPDC
ACT046            AND CCDULA      = :RUFIB002.CCDULA
PW2875            AND (CSTTUS_CONTR_RURAL > 0
PW2875            AND  CSTTUS_CONTR_RURAL < 8)
ACT046     END-EXEC.

ACT046     IF (SQLCODE                 NOT EQUAL ZEROS) OR
ACT046        (SQLWARN0                EQUAL 'W')
ACT046         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
ACT046         MOVE 'CONTRATO_RURAL'   TO  ERR-DBD-TAB
ACT046         MOVE SQLCODE            TO  ERR-SQL-CODE
ACT046         MOVE '0016'             TO  ERR-LOCAL
ACT046         MOVE SPACES             TO  ERR-SEGM
ACT046         PERFORM 9000-ROTINA-ERRO
ACT046     END-IF.

ACT046     IF  WRK-CFNALD-RURAL-NULL  LESS  ZEROS
ACT046         MOVE ZEROS              TO  CFNALD-RURAL OF RUFIB002
ACT046     END-IF.

ACT046     IF  WRK-CATVDD-RURAL-NULL  LESS  ZEROS
ACT046         MOVE ZEROS              TO  CATVDD-RURAL OF RUFIB002
ACT046     END-IF.

ACT046     IF  WRK-CORIGE-REC-NULL  LESS  ZEROS
ACT046         MOVE ZEROS              TO  CORIGE-REC OF RUFIB002
ACT046     END-IF.

ACT046*----------------------------------------------------------------*
ACT046 3010-99-FIM. EXIT.
ACT046*----------------------------------------------------------------*
ACT046*----------------------------------------------------------------*
ACT046*    ACESSAR TABELA RURCV040
ACT046*----------------------------------------------------------------*
ACT046 3020-ACESSAR-RURCV040           SECTION.
ACT046*----------------------------------------------------------------*

ACT046     MOVE CATVDD-RURAL OF  RUFIB002 TO CATVDD-RURAL OF RURCV040.
ACT046     MOVE CFNALD-RURAL OF  RUFIB002 TO CFNALD-RURAL OF RURCV040.
ACT046     MOVE CORIGE-REC   OF  RUFIB002 TO CORIGE-REC   OF RURCV040.

ACT046     EXEC SQL
ACT046        SELECT  CGRP_CTBIL_ATUAL,
ACT046                CSGRP_CTBIL
ACT046          INTO  :RURCV040.CGRP-CTBIL-ATUAL,
ACT046                :RURCV040.CSGRP-CTBIL
ACT046          FROM  DB2PRD.V01RAZMES_ORIGREC
ACT046         WHERE  CATVDD_RURAL     =  :RURCV040.CATVDD-RURAL  AND
ACT046                CFNALD_RURAL     =  :RURCV040.CFNALD-RURAL  AND
ACT046                CORIGE_REC       =  :RURCV040.CORIGE-REC
ACT046     END-EXEC.

ACT046        (SQLWARN0                EQUAL 'W')
ACT046         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
ACT046         MOVE 'V01RAZMES_ORIGREC' TO  ERR-DBD-TAB
ACT046         MOVE 'SELECT'           TO  ERR-FUN-COMANDO
ACT046         MOVE SQLCODE            TO  ERR-SQL-CODE
ACT046         MOVE '0017'             TO  ERR-LOCAL
ACT046         MOVE SPACES             TO  ERR-SEGM
ACT046         PERFORM 9000-ROTINA-ERRO
ACT046     END-IF.

ACT046*----------------------------------------------------------------*
ACT046 3020-99-FIM. EXIT.
ACT046*----------------------------------------------------------------*

ACT046*----------------------------------------------------------------*
ACT046*    ACESSAR TABELA RURCV037
ACT046*----------------------------------------------------------------*
ACT046 3030-ACESSAR-RURCV037           SECTION.
ACT046*----------------------------------------------------------------*

ACT046     MOVE CGRP-CTBIL-ATUAL  OF RURCV040 TO
ACT046     MOVE CSGRP-CTBIL       OF RURCV040 TO
ACT046                                CSGRP-CTBIL OF RURCV037

ACT046     EXEC SQL
ACT046         SELECT  CCART_RESP
ACT046           INTO  :RURCV037.CCART-RESP
ACT046           FROM  DB2PRD.V01RAZAO_MESTRE
ACT046          WHERE  CGRP_CTBIL_ATUAL  = :RURCV037.CGRP-CTBIL-ATUAL
ACT046            AND  CSGRP_CTBIL       = :RURCV037.CSGRP-CTBIL
ACT046     END-EXEC.

ACT046     IF (SQLCODE                 NOT EQUAL ZEROS) OR
ACT046        (SQLWARN0                EQUAL 'W')
ACT046         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
ACT046         MOVE 'V01RAZAO_MESTRE'  TO  ERR-DBD-TAB
ACT046         MOVE 'SELECT'           TO  ERR-FUN-COMANDO
ACT046         MOVE SQLCODE            TO  ERR-SQL-CODE
ACT046         MOVE '0018'             TO  ERR-LOCAL
ACT046         MOVE SPACES             TO  ERR-SEGM
ACT046         PERFORM 9000-ROTINA-ERRO
ACT046     END-IF.
ACT046*----------------------------------------------------------------*
ACT046 3030-99-FIM. EXIT.
ACT046*----------------------------------------------------------------*

ACT046*----------------------------------------------------------------*
ACT046*    IDENTIFICA CONTRATO INCLUIDO NO SISTEMA FORP
ACT046*----------------------------------------------------------------*
ACT046 3040-ATUALIZAR-RUECV007         SECTION.
ACT046*----------------------------------------------------------------*

ACT046     MOVE INP-AGENCIA-T916       TO CJUNC-DEPDC       OF RUECV007.
ACT046     MOVE INP-CEDULA-T916        TO CCDULA            OF RUECV007.
ACT046     MOVE 'I'                    TO CINDCD-CTRL-FORML OF RUECV007.

ACT046     EXEC SQL
ACT046         UPDATE  DB2PRD.CEDULA_TEMPR
ACT046            SET  CINDCD_CTRL_FORML = :RUECV007.CINDCD-CTRL-FORML
ACT046          WHERE  CJUNC_DEPDC       = :RUECV007.CJUNC-DEPDC
                  AND  CCDULA            = :RUECV007.CCDULA
ACT046     END-EXEC.

ACT046        (SQLWARN0                EQUAL 'W')
ACT046         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
ACT046         MOVE 'V01RAZAO_MESTRE'  TO  ERR-DBD-TAB
ACT046         MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
ACT046         MOVE SQLCODE            TO  ERR-SQL-CODE
ACT046         MOVE '0019'             TO  ERR-LOCAL
ACT046         MOVE SPACES             TO  ERR-SEGM
ACT046         PERFORM 9000-ROTINA-ERRO
ACT046     END-IF.

ACT046*----------------------------------------------------------------*
ACT046 3040-99-FIM. EXIT.
ACT046*----------------------------------------------------------------*

ACT046*----------------------------------------------------------------*
ACT046 3050-ACESSAR-MESU9015           SECTION.
ACT046*----------------------------------------------------------------*

ACT046     MOVE 237                    TO WRK-COMU-EMPR.
ACT046     MOVE INP-AGENCIA-T916       TO WRK-COMU-DEPDC.
ACT046     MOVE 'RUEC9160'             TO WRK-COMU-NOME-PGM.

ACT046     CALL WRK-MODULO             USING WRK-COMU-DADOS-9015
ACT046                                       WRK-COMU-POOL7100
ACT046                                       WRK-COMU-SQLCA.

ACT046     IF  WRK-COMU-COD-RETORNO    EQUAL 88
ACT046         MOVE '0020'                 TO  ERR-LOCAL
ACT046         MOVE WRK-COMU-POOL7100      TO  ERRO-AREA
ACT046         MOVE WRK-COMU-SQLCA         TO  SQLCA
ACT046         PERFORM 9000-ROTINA-ERRO
ACT046     END-IF.

ACT046     IF  WRK-COMU-COD-RETORNO    EQUAL  01
ACT046         MOVE ZEROS              TO FORP14-CDEPDC-DIR-RGNAL
ACT046                                    FORP14-CEMPR-INC-DIR
ACT046     END-IF.

ACT046     IF  WRK-COMU-COD-RETORNO    EQUAL ZEROS
ACT046         MOVE WRK-COMU-DIRETORIA-REG
ACT046                                 TO FORP14-CDEPDC-DIR-RGNAL
ACT046         MOVE WRK-COMU-EMPR      TO FORP14-CEMPR-INC-DIR

ACT046*----------------------------------------------------------------*
ACT046 3050-99-FIM. EXIT.
ACT046*----------------------------------------------------------------

ACT046*----------------------------------------------------------------*
ACT046 3060-ACESSAR-MESU9023           SECTION.
ACT046*----------------------------------------------------------------*

ACT046     MOVE 237                    TO WRK-COMU-EMPRESA-9023.
ACT046     MOVE 17                     TO WRK-COMU-TPO-RELAC-9023.
ACT046     MOVE INP-AGENCIA-T916       TO WRK-COMU-DEPDC-9023.
ACT046     MOVE 'RUEC9160'             TO WRK-COMU-NOME-PGM-9023.
ACT046     MOVE 'MESU9023'             TO WRK-MODULO.
ACT046     MOVE 'B'                    TO WRK-COMU-AMBIENTE-9023.

ACT046     CALL WRK-MODULO             USING WRK-COMU-AMBIENTE-9023
ACT046                                       WRK-COMU-IO-PCB-9023
ACT046                                       WRK-COMU-ALT-PCB-9023
ACT046                                       WRK-COMU-PARM-9023
ACT046                                       WRK-COMU-POOL7100-9023

ACT046     IF  WRK-COMU-COD-RETORNO-9023   EQUAL 88
ACT046         MOVE '0021'                 TO  ERR-LOCAL
ACT046         MOVE WRK-COMU-POOL7100-9023 TO  ERRO-AREA
ACT046         MOVE WRK-COMU-SQLCA-9023    TO  SQLCA
ACT046         PERFORM 9000-ROTINA-ERRO
ACT046     END-IF.

ACT046     IF  WRK-COMU-COD-RETORNO-9023   EQUAL 01
ACT046         MOVE ZEROS      TO  FORP14-CEMPR-INC-GERC
ACT046                             FORP14-CDEPDC-GERC-RGNAL
ACT046     END-IF.

ACT046     IF  WRK-COMU-COD-RETORNO-9023    EQUAL ZEROS
ACT046         MOVE WRK-COMU-EMPR-RELAC-9023
ACT046                            TO  FORP14-CEMPR-INC-GERC
ACT046         MOVE WRK-COMU-DEPTO-RELAC-9023
ACT046                            TO  FORP14-CDEPDC-GERC-RGNAL
ACT046     END-IF.

ACT046*----------------------------------------------------------------*
ACT046*----------------------------------------------------------------*

ACT046*----------------------------------------------------------------*
ACT046 3070-ACESSAR-RUECV014           SECTION.
ACT046*----------------------------------------------------------------*

ACT046     EXEC SQL
ACT046        SELECT  CCGC_CPF,
ACT046                CFLIAL_CGC,
ACT046                CCTRL_CPF_CGC
ACT046          INTO :RUECV014.CCGC-CPF,
ACT046               :RUECV014.CFLIAL-CGC,
ACT046               :RUECV014.CCTRL-CPF-CGC
ACT046          FROM  DB2PRD.V01PESSOA_RURAL
ACT046         WHERE  CCONTR          = :RUFIB002.CCONTR
ACT046           AND  CTPO_RESP_PSSOA = 'T'
ACT046     END-EXEC.

ACT046     IF (SQLCODE                 NOT EQUAL ZEROS) OR
ACT046        (SQLWARN0                EQUAL 'W')
ACT046         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
ACT046         MOVE 'SELECT'           TO  ERR-FUN-COMANDO
ACT046         MOVE SQLCODE            TO  ERR-SQL-CODE
ACT046         MOVE '0022'             TO  ERR-LOCAL
ACT046         MOVE SPACES             TO  ERR-SEGM
ACT046         PERFORM 9000-ROTINA-ERRO
ACT046     END-IF.

ACT046*----------------------------------------------------------------*
ACT046 3070-99-FIM. EXIT.
ACT046*----------------------------------------------------------------*

       150-00-ACESSA-RUECV019 SECTION.

           MOVE    INP-AGENCIA-T916        TO    CJUNC-DEPDC OF RUECV019
           MOVE    INP-CEDULA-T916         TO    CCDULA      OF RUECV019

           EXEC SQL
                SELECT RMSGEM_INCON
                INTO  :RUECV019.RMSGEM-INCON
                FROM   DB2PRD.V01INCON_CDULA
                WHERE
                       CCDULA      = :RUECV019.CCDULA
           END-EXEC.

           IF SQLCODE  NOT EQUAL ZEROS AND +100 AND -811 OR
              SQLWARN0     EQUAL 'W'
              MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
              MOVE   'V01INCON_CDULA   '  TO    ERR-DBD-TAB
              MOVE   'SELECT'              TO    ERR-FUN-COMANDO
              MOVE    SQLCODE              TO    ERR-SQL-CODE
              MOVE    0005                 TO    ERR-LOCAL
              MOVE    SPACES               TO    ERR-SEGM
              PERFORM 9000-ROTINA-ERRO.

       150-99-ACESSA-RUECV019. EXIT.

       160-00-ACESSA-RUECV007 SECTION.

           MOVE    INP-AGENCIA-T916        TO    CJUNC-DEPDC OF RUECV007
           MOVE    INP-CEDULA-T916         TO    CCDULA      OF RUECV007

           EXEC SQL
PW2875                 CATVDD_RURAL,
PW2875                 CFNALD_RURAL,
PW2875                 CPRODT_RURAL,
MAGA                   CORIGE_REC,
                       CPSSOA_CADTR,
                       CCONTR,
                       CGARTD,
PW2875                 CIMPRE_CDULA_TEMPR,
                       CSIT_CDULA_TEMPR,
PW7398                 DIMPRE_CONTR_RURAL,
POL097                 CFNALD_RURAL,
POL097                 CAPLIC_RURAL,
POL097                 CCLI_UNIC_BDSCO,
POL097                 DULT_VALDC_CDULA,
POL097                 CINDCD_FONE_RESID,
POL097                 CINDCD_FONE_COML,
POL097                 CINDCD_ENDER_RESID,
POL097                 CINDCD_ENDER_COML,
POL097                 CINDCD_QUALF_PROFS,
POL097                 QBLOCO_PATRM_IMOV,
POL097                 QBLOCO_PATRM_VEIC,
POL097                 QBLOCO_SEGUR_CLI,
POL097                 QBLOCO_QUADR_SCIAL
MF0113               , VFINAN
7C0325               , CDECLR_AGCLT_FAMLR
7C0325               , DVALDD_DECLR_FAMLR
PW2875          INTO  :RUECV007.DCDULA,
PW2875                :RUECV007.CATVDD-RURAL,
PW2875                :RUECV007.CFNALD-RURAL,
PW2875                :RUECV007.CPRODT-RURAL,
MAGA                  :RUECV007.CORIGE-REC,
                      :RUECV007.CPSSOA-CADTR,
                      :RUECV007.CCONTR:WRK-NULO-CCONTR,
                      :RUECV007.CGARTD,
PW2875                :RUECV007.CIMPRE-CDULA-TEMPR,
                      :RUECV007.CSIT-CDULA-TEMPR,
PW7398                :RUECV007.DIMPRE-CONTR-RURAL,
POL097                :RUECV007.CFNALD-RURAL,
POL097                :RUECV007.CAPLIC-RURAL,
POL097                :RUECV007.CCLI-UNIC-BDSCO,
POL097                :RUECV007.DULT-VALDC-CDULA,
POL097                :RUECV007.CINDCD-FONE-RESID,
POL097                :RUECV007.CINDCD-ENDER-RESID,
POL097                :RUECV007.CINDCD-ENDER-COML,
POL097                :RUECV007.CINDCD-QUALF-PROFS,
POL097                :RUECV007.QBLOCO-PATRM-IMOV,
POL097                :RUECV007.QBLOCO-PATRM-VEIC,
POL097                :RUECV007.QBLOCO-LSNG-CLI,
POL097                :RUECV007.QBLOCO-SEGUR-CLI,
POL097                :RUECV007.QBLOCO-QUADR-SCIAL
MF0113              , :RUECV007.VFINAN
7C0325              , :RUECV007.CDECLR-AGCLT-FAMLR:WRK-NULO-CDECLR-AGCLT
7C0325              , :RUECV007.DVALDD-DECLR-FAMLR:WRK-NULO-DVALDD-DECLR
                FROM   DB2PRD.V01CEDULA_TEMPR
                WHERE  CJUNC_DEPDC = :RUECV007.CJUNC-DEPDC AND
                       CCDULA      = :RUECV007.CCDULA
           END-EXEC.

MF0611     IF (SQLCODE  NOT EQUAL ZEROS AND +100)
MF0611     OR (SQLWARN0     EQUAL 'W')
              MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
              MOVE   'V01CEDULA_TEMPR  '  TO    ERR-DBD-TAB
              MOVE   'SELECT'              TO    ERR-FUN-COMANDO
              MOVE    0006                 TO    ERR-LOCAL
              MOVE    SPACES               TO    ERR-SEGM
              PERFORM 9000-ROTINA-ERRO
           ELSE
              IF  WRK-NULO-CCONTR         EQUAL -1
POL126            MOVE ZEROS TO CCONTR OF RUECV007
DK0611        END-IF
7C0325        IF  WRK-NULO-CDECLR-AGCLT EQUAL -1
.                 MOVE SPACES TO CDECLR-AGCLT-FAMLR OF RUECV007
.             END-IF
.             IF  WRK-NULO-DVALDD-DECLR EQUAL -1
.                 MOVE ZEROS  TO DVALDD-DECLR-FAMLR OF RUECV007
7C0325        END-IF
DK0611     END-IF.
       160-99-ACESSA-RUECV007. EXIT.

       170-00-OBTEM-IMPRESSORA SECTION.

           MOVE    INP-SENHAS-T916         TO    COMU-SENHAS

           CALL   'POOL1080'            USING    IO-PCB
                                                 COMU-IMPRESSORA.

       170-99-OBTEM-IMPRESSORA. EXIT.

       180-00-ACESSA-RUECB033 SECTION.

           MOVE    INP-AGENCIA-T916        TO    CJUNC-DEPDC OF RUECB033
           MOVE    INP-CEDULA-T916         TO    CCDULA      OF RUECB033

           EXEC SQL
                SELECT CINDCD_LOC_FIS,
                       DIMPRE
                INTO
                      :RUECB033.CINDCD-LOC-FIS,
                      :RUECB033.DIMPRE
                FROM
                       DB2PRD.CTRL_HIST_IMPRE
                WHERE
                       CJUNC_DEPDC = :RUECB033.CJUNC-DEPDC AND
                       CCDULA      = :RUECB033.CCDULA
           END-EXEC.
           IF SQLCODE  NOT EQUAL ZEROS AND +100  OR
              SQLWARN0     EQUAL 'W'
              MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
              MOVE   'CTRL_HIST_IMPRE   '  TO    ERR-DBD-TAB
              MOVE   'SELECT'              TO    ERR-FUN-COMANDO
              MOVE    SQLCODE              TO    ERR-SQL-CODE
              MOVE    0008                 TO    ERR-LOCAL
              MOVE    SPACES               TO    ERR-SEGM
              PERFORM 9000-ROTINA-ERRO.

           IF SQLCODE  EQUAL ZEROS
              MOVE   'S'                   TO    WRK-EXISTE-RUECB033.

       180-99-ACESSA-RUECB033. EXIT.

       181-00-ACESSA-RUECB034 SECTION.

           MOVE    INP-AGENCIA-T916        TO    CJUNC-DEPDC OF RUECB034
           MOVE    INP-CEDULA-T916         TO    CCDULA      OF RUECB034

           EXEC SQL
                        RLIN_MAX
                INTO
                      :RUECB034.CLIN-FIS-TEXTO,
                      :RUECB034.RLIN-MAX
                FROM    DB2PRD.HIST_IMPRE_CDULA
                WHERE
                       CJUNC_DEPDC = :RUECB034.CJUNC-DEPDC AND
                       CCDULA      = :RUECB034.CCDULA
PW7398            AND  CLIN_FIS_TEXTO > 0
           END-EXEC.

           IF (SQLCODE  NOT EQUAL ZEROS AND +100 AND -811)  OR
              SQLWARN0     EQUAL 'W'
              MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
              MOVE   'HIST_IMPRE_CDULA  '  TO    ERR-DBD-TAB
              MOVE   'SELECT'              TO    ERR-FUN-COMANDO
              MOVE    SQLCODE              TO    ERR-SQL-CODE
              MOVE    0008                 TO    ERR-LOCAL
              MOVE    SPACES               TO    ERR-SEGM
              PERFORM 9000-ROTINA-ERRO.

              MOVE   'S'                   TO    WRK-EXISTE-RUECB034.

PW7398     EXEC SQL
PW7398          SELECT  CLIN_FIS_TEXTO,
PW7398                  RLIN_MAX
PW7398          INTO
PW7398                :RUECB034.CLIN-FIS-TEXTO,
PW7398                :RUECB034.RLIN-MAX
PW7398          FROM    DB2PRD.HIST_IMPRE_CDULA
PW7398          WHERE
PW7398                 CJUNC_DEPDC = :RUECB034.CJUNC-DEPDC
PW7398            AND  CCDULA      = :RUECB034.CCDULA
PW7398            AND  CLIN_FIS_TEXTO < 0
PW7398     END-EXEC.

PW7398     IF (SQLCODE  NOT EQUAL ZEROS AND +100 AND -811)  OR
PW7398        SQLWARN0     EQUAL 'W'
PW7398        MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
PW7398        MOVE   'HIST_IMPRE_CDULA  '  TO    ERR-DBD-TAB
PW7298        MOVE   'SELECT'              TO    ERR-FUN-COMANDO
PW7398        MOVE    SQLCODE              TO    ERR-SQL-CODE
PW7398        MOVE    SPACES               TO    ERR-SEGM
PW7398        PERFORM 9000-ROTINA-ERRO
PW7398     END-IF.

PW7398     IF SQLCODE  EQUAL ZEROS OR -811
PW7398        MOVE   'S'                   TO    WRK-EXISTE-RUECB034-REC
PW7398     END-IF.

       181-99-ACESSA-RUECB034. EXIT.

       233-00-PASSA-CTRL-RUEC2020 SECTION.

           MOVE    'RUEC9160'           TO  COMU-TRANSACAO.
           MOVE    COMU-AREA-IMPRESSAO  TO  WRK-MENSAGEM.
           MOVE    'RUEC2020'           TO  WRK-TELA.
           MOVE    WRK-CHNG             TO  WRK-FUNCAO.

       233-99-FIM. EXIT.

       234-00-INSERT-RUECB042 SECTION.


           MOVE INP-AGENCIA-T916        TO  AGENCIA OF AREA-RUEC7000.
           MOVE INP-CEDULA-T916         TO  CCDULA  OF AREA-RUEC7000.

           MOVE INP-SENHAS-T916         TO  HOLD-8600-SENHA.
           MOVE INP-OPCAO-T916          TO  HOLD-8600-OPCAO.
           MOVE INP-AGENCIA-T916-X      TO  HOLD-8600-AGENCIA-X.
           MOVE INP-CEDULA-T916-X       TO  HOLD-8600-CEDULA-X.
           MOVE INP-IMP-PROPOSTA-T916   TO  HOLD-8600-VIA01A.
           MOVE INP-IMP-CEDULA-T916     TO  HOLD-8600-VIA01B.
RE0517     MOVE INP-IMP-CGEODE-T916     TO  HOLD-8600-VIA01G.
           MOVE INP-IMP-FICHA-T916      TO  HOLD-8600-VIA01C.
CET        MOVE INP-IMP-CET-T916        TO  HOLD-8600-VIA01H.
           MOVE INP-IMP-TODOS-T916      TO  HOLD-8600-VIA01D.
PW8123     IF INP-IMP-COMCZ-T916        EQUAL 'X'
PW8123        MOVE 'P'                  TO  HOLD-8600-VIA01E
PW8123     ELSE
              MOVE INP-IMP-ANEXOS-T916  TO  HOLD-8600-VIA01E
PW8123     END-IF.
           MOVE INP-IMP-OPERAC-T916     TO  HOLD-8600-VIA01F.
           MOVE INP-IMP-4VIAS-T916      TO  HOLD-8600-VIA04.
           MOVE INP-REIMPR-T916         TO  HOLD-8600-REIMPR.
           MOVE INP-CONF2-T916          TO  HOLD-8600-CONF2.
           MOVE OUT-MENSAG-T916         TO  HOLD-8600-MENSAG.
           MOVE NAVEGACAO-RURAL         TO  WNAVG-CREDT-RURAL-TEXT.
           MOVE 4000                    TO  WNAVG-CREDT-RURAL-LEN.

           EXEC SQL
             INSERT INTO DB2PRD.NAVEGACAO_RURAL
                  (CTERM,
                   WNAVG_CREDT_RURAL)
               VALUES
                  (:RUECB042.CTERM,
                   :RUECB042.WNAVG-CREDT-RURAL)
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS AND -803 )   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                   MOVE 'INSERT'             TO  ERR-FUN-COMANDO
                   MOVE 'NAVEGACAO_RURAL'    TO  ERR-DBD-TAB
                   MOVE SQLCODE              TO  ERR-SQL-CODE
                   MOVE SPACES               TO  ERR-SEGM
                   PERFORM 9000-ROTINA-ERRO.

       234-99-FIM. EXIT.

       235-00-UPDATE-RUECB042 SECTION.

           MOVE IO-LTERM                TO  CTERM OF RUECB042.

           MOVE INP-AGENCIA-T916        TO  AGENCIA OF AREA-RUEC7000.
           MOVE INP-CEDULA-T916         TO  CCDULA  OF AREA-RUEC7000.

           MOVE INP-SENHAS-T916         TO  HOLD-8600-SENHA.
           MOVE INP-OPCAO-T916          TO  HOLD-8600-OPCAO.
           MOVE INP-AGENCIA-T916-X      TO  HOLD-8600-AGENCIA-X.
           MOVE INP-CEDULA-T916-X       TO  HOLD-8600-CEDULA-X.
           MOVE INP-IMP-PROPOSTA-T916   TO  HOLD-8600-VIA01A.
           MOVE INP-IMP-CEDULA-T916     TO  HOLD-8600-VIA01B.
RE0517     MOVE INP-IMP-CGEODE-T916     TO  HOLD-8600-VIA01G.
           MOVE INP-IMP-FICHA-T916      TO  HOLD-8600-VIA01C.
CET        MOVE INP-IMP-CET-T916        TO  HOLD-8600-VIA01H.
PW8123     IF INP-IMP-COMCZ-T916        EQUAL 'X'
PW8123        MOVE 'P'                  TO  HOLD-8600-VIA01E
PW8123     ELSE
              MOVE INP-IMP-ANEXOS-T916  TO  HOLD-8600-VIA01E
PW8123     END-IF.
           MOVE INP-IMP-OPERAC-T916     TO  HOLD-8600-VIA01F.
           MOVE INP-IMP-4VIAS-T916      TO  HOLD-8600-VIA04.
           MOVE INP-CONF1-T916          TO  HOLD-8600-CONF1.
           MOVE INP-REIMPR-T916         TO  HOLD-8600-REIMPR.
           MOVE INP-CONF2-T916          TO  HOLD-8600-CONF2.
           MOVE OUT-MENSAG-T916         TO  HOLD-8600-MENSAG.
           MOVE NAVEGACAO-RURAL         TO  WNAVG-CREDT-RURAL-TEXT.
           MOVE 4000                    TO  WNAVG-CREDT-RURAL-LEN.

           EXEC SQL
                UPDATE DB2PRD.NAVEGACAO_RURAL
                   SET WNAVG_CREDT_RURAL = :RUECB042.WNAVG-CREDT-RURAL
                 WHERE CTERM             = :RUECB042.CTERM
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS )   OR
                   MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                   MOVE 'UPDATE'             TO  ERR-FUN-COMANDO
                   MOVE 'NAVEGACAO_RURAL'    TO  ERR-DBD-TAB
                   MOVE SQLCODE              TO  ERR-SQL-CODE
                   MOVE 0015                 TO  ERR-LOCAL
                   MOVE SPACES               TO  ERR-SEGM
                   PERFORM 9000-ROTINA-ERRO.

       235-99-FIM. EXIT.

       236-00-PASSA-CTRL-RUEC8600 SECTION.

           PERFORM 234-00-INSERT-RUECB042
           IF SQLCODE EQUAL -803
              PERFORM 235-00-UPDATE-RUECB042
           END-IF

           MOVE    'RUEC9160'              TO    COMU-TRANSACAO.

           MOVE    INP-SENHAS-T916         TO    COMU-SENHAS
           MOVE    INP-AGENCIA-T916        TO    COMU-AGENCIA
           MOVE    INP-IMP-PROPOSTA-T916   TO    COMU-IMP-PROPOSTA
           MOVE    INP-IMP-CEDULA-T916     TO    COMU-IMP-CEDULA
RE0517     MOVE    INP-IMP-CGEODE-T916     TO    COMU-IMP-CGEODE
CET        MOVE    INP-IMP-CET-T916        TO    COMU-IMP-CET
           MOVE    INP-IMP-FICHA-T916      TO    COMU-IMP-FICHA
PW8123     IF INP-IMP-COMCZ-T916           EQUAL 'X'
PW8123        MOVE    'P'                  TO    COMU-IMP-ANEXOS
PW8123     ELSE
              MOVE    INP-IMP-ANEXOS-T916  TO    COMU-IMP-ANEXOS
PW8123     END-IF.
           MOVE    INP-IMP-OPERAC-T916     TO    COMU-IMP-VIGOR-MORA

           IF  INP-IMP-TODOS-T916      EQUAL 'X'
               MOVE 'X'                    TO    COMU-IMP-PROPOSTA
               MOVE 'X'                    TO    COMU-IMP-CEDULA
RE0517         MOVE 'X'                    TO    COMU-IMP-CGEODE
               MOVE 'X'                    TO    COMU-IMP-FICHA
               MOVE 'X'                    TO    COMU-IMP-ANEXOS
               MOVE 'X'                    TO    COMU-IMP-VIGOR-MORA
CET            MOVE 'X'                    TO    COMU-IMP-CET
           END-IF.
           IF  INP-IMP-4VIAS-T916      EQUAL 'X'
               MOVE 'X'                    TO    COMU-IMP-PROPOSTA
               MOVE 'X'                    TO    COMU-IMP-CEDULA
RE0517         MOVE 'X'                    TO    COMU-IMP-CGEODE
               MOVE ' '                    TO    COMU-IMP-FICHA
               MOVE 'X'                    TO    COMU-IMP-ANEXOS
               MOVE ' '                    TO    COMU-IMP-VIGOR-MORA
               MOVE  2                     TO    COMU-TIPO-GRAVACAO
               MOVE  4                     TO    COMU-NRO-VIAS
           ELSE
               MOVE  3                     TO    COMU-TIPO-GRAVACAO
               MOVE  1                     TO    COMU-NRO-VIAS.

PW7398     IF  INP-IMP-4VIAS-T916      EQUAL 'R'
PW7398         MOVE 'X'                    TO    COMU-IMP-PROPOSTA
PW7398         MOVE 'X'                    TO    COMU-IMP-CEDULA
RE0517         MOVE 'X'                    TO    COMU-IMP-CGEODE
PW7398         MOVE ' '                    TO    COMU-IMP-FICHA
PW7398         MOVE 'X'                    TO    COMU-IMP-ANEXOS
PW7398         MOVE ' '                    TO    COMU-IMP-VIGOR-MORA
PW7398         MOVE '2'                    TO    COMU-TIPO-GRAVACAO
PW7398     END-IF.

           MOVE    'RUEC9160'              TO    COMU-PGM-RETORNO

           MOVE    COMU-AREA-IMPRESSAO     TO    WRK-MENSAGEM.

           MOVE    'RUEC8600'              TO    WRK-TELA.
           MOVE    WRK-CHNG                TO    WRK-FUNCAO.

       236-99-FIM. EXIT.

       237-00-VERIFICA-HISTORICO  SECTION.

      *---> A TABELA RUECB033 IDENTIFICA A LOCALIZACAO FISICA DOS DADOS
      *---> DE HISTORICO DE IMPRESSAO. CASO NAO EXISTAM DADOS NESTA TA-
      *---> BELA A TABELA RUECB034 (HISTORICO) TAMBEM DEVE ESTAR VAZIA.

      *---> REGISTRO ENCONTRADO NA TABELA RUECB033

      *---> SE CINDCD-LOC-FIS = 1 (CEDULA JA IMPRESSA EM 4 VIAS),
      *---> A IMAGEM DA CEDULA ESTA EM TABELA DB2 (RUECB034)
      *---> SE CINDCD-LOC-FIS = 2 (CEDULA JA IMPRESSA EM 4 VIAS),
      *---> A IMAGEM DA CEDULA ESTA EM BACK-UP

           PERFORM 180-00-ACESSA-RUECB033

           PERFORM 181-00-ACESSA-RUECB034

PW7398     IF INP-IMP-4VIAS-T916   EQUAL 'R'
PW7398         IF  WRK-EXISTE-RUECB034-REC  EQUAL  'N'
PW7398         AND WRK-EXISTE-RUECB034      EQUAL  'N'
PW7398             MOVE   WRK-M41          TO    OUT-MENSAG-T916
PW7398             GO  TO  237-99-FIM
PW7398         END-IF
PW7398         IF  WRK-EXISTE-RUECB034-REC  EQUAL  'S'
PW7398         AND WRK-EXISTE-RUECB034      EQUAL  'S'
PW7398         AND (WRK-EXISTE-RUECB033     EQUAL  'S'
PW7398         AND  CINDCD-LOC-FIS OF RUECB033 NOT EQUAL 1)
PW7398             MOVE   WRK-M43          TO    OUT-MENSAG-T916
PW7398             GO  TO  237-99-FIM
PW7398         END-IF
PW7398         GO  TO  237-99-FIM

           IF  WRK-EXISTE-RUECB033  EQUAL  'S' AND
               WRK-EXISTE-RUECB034  EQUAL  'N'
               IF CINDCD-LOC-FIS OF RUECB033 EQUAL 1
                  MOVE   WRK-M20          TO    OUT-MENSAG-T916
               ELSE
                  IF  INP-CONF1-T916  NOT EQUAL  SPACES AND LOW-VALUES
                      IF  INP-REIMPR-T916 EQUAL 'X'
                          PERFORM  240-00-UPDATE-RUECB033
                          MOVE   WRK-M23          TO  OUT-MENSAG-T916
                      ELSE
                          MOVE   WRK-M2           TO  OUT-MENSAG-T916
                      END-IF
                      MOVE   SPACES           TO  OUT-CONF1-T916
                      MOVE   SPACES           TO  OUT-CONF2-T916
                      MOVE   SPACES           TO  OUT-REIMPR-T916
                  ELSE
                      MOVE   DIMPRE OF RUECB033   TO    WRK-DATA-M14
                      INSPECT WRK-DATA-M14 REPLACING ALL '.' BY '/'
                      MOVE   WRK-M14       TO OUT-MENSAG-T916
                      MOVE   WRK-CONFIRM-1 TO OUT-CONF1-T916
                      MOVE   WRK-49345     TO OUT-REIMPR-ATTR-T916
                      MOVE   WRK-225       TO OUT-IMP-CEDULA-ATTR-T916
RE0517                                        OUT-IMP-CGEODE-ATTR-T916
                                              OUT-IMP-PROPOSTA-ATTR-T916
                                              OUT-IMP-FICHA-ATTR-T916
                                              OUT-IMP-ANEXOS-ATTR-T916
PW8123                                        OUT-IMP-COMCZ-ATTR-T916
                                              OUT-IMP-OPERAC-ATTR-T916
CET                                           OUT-IMP-CET-ATTR-T916
                                              OUT-IMP-TODOS-ATTR-T916
                                              OUT-IMP-4VIAS-ATTR-T916
           ELSE
               IF  WRK-EXISTE-RUECB033  EQUAL  'N' AND
                   WRK-EXISTE-RUECB034  EQUAL  'S'
                   MOVE   WRK-M21         TO    OUT-MENSAG-T916
               ELSE
                   IF  WRK-EXISTE-RUECB033  EQUAL  'S' AND
                       WRK-EXISTE-RUECB034  EQUAL  'S'
                       IF CINDCD-LOC-FIS OF RUECB033 EQUAL 2
                          MOVE   WRK-M22  TO    OUT-MENSAG-T916.


ACT181*------------------------------------------------------------*
ACT181 238-00-VERIFICAR-FIM-ANO         SECTION.
ACT181*------------------------------------------------------------*
ACT181
ACT181     MOVE SPACES                 TO  WRK-MENS01
ACT181
ACT181     MOVE  DT-AAAAMMDD           TO  WRK-DATA-AUX-R.
ACT181
ACT181     MOVE  WRK-DIA-AUX OF WRK-DATA-AUX  TO  DIA-C.
ACT181     MOVE  WRK-MES-AUX OF WRK-DATA-AUX  TO  MES-C.
ACT181     MOVE  WRK-ANO-AUX OF WRK-DATA-AUX  TO  ANO-C.
ACT181
ACT181     MOVE  31                    TO  DIA-F F-DIA.
ACT181     MOVE  12                    TO  MES-F F-MES.
ACT181     MOVE  WRK-ANO-AUX OF WRK-DATA-AUX  TO  ANO-F F-ANO.
ACT181     MOVE  WRK-AAAAMMDD-FIM      TO  WRK-1205-DT-ENVIADA
ACT181     MOVE ' '                    TO  WRK-1205-OPCAO
ACT181
ACT181     CALL 'POOL1205' USING  WRK-DADOS-POOL1205
ACT181                            WRK-1205-MENSAGEM.
ACT181     IF  RETURN-CODE NOT EQUAL ZEROS
ACT181         MOVE  WRK-1205-MENSAGEM        TO  WRK-MENS01
ACT181     ELSE
ACT181     IF (WRK-1205-OPCAO   EQUAL 'U')  AND
ACT181        (WRK-DDMMAAAA-COR EQUAL  WRK-DDMMAAAA-FIM)
ACT181         MOVE 'TRANSACAO INDISPONIVEL'  TO  WRK-MENS01
ACT181     ELSE
ACT181     IF (WRK-DDMMAAAA-COR EQUAL   WRK-1205-DT-UT-ANTER) AND
ACT181        (WRK-1205-OPCAO   NOT EQUAL 'U')
ACT181         MOVE 'TRANSACAO INDISPONIVEL'  TO  WRK-MENS01.
ACT181
ACT181*------------------------------------------------------------*
ACT181 238-00-EXIT.    EXIT.
ACT181*------------------------------------------------------------*


       240-00-UPDATE-RUECB033   SECTION.

           MOVE INP-AGENCIA-T916    TO   CJUNC-DEPDC OF RUECB033.
           MOVE INP-CEDULA-T916     TO   CCDULA      OF RUECB033.
           MOVE 3                   TO   CINDCD-LOC-FIS OF RUECB033.
           EXEC SQL
                UPDATE DB2PRD.CTRL_HIST_IMPRE
                SET    CINDCD_LOC_FIS    = :RUECB033.CINDCD-LOC-FIS
                WHERE  CJUNC_DEPDC       = :RUECB033.CJUNC-DEPDC AND
                       CCDULA            = :RUECB033.CCDULA
           END-EXEC.

           IF SQLCODE  NOT EQUAL ZEROS     OR
              SQLWARN0     EQUAL 'W'
              MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
              MOVE   'CTRL_HIST_IMPRE   '  TO    ERR-DBD-TAB
              MOVE   'UPDATE'              TO    ERR-FUN-COMANDO
              MOVE    SQLCODE              TO    ERR-SQL-CODE
              MOVE    0011                 TO    ERR-LOCAL
              MOVE    SPACES               TO    ERR-SEGM
              PERFORM 9000-ROTINA-ERRO.

       240-99-FIM. EXIT.

       245-00-DELETE-RUECB042  SECTION.


           EXEC SQL
                DELETE FROM  DB2PRD.NAVEGACAO_RURAL
                 WHERE CTERM = :RUECB042.CTERM
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS AND +100)   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                   MOVE 'DELETE'             TO  ERR-FUN-COMANDO
                   MOVE 'NAVEGACAO_RURAL'    TO  ERR-DBD-TAB
                   MOVE SQLCODE              TO  ERR-SQL-CODE
                   MOVE 0245                 TO  ERR-LOCAL
                   MOVE SPACES               TO  ERR-SEGM
                   PERFORM 9000-ROTINA-ERRO.

       245-99-FIM. EXIT.

      *------------------------------------*
POL097 470-GERA-HISTORICO  SECTION.
      *------------------------------------*
POL097     IF  COMU-TIPO-GRAVACAO  NOT  EQUAL  2
POL097         GO  TO  470-EXIT
POL097     END-IF.

POL097     MOVE COMU-AGENCIA                TO INP-AGENCIA-T916
POL097     MOVE COMU-CEDULA                 TO INP-CEDULA-T916

POL097     PERFORM  160-00-ACESSA-RUECV007.

POL097     INITIALIZE  R35-AREA-R7728.
POL097     MOVE 'RUEC9160'                  TO R35-PGMCHMD-R7728.
POL097     MOVE 'G'                         TO R35-FUNCAO-R7728.
POL097     MOVE CCONTR OF RUECV007          TO R35-CCONTR-R7728.
POL097     MOVE CCLI-UNIC-BDSCO OF RUECV007 TO R35-CCLUB-R7728.

POL097     CALL  WRK-RUEC7728 USING R35-AREA-R7728
POL097                              IO-PCB
POL097                              ALT-PCB.

POL097     IF  R35-RET-CODE-R7728  EQUAL  99
POL097         MOVE R35-ERRO-AREA-R7728     TO ERRO-AREA
POL097         PERFORM 9000-ROTINA-ERRO
POL097     END-IF.

POL097 470-EXIT.
           EXIT.

PW9931*------------------------------------------------------------*
PW9931 4890-VER-HORARIO-BACEN      SECTION.
PW9931*------------------------------------------------------------*
PW9931
PW9931     EXEC SQL
PW9931          SELECT VINFER_PARM_RURAL
PW9931               , VSUPE_PARM_RURAL
PW9931          INTO   :RUECV016.VINFER-PARM-RURAL
PW9931               , :RUECV016.VSUPE-PARM-RURAL
PW9931                   :WRK-VSUPE-PARM-RURAL
PW9931          FROM   DB2PRD.V01PARAMETRO_RURAL
PW9931          WHERE  CPARM_RURAL  =  'I4DUT'
PW9931     END-EXEC.

PW9931     IF SQLCODE  NOT EQUAL ZEROS     OR
PW9931        MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
PW9931        MOVE   'PARAMETRO_RURAL   '  TO    ERR-DBD-TAB
PW9931        MOVE   'SELECT'              TO    ERR-FUN-COMANDO
PW9931        MOVE    SQLCODE              TO    ERR-SQL-CODE
PW9931        MOVE    '011C'               TO    ERR-LOCAL
PW9931        MOVE    SPACES               TO    ERR-SEGM
PW9931        PERFORM 9000-ROTINA-ERRO
PW9931     END-IF.

           IF WRK-VSUPE-PARM-RURAL LESS ZEROS
              MOVE ZEROS                 TO VSUPE-PARM-RURAL OF RUECV016
           END-IF.

PW9931     MOVE SPACES                 TO  WRK-MENS01
PW9931     MOVE  DT-AAAAMMDD           TO  WRK-1205-DT-ENVIADA
PW9931     MOVE ' '                    TO  WRK-1205-OPCAO
PW9931
PW9931     CALL 'POOL1205' USING  WRK-DADOS-POOL1205
PW9931                            WRK-1205-MENSAGEM.
PW9931
PW9931     IF  RETURN-CODE NOT EQUAL ZEROS
PW9931     ELSE
PW9931         IF  (WRK-1205-OPCAO   NOT EQUAL 'U')
PW9931         AND (VINFER-PARM-RURAL OF RUECV016  EQUAL  1,00)
PW9931             MOVE 'IMPRES. 4 VIAS PERMITIDO SOMENTE EM DIAS UTEIS'
PW9931                                        TO  WRK-MENS01
PW9931             GO  TO  4890-99-FIM
PW9931         END-IF
PW9931     END-IF.

PW9931     EXEC SQL
PW9931          SELECT VINFER_PARM_RURAL
PW9931               , VSUPE_PARM_RURAL
PW9931          INTO   :RUECV016.VINFER-PARM-RURAL
PW9931               , :RUECV016.VSUPE-PARM-RURAL
PW9931                   :WRK-VSUPE-PARM-RURAL
PW9931          FROM   DB2PRD.V01PARAMETRO_RURAL
PW9931          WHERE  CPARM_RURAL  =  'IMP4V'
PW9931     END-EXEC.

PW9931     IF SQLCODE  NOT EQUAL ZEROS     OR
PW9931        SQLWARN0     EQUAL 'W'
PW9931        MOVE   'PARAMETRO_RURAL   '  TO    ERR-DBD-TAB
PW9931        MOVE   'SELECT'              TO    ERR-FUN-COMANDO
PW9931        MOVE    SQLCODE              TO    ERR-SQL-CODE
PW9931        MOVE    '011D'               TO    ERR-LOCAL
PW9931        MOVE    SPACES               TO    ERR-SEGM
PW9931        PERFORM 9000-ROTINA-ERRO
PW9931     END-IF.

           IF WRK-VSUPE-PARM-RURAL LESS ZEROS
              MOVE ZEROS                 TO VSUPE-PARM-RURAL OF RUECV016
           END-IF.

PW9931     MOVE  VINFER-PARM-RURAL OF RUECV016  TO  WRK-HRMIN-4VIAS
PW9931     MOVE  VSUPE-PARM-RURAL  OF RUECV016  TO  WRK-HRMAX-4VIAS

PW9931     IF  TI-HHMMSS  LESS  WRK-HMSMIN-4VIAS
PW9931     OR  TI-HHMMSS  GREATER  WRK-HMSMAX-4VIAS
PW9931         MOVE 'HORARIO NAO PERMITIDO P/ IMPRESSAO 4 VIAS'
PW9931                                      TO  WRK-MENS01
PW9931         GO  TO  4890-99-FIM
PW9931     END-IF.
      *------------------------------------------------------------*
PW9931 4890-99-FIM.    EXIT.
      *------------------------------------------------------------*

PW9931*------------------------------------------------------------*
PW9931 4892-VER-RUEC7764           SECTION.
PW9931*------------------------------------------------------------*

PW9931     MOVE  ZEROS               TO WRK-QT-PROPRIET.
PW9931     EXEC SQL
PW9931       SELECT COUNT(*)
PW9931       INTO :WRK-QT-PROPRIET
PW9931          FROM  DB2PRD.V01PESSOA_RURAL
PW9931         WHERE  CCONTR          = :RUECV007.CCONTR
PW9931           AND  CTPO_RESP_PSSOA = 'P'
PW9931     END-EXEC.

PW9931     IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
PW9931        (SQLWARN0                EQUAL 'W')
PW9931        MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
PW9931        MOVE   'PESSOA_RURAL      '  TO    ERR-DBD-TAB
PW9931        MOVE    SQLCODE              TO    ERR-SQL-CODE
PW9931        MOVE    '011F'               TO    ERR-LOCAL
PW9931        MOVE    SPACES               TO    ERR-SEGM
PW9931        PERFORM 9000-ROTINA-ERRO
PW9931     END-IF.

PW9931     IF  WRK-QT-PROPRIET  EQUAL  ZEROS
PW9931         INITIALIZE  RUEC40-AREA
PW9931         MOVE  'RUEC9160'       TO  RUEC40-PGM-CHAMADOR
PW9931         MOVE  INP-AGENCIA-T916 TO  RUEC40-AGENCIA
PW9931         MOVE  INP-CEDULA-T916  TO  RUEC40-CEDULA
PW9931         MOVE  'RUEC7764'       TO  WRK-MODULO
PW9931         CALL  WRK-MODULO  USING RUEC40-AREA
PW9931                                 IO-PCB
PW9931                                 ALT-PCB
PW9931         EVALUATE RUEC40-COD-RET
PW9931             WHEN 01
PW9931                 MOVE WRK-M57     TO    WRK-MENS01
PW9931                 GO TO 4892-99-FIM
PW9931             WHEN 99
PW9931                 MOVE RUEC40-SQLCA TO   SQLCA
PW9931                 PERFORM 9000-ROTINA-ERRO
PW9931                 GO TO 4892-99-FIM
PW9931         END-EVALUATE
PW9931     END-IF.

      *------------------------------------------------------------*
PW9931 4892-99-FIM.    EXIT.
      *------------------------------------------------------------*

PW9931*------------------------------------------------------------*
PW9931 4894-TRATAR-RECOR           SECTION.
PW9931*------------------------------------------------------------*

BR1217     IF  CIMPRE-CDULA-TEMPR  EQUAL  0
BR1217         IF INP-CTRBAC-T916        EQUAL SPACES
BR1217            MOVE 'S'               TO WRK-RCOR
BR1217            PERFORM 6000-OBTER-RESP
BR1217            IF WRK-COD-RET         EQUAL ZEROS
BR1217               GO TO 4894-TRATAR-RECOR
BR1217            END-IF
BR1217         ELSE
BR1217            IF SQLCODE           EQUAL +100
BR1217               GO TO 4894-99-FIM
BR1217            END-IF
BR1217            IF WRK-RCOR          EQUAL 'N'
BR1217               GO TO 4894-99-FIM
BR1217           END-IF
BR1217         END-IF
BR1217     END-IF.

PW9931     IF  CIMPRE-CDULA-TEMPR  EQUAL  0
PW9931         INITIALIZE  RUEC62-AREA
PW9931         MOVE  'RUEC9160'       TO  RUEC62-PGM-CHAMADOR
PW9931         MOVE  CCONTR OF RUFIB002   TO  RUEC62-CCONTR
PW9931         MOVE  WRK-COD-USER         TO  RUEC62-USER
PW9931         MOVE  'O'                  TO  RUEC62-PROCESS
PW9931         MOVE  'RUEC8808'       TO  WRK-MODULO
PW9931         CALL  WRK-MODULO  USING RUEC62-AREA
PW9931                                 IO-PCB
PW9931                                 ALT-PCB
PW9931         EVALUATE RUEC62-COD-RET
PW9931             WHEN 01 THRU 98
PW9931                 GO TO 4894-99-FIM
PW9931             WHEN 99
PW9931                 MOVE RUEC62-SQLCA TO   SQLCA
PW9931                 MOVE RUEC62-ERRO-AREA TO ERRO-AREA
PW9931                 PERFORM 9000-ROTINA-ERRO
PW9931                 GO TO 4894-99-FIM
PW9931         END-EVALUATE
PW9931     END-IF.

PW9931     IF  CIMPRE-CDULA-TEMPR  EQUAL  0
PW9931         INITIALIZE  RUEC65-AREA
PW9931         MOVE  'RUEC9160'       TO  RUEC65-PGM-CHAMADOR
PW9931         MOVE  CCONTR OF RUFIB002   TO  RUEC65-CCONTR
PW9931         MOVE  RUEC62-NENVIO-RECOR  TO  RUEC65-NENVIO-RECOR
PW9931         MOVE  'F'              TO  RUEC65-FUNCAO
EG0517         PERFORM 5000-CHAMAR-RUEC8810
EG0517         IF  RUEC65-COD-RET      NOT EQUAL ZEROS AND 15
EG0517             GO TO 4894-99-FIM
EG0517         END-IF
PW9931     END-IF.

PW9931         GO  TO  4894-99-FIM
PW9931     END-IF.

PW9931     MOVE ZEROS              TO NENVIO-RECOR       OF RUECB09B.
PW9931     MOVE CCONTR OF RUFIB002 TO CCONTR-CREDT-RURAL OF RUECB09B.
PW9931     EXEC SQL
PW9931       SELECT  MAX (NENVIO_RECOR)
PW9931         INTO :RUECB09B.NENVIO-RECOR
PW9931       FROM  DB2PRD.THIST_ENVIO_RECOR
PW9931         WHERE CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
PW9931     END-EXEC.

PW9931     IF SQLCODE  NOT EQUAL ZEROS     OR
PW9931        SQLWARN0     EQUAL 'W'
PW9931        MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
PW9931        MOVE   'THIST_ENVIO_RECOR '  TO    ERR-DBD-TAB
PW9931        MOVE   'SELECT'              TO    ERR-FUN-COMANDO
PW9931        MOVE    SQLCODE              TO    ERR-SQL-CODE
PW9931        MOVE   '011X'                TO    ERR-LOCAL
PW9931        MOVE    SPACES               TO    ERR-SEGM
PW9931        PERFORM 9000-ROTINA-ERRO

PW9931     EXEC SQL
PW9931       SELECT  CSIT_MSGEM
PW9931         INTO :RUECB09B.CSIT-MSGEM :WRK-CSIT-MSGEM-NULL
PW9931       FROM  DB2PRD.THIST_ENVIO_RECOR
PW9931         WHERE CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
PW9931           AND NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
PW9931     END-EXEC.

PW9931     IF (SQLCODE  NOT EQUAL ZEROS)     OR
PW9931        (SQLWARN0     EQUAL 'W')
PW9931        MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
PW9931        MOVE   'THIST_ENVIO_RECOR '  TO    ERR-DBD-TAB
PW9931        MOVE   'SELECT'              TO    ERR-FUN-COMANDO
PW9931        MOVE    SQLCODE              TO    ERR-SQL-CODE
PW9931        MOVE   '011Y'                TO    ERR-LOCAL
PW9931        MOVE    SPACES               TO    ERR-SEGM
PW9931        PERFORM 9000-ROTINA-ERRO
PW9931     END-IF.

PW9931     IF  WRK-CSIT-MSGEM-NULL  LESS  ZEROS
PW9931     END-IF.

PW9931     IF  CSIT-MSGEM  OF RUECB09B EQUAL  1
PW9931     OR  INP-IMP-4VIAS-T916  EQUAL  'B'
PW9931         INITIALIZE  RUEC65-AREA
PW9931         MOVE  'RUEC9160'       TO  RUEC65-PGM-CHAMADOR
PW9931         MOVE  CCONTR OF RUFIB002   TO  RUEC65-CCONTR
PW9931         MOVE  NENVIO-RECOR OF RUECB09B TO  RUEC65-NENVIO-RECOR
PW9931         MOVE  'R'              TO  RUEC65-FUNCAO
EG0517         PERFORM 5000-CHAMAR-RUEC8810
EG0517         IF  RUEC65-COD-RET      NOT EQUAL ZEROS AND 15
EG0517             GO TO 4894-99-FIM
EG0517         END-IF
PW9931     END-IF.

BS0250     IF  CSIT-MSGEM  OF RUECB09B EQUAL  2 OR 6
PW9931         MOVE WRK-M63     TO    WRK-MENS01
PW9931         GO TO 4894-99-FIM
PW9931     END-IF.

BS0250     IF  CSIT-MSGEM  OF RUECB09B EQUAL  7 OR 8
BS0250         GO TO 4894-99-FIM
BS0250     END-IF.

PW9931     IF  CSIT-MSGEM  OF RUECB09B EQUAL  3
PW9931         MOVE WRK-M64     TO    WRK-MENS01
PW9931         GO TO 4894-99-FIM
PW9931     END-IF.

BS0250     IF  CSIT-MSGEM  OF RUECB09B
BS0250                         NOT  EQUAL  4 AND 5 AND 6 AND 7 AND 8
PW9931         MOVE WRK-M65     TO    WRK-MENS01
PW9931     END-IF.

      *------------------------------------------------------------*
PW9931 4894-99-FIM.    EXIT.
      *------------------------------------------------------------*

MF0113*------------------------------------------------------------*
MF0113 4900-SOMA-PARCELA           SECTION.
MF0113*------------------------------------------------------------*

MF0113     MOVE 6000                    TO WRK-EVNTO-INI.
MF0113     MOVE 6999                    TO WRK-EVNTO-FIM.
MF0113     MOVE INP-AGENCIA-T916        TO CJUNC-DEPDC OF RUECV010
MF0113     MOVE INP-CEDULA-T916         TO CCDULA      OF RUECV010

MF0113     EXEC SQL
MF0113       SELECT  SUM(VEVNTO_RURAL)
MF0113         INTO :WRK-TOT-PARC
MF0113       FROM  DB2PRD.V01EVNTO_CDULA
MF0113         WHERE CCDULA             = :RUECV010.CCDULA
MF0113           AND CJUNC_DEPDC        = :RUECV010.CJUNC-DEPDC
MF0113           AND CTPO_EVNTO_RURAL  BETWEEN  :WRK-EVNTO-INI
MF0113                                     AND  :WRK-EVNTO-FIM
MF0113     END-EXEC.

MF0113     IF (SQLCODE  NOT EQUAL ZEROS AND +100 AND -305)     OR
MF0113        (SQLWARN0     EQUAL 'W')
MF0113        MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
MF0113        MOVE   'EVNTO_CDULA       '  TO    ERR-DBD-TAB
MF0113        MOVE   'SELECT'              TO    ERR-FUN-COMANDO
MF0113        MOVE    SQLCODE              TO    ERR-SQL-CODE
MF0113        MOVE    SPACES               TO    ERR-SEGM
MF0113        PERFORM 9000-ROTINA-ERRO
MF0113     END-IF.

      *------------------------------------------------------------*
MF0113 4900-99-FIM.    EXIT.
      *------------------------------------------------------------*

MF0113*------------------------------------------------------------*
MF0113 4910-SOMA-LIBERACAO         SECTION.
MF0113*------------------------------------------------------------*

MF0113     MOVE ZEROS                   TO WRK-TOT-LIBER.
MF0113     MOVE 2000                    TO WRK-EVNTO-INI.
MF0113     MOVE 2999                    TO WRK-EVNTO-FIM.
MF0113     MOVE INP-AGENCIA-T916        TO CJUNC-DEPDC OF RUECV010
MF0113     MOVE INP-CEDULA-T916         TO CCDULA      OF RUECV010

MF0113     EXEC SQL
MF0113       SELECT  SUM(VEVNTO_RURAL)
MF0113         INTO :WRK-TOT-LIBER
MF0113         WHERE CCDULA             = :RUECV010.CCDULA
MF0113           AND CJUNC_DEPDC        = :RUECV010.CJUNC-DEPDC
MF0113           AND CTPO_EVNTO_RURAL  BETWEEN  :WRK-EVNTO-INI
MF0113                                     AND  :WRK-EVNTO-FIM
MF0113     END-EXEC.

MF0113     IF (SQLCODE  NOT EQUAL ZEROS AND +100 AND -305)     OR
MF0113        (SQLWARN0     EQUAL 'W')
MF0113        MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
MF0113        MOVE   'EVNTO_CDULA       '  TO    ERR-DBD-TAB
MF0113        MOVE   'SELECT'              TO    ERR-FUN-COMANDO
MF0113        MOVE    SQLCODE              TO    ERR-SQL-CODE
MF0113        MOVE   '011Y'                TO    ERR-LOCAL
MF0113        MOVE    SPACES               TO    ERR-SEGM
MF0113        PERFORM 9000-ROTINA-ERRO
MF0113     END-IF.

      *------------------------------------------------------------*
MF0113 4910-99-FIM.    EXIT.
      *------------------------------------------------------------*

EG1215 4920-COORDENADA-OBRIGATORIA     SECTION.
EG1215*------------------------------------------------------------*
EG1215
EG1215     INITIALIZE RUECWGEO-REGISTRO.
EG1215
EG1215     MOVE INP-AGENCIA-T916       TO RUECWGEO-E-CJUNC-DEPDC.
EG1215     MOVE INP-CEDULA-T916        TO RUECWGEO-E-CCDULA.
EG1215
EG1215     MOVE 'RUEC7941'             TO WRK-RUEC7941
EG1215     CALL WRK-RUEC7941           USING RUECWGEO-COPYBOOK
EG1215                                       ERRO-AREA
EG1215                                       SQLCA.
EG1215
EG1215     IF  RUECWGEO-COD-RETORNO    NOT EQUAL ZEROS
EG1215         MOVE 'N'                TO RUECWGEO-COORD-OBRIG
EG1215     END-IF.
EG1215
EG1215*------------------------------------------------------------*
EG1215 4920-99-FIM.                    EXIT.
EG1215*------------------------------------------------------------*
EG1215
EG1215 4921-CONSISTIR-COORDENADAS      SECTION.
EG1215*------------------------------------------------------------*
EG1215
EG1215     MOVE SPACES                 TO WRK-COORD-INCONSIS.
EG1215
EG1215     MOVE CCONTR OF RUECV007     TO CCONTR OF RUECB040.
EG1215
EG1215     PERFORM 4922-OPEN-CSR01-RUECB040.
EG1215
EG1215     PERFORM 4923-FETCH-CSR01-RUECB040.
EG1215
EG1215     PERFORM 4925-TRATAR-CURSOR
EG1215     UNTIL   WRK-FIM-RUECB040    EQUAL 'S'
EG1215     OR      WRK-COORD-INCONSIS  EQUAL 'S'.
EG1215
EG1215     PERFORM 4924-CLOSE-CSR01-RUECB040.
EG1215
EG1215*------------------------------------------------------------*
EG1215 4921-99-FIM.                    EXIT.
EG1215*------------------------------------------------------------*
EG1215
EG1215 4922-OPEN-CSR01-RUECB040        SECTION.
EG1215*------------------------------------------------------------*
EG1215
EG1215     EXEC SQL
EG1215       OPEN CSR01-RUECB040
EG1215     END-EXEC.
EG1215
EG1215     IF (SQLCODE                 NOT EQUAL ZEROS )
EG1215     OR (SQLWARN0                EQUAL 'W'       )
EG1215         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG1215         MOVE 'RUECB040'         TO ERR-DBD-TAB
EG1215         MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
EG1215         MOVE 'OPEN'             TO ERR-FUN-COMANDO
EG1215         MOVE SQLCODE            TO ERR-SQL-CODE
EG1215         MOVE '4922'             TO ERR-LOCAL
EG1215         MOVE SPACES             TO ERR-SEGM
EG1215         PERFORM 9000-ROTINA-ERRO
EG1215     END-IF.
EG1215
EG1215     MOVE 'N'                    TO WRK-FIM-RUECB040.
EG1215
EG1215 4922-99-FIM.                    EXIT.
EG1215*------------------------------------------------------------*
EG1215
EG1215*------------------------------------------------------------*
EG1215 4923-FETCH-CSR01-RUECB040       SECTION.
EG1215*------------------------------------------------------------*
EG1215
EG1215     EXEC SQL
EG1215       FETCH CSR01-RUECB040
EG1215       INTO
EG1215         :RUECB040.CCONTR
EG1215       , :RUECB040.CSEQ-MATR-RURAL
EG1215       , :RUECB040.CINDCD-AREA-CIRC
EG1215     END-EXEC.
EG1215
EG1215     IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
EG1215     OR (SQLWARN0                EQUAL 'W'               )
EG1215         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG1215         MOVE 'RUECB040'         TO ERR-DBD-TAB
EG1215         MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
EG1215         MOVE 'FETCH'            TO ERR-FUN-COMANDO
EG1215         MOVE '4923'             TO ERR-LOCAL
EG1215         MOVE SPACES             TO ERR-SEGM
EG1215         PERFORM 9000-ROTINA-ERRO
EG1215     END-IF.
EG1215
EG1215     IF  SQLCODE                 EQUAL +100
EG1215         MOVE 'S'                TO WRK-FIM-RUECB040
EG1215     END-IF.
EG1215
EG1215*------------------------------------------------------------*
EG1215 4923-99-FIM.                    EXIT.
EG1215*------------------------------------------------------------*
EG1215
EG1215*------------------------------------------------------------*
EG1215 4924-CLOSE-CSR01-RUECB040       SECTION.
EG1215*------------------------------------------------------------*
EG1215
EG1215     EXEC SQL
EG1215       CLOSE CSR01-RUECB040
EG1215     END-EXEC.
EG1215
EG1215     OR (SQLWARN0                EQUAL 'W'                )
EG1215         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG1215         MOVE 'RUECB040'         TO ERR-DBD-TAB
EG1215         MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
EG1215         MOVE 'CLOSE'            TO ERR-FUN-COMANDO
EG1215         MOVE SQLCODE            TO ERR-SQL-CODE
EG1215         MOVE '4924'             TO ERR-LOCAL
EG1215         MOVE SPACES             TO ERR-SEGM
EG1215         PERFORM 9000-ROTINA-ERRO
EG1215     END-IF.
EG1215
EG1215*------------------------------------------------------------*
EG1215 4924-99-FIM.                    EXIT.
EG1215*------------------------------------------------------------*
EG1215
EG1215*------------------------------------------------------------*
EG1215 4925-TRATAR-CURSOR              SECTION.
EG1215*------------------------------------------------------------*
EG1215
EG1215     PERFORM 4926-SELECT-COUNT-RUECB09N.
EG1215
EG1215                                 EQUAL ZEROS
EG1215         MOVE 'COORDENADAS GEODESICAS NAO CADASTRADAS OU INCOMPLET
EG1215-             'AS'               TO OUT-MENSAG-T916
EG1215         MOVE 'S'                TO WRK-COORD-INCONSIS
EG1215         GO TO 4925-99-FIM
EG1215     END-IF.
EG1215
EG1215     IF  CINDCD-AREA-CIRC        OF RUECB040
EG1215                                 EQUAL 1
EG1215     AND WRK-QTDE-COORDENADAS    LESS      3
EG1215         MOVE 'QUANTIDADE DE COORDENADAS INVALIDA CADASTRADA EM IM
EG1215-             'OVEL COM AREA CIRCULAR'
EG1215                                 TO OUT-MENSAG-T916
EG1215         MOVE 'S'                TO WRK-COORD-INCONSIS
EG1215         GO TO 4925-99-FIM
EG1215     END-IF.
EG1215
EG1215     IF  CINDCD-AREA-CIRC        OF RUECB040
EG1215                                 EQUAL 2
EG1215     AND WRK-QTDE-COORDENADAS    LESS  3
EG1215         MOVE 'IMOVEL DE AREA NAO CIRCULAR COM MENOS DE TRES COORD
EG1215                                 TO OUT-MENSAG-T916
EG1215         MOVE 'S'                TO WRK-COORD-INCONSIS
EG1215     END-IF.
EG1215
EG1215     PERFORM 4923-FETCH-CSR01-RUECB040.
EG1215
EG1215*------------------------------------------------------------*
EG1215 4925-99-FIM.                    EXIT.
EG1215*------------------------------------------------------------*
EG1215
EG1215*------------------------------------------------------------*
EG1215 4926-SELECT-COUNT-RUECB09N      SECTION.
EG1215*------------------------------------------------------------*
EG1215
EG1215     MOVE CCONTR                 OF RUECB040
EG1215                                 TO CCONTR          OF RUECB09N.
EG1215     MOVE CSEQ-MATR-RURAL        OF RUECB040
EG1215                                 TO CSEQ-MATR-RURAL OF RUECB09N.
EG1215
EG1215     EXEC SQL
EG1215       SELECT
EG1215       INTO
EG1215         :WRK-QTDE-COORDENADAS
EG1215       FROM  DB2PRD.TCORND_GEOGR_CONTR
EG1215       WHERE CCONTR              = :RUECB09N.CCONTR
EG1215       AND   CSEQ_MATR_RURAL     = :RUECB09N.CSEQ-MATR-RURAL
EG1215     END-EXEC.
EG1215
EG1215     IF (SQLCODE                 NOT EQUAL ZEROS )
EG1215     OR (SQLWARN0                EQUAL 'W'       )
EG1215         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG1215         MOVE 'RUECB09M'         TO ERR-DBD-TAB
EG1215         MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
EG1215         MOVE 'SELECT'           TO ERR-FUN-COMANDO
EG1215         MOVE SQLCODE            TO ERR-SQL-CODE
EG1215         MOVE '6550'             TO ERR-LOCAL
EG1215         MOVE SPACES             TO ERR-SEGM
EG1215         PERFORM 9000-ROTINA-ERRO
EG1215     END-IF.
EG1215
EG1215*------------------------------------------------------------*
EG1215 4926-99-FIM.                    EXIT.
EG1215
BS0616*----------------------------------------------------------------*
BS0616 4930-REGRAS-PROAGRO             SECTION.
BS0616*----------------------------------------------------------------*
BS0616
BS0616     PERFORM 4931-BUSCAR-ORIGEM-ATUAL.
BS0616
BS0616     IF  CORIGE-REC              OF RUECV007 EQUAL 13 OR 31
BS0616         MOVE CFNALD-RURAL       OF RUECV007
BS0616                                 TO CFNALD-RURAL OF RURCB003
BS0616         PERFORM 4935-CHECAR-FINALIDADE-BACEN
BS0616         IF  CFNALD-RECOR        OF RURCB003 EQUAL '00002'
BS0616         AND ( CFNALD-RURAL      OF RUECV007 EQUAL 1 OR 2 OR 3
BS0616                                                OR 5 OR 9 OR 24 )
BS0616         AND ( CATVDD-RURAL      OF RUECV007 EQUAL 1 OR 5 )
BS0616         AND VFINAN              OF  RUECV007
BS0616                                 NOT GREATER  300000,00
BS0616         AND CPRODT-RURAL        OF  RUECV007 EQUAL
BS0616           ( 002 OR 007 OR 008 OR 009 OR 010 OR 011 OR 012 OR 015
BS0616          OR 017 OR 019 OR 021 OR 022 OR 023 OR 029 OR 031 OR 032
BS0616          OR 039 OR 043 OR 044 OR 045 OR 048 OR 050 OR 051 OR 052
BS0616          OR 066 OR 067 OR 071 OR 072 OR 077 OR 096 OR 098 OR 160
BS0616          OR 190 OR 192 OR 231 OR 232 OR 234 OR 235 OR 236 OR 236
BS0616          OR 237 OR 238 OR 239 OR 240 OR 241 OR 241 OR 243 OR 244
BS0616          OR 244 OR 245 OR 258 OR 259 OR 260 OR 261 OR 262 OR 263
BS0616          OR 265 OR 266 OR 267 OR 279 OR 280 OR 281 OR 282 OR 282
BS0616          OR 283 OR 291 OR 292 OR 293 OR 297 OR 298 OR 299 OR 301
BS0616          OR 303 OR 304 OR 305 OR 306 OR 307 OR 309 OR 315 OR 317
BS0616          OR 318 OR 320 OR 321 OR 323 OR 324 OR 324 OR 326 OR 330
BS0616          OR 332 OR 338 OR 340 OR 342 OR 343 OR 347 OR 349 OR 350
BS0616          OR 351 OR 352 OR 358 OR 363 OR 365 OR 368 OR 369 OR 372
BS0616          OR 378 OR 383 OR 383 OR 385 OR 389 OR 396 OR 397 OR 398
BS0616          OR 399 OR 400 OR 401 OR 405 OR 406 OR 411 OR 420 OR 421
BS0616          OR 423 OR 424 OR 425 OR 427 OR 429 OR 436 OR 499 OR 503
BS0616          OR 504 OR 530 OR 531 OR 563 OR 602 OR 606 OR 608 OR 609
BS0616          OR 610 OR 617 OR 618 OR 622 OR 626 )
BS0616             IF  CORIGE-REC      OF RUECV007 EQUAL 13
BS0616                 MOVE 28         TO CORIGE-REC OF RUECV007
BS0616                                    CORIGE-REC OF RUFIB002
BS0616                 PERFORM 4940-ATUALIZAR-RUECV007-ORIGEM
BS0616                 MOVE CCONTR     OF RUECV007
BS0616                                 TO CCONTR OF RUFIB002
BS0616             ELSE
BS0616                 IF  CORIGE-REC  OF RUECV007 EQUAL 31
BS0616                     MOVE 34     TO CORIGE-REC OF RUECV007
BS0616                                    CORIGE-REC OF RUFIB002
BS0616                     PERFORM 4940-ATUALIZAR-RUECV007-ORIGEM
BS0616                     MOVE CCONTR OF RUECV007
BS0616                                 TO CCONTR OF RUFIB002
BS0616                     PERFORM 4950-ATUALIZAR-RUFIB002-ORIGEM
BS0616                 END-IF
BS0616             END-IF
BS0616         END-IF
BS0616     END-IF.
BS0616
BS0616*----------------------------------------------------------------*
BS0616 4930-99-FIM.                    EXIT.
BS0616*----------------------------------------------------------------*
BS0616
BS0616*----------------------------------------------------------------*
BS0616 4931-BUSCAR-ORIGEM-ATUAL        SECTION.
BS0616*----------------------------------------------------------------*
BS0616
BS0616     MOVE INP-CEDULA-T916        TO CCDULA            OF RUECV007.
BS0616
BS0616     EXEC SQL
BS0616       SELECT
BS0616         CORIGE_REC
BS0616       INTO
BS0616         :RUECV007.CORIGE-REC
BS0616       FROM  DB2PRD.V01CEDULA_TEMPR
BS0616       WHERE CJUNC_DEPDC         = :RUECV007.CJUNC-DEPDC
BS0616       AND   CCDULA              = :RUECV007.CCDULA
BS0616     END-EXEC.
BS0616
BS0616     IF  ( SQLCODE               NOT EQUAL ZEROS )
BS0616     OR  SQLWARN0                EQUAL 'W'
BS0616         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0616         MOVE 'CEDULA_TEMPR'     TO ERR-DBD-TAB
BS0616         MOVE 'SELECT'           TO ERR-FUN-COMANDO
BS0616         MOVE SQLCODE            TO ERR-SQL-CODE
BS0616         MOVE '4931'             TO ERR-LOCAL
BS0616         MOVE SPACES             TO ERR-SEGM
BS0616         PERFORM 9000-ROTINA-ERRO
BS0616
BS0616*----------------------------------------------------------------*
BS0616 4931-99-FIM.                    EXIT.
BS0616*----------------------------------------------------------------*
BS0616
BS0616*----------------------------------------------------------------*
BS0616 4935-CHECAR-FINALIDADE-BACEN    SECTION.
BS0616*----------------------------------------------------------------*
BS0616
BS0616     EXEC SQL
BS0616       SELECT
BS0616         CFNALD_RECOR
BS0616       INTO
BS0616         :RURCB003.CFNALD-RECOR
BS0616       FROM  DB2PRD.FNALD_RURAL
BS0616       WHERE CFNALD_RURAL        = :RURCB003.CFNALD-RURAL
BS0616     END-EXEC.
BS0616
BS0616     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100 )
BS0616     OR  SQLWARN0                EQUAL 'W'
BS0616         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0616         MOVE 'SELECT'           TO ERR-FUN-COMANDO
BS0616         MOVE SQLCODE            TO ERR-SQL-CODE
BS0616         MOVE '4935'             TO ERR-LOCAL
BS0616         MOVE SPACES             TO ERR-SEGM
BS0616         PERFORM 9000-ROTINA-ERRO
BS0616     END-IF.
BS0616
BS0616     IF  SQLCODE                 EQUAL +100
BS0616         MOVE SPACES             TO CFNALD-RECOR OF RURCB003
BS0616     END-IF.
BS0616
BS0616*----------------------------------------------------------------*
BS0616 4935-99-FIM.                    EXIT.
BS0616*----------------------------------------------------------------*
BS0616
BS0616*----------------------------------------------------------------*
BS0616 4940-ATUALIZAR-RUECV007-ORIGEM  SECTION.
BS0616*----------------------------------------------------------------*
BS0616
BS0616     MOVE INP-AGENCIA-T916       TO CJUNC-DEPDC       OF RUECV007.
BS0616     MOVE INP-CEDULA-T916        TO CCDULA            OF RUECV007.
BS0616     EXEC SQL
BS0616         UPDATE  DB2PRD.CEDULA_TEMPR
BS0616            SET  CORIGE_REC        = :RUECV007.CORIGE-REC
BS0616          WHERE  CJUNC_DEPDC       = :RUECV007.CJUNC-DEPDC
BS0616            AND  CCDULA            = :RUECV007.CCDULA
BS0616     END-EXEC.
BS0616
BS0616     IF (SQLCODE                 NOT EQUAL ZEROS) OR
BS0616        (SQLWARN0                EQUAL 'W')
BS0616         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
BS0616         MOVE 'CEDULA_TEMPR'     TO  ERR-DBD-TAB
BS0616         MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
BS0616         MOVE SQLCODE            TO  ERR-SQL-CODE
BS0616         MOVE '4940'             TO  ERR-LOCAL
BS0616         MOVE SPACES             TO  ERR-SEGM
BS0616         PERFORM 9000-ROTINA-ERRO
BS0616     END-IF.
BS0616
BS0616*----------------------------------------------------------------*
BS0616 4940-99-FIM. EXIT.
BS0616*----------------------------------------------------------------*
BS0616*----------------------------------------------------------------*
BS0616 4950-ATUALIZAR-RUFIB002-ORIGEM  SECTION.
BS0616*----------------------------------------------------------------*
BS0616
BS0616     EXEC SQL
BS0616         UPDATE  DB2PRD.CONTRATO_RURAL
BS0616            SET  CORIGE_REC        = :RUFIB002.CORIGE-REC
BS0616          WHERE  CCONTR            = :RUFIB002.CCONTR
BS0616     END-EXEC.
BS0616
BS0616     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
BS0616        (SQLWARN0                EQUAL 'W')
BS0616         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
BS0616         MOVE 'CONTRATO_RURAL'   TO  ERR-DBD-TAB
BS0616         MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
BS0616         MOVE SQLCODE            TO  ERR-SQL-CODE
BS0616         MOVE '4950'             TO  ERR-LOCAL
BS0616         MOVE SPACES             TO  ERR-SEGM
BS0616         PERFORM 9000-ROTINA-ERRO
BS0616     END-IF.
BS0616
BS0616 4950-99-FIM. EXIT.
BS0616*----------------------------------------------------------------*
BS0616
BS0617*------------------------------------------------------------*
BS0617 4951-EXIGE-PRODUTIVIDADE        SECTION.
BS0617*------------------------------------------------------------*
BS0617
BS0617     MOVE SPACES                 TO WRK-EXIGE-PRODUT.
BS0617
BS0617     PERFORM 3070-ACESSAR-RUECV014.
BS0617
BS0617     PERFORM 4952-OPEN-CSR01-RUFIB002-JN.
BS0617
BS0617     PERFORM 4953-FETCH-CSR01-RUFIB002-JN.
BS0617
BS0617     PERFORM 4955-TRATAR-CURSOR-PRODUT
BS0617     UNTIL   WRK-FIM-RUFIB002    EQUAL 'S'
BS0617     OR      WRK-EXIGE-PRODUT    EQUAL 'S'.
BS0617
BS0617     PERFORM 4954-CLOSE-CSR01-RUFIB002-JN.
BS0617
BS0617 4951-99-FIM.                    EXIT.
BS0617*------------------------------------------------------------*
BS0617
BS0617*------------------------------------------------------------*
BS0617 4952-OPEN-CSR01-RUFIB002-JN     SECTION.
BS0617*------------------------------------------------------------*
BS0617
BS0617     EXEC SQL
BS0617       OPEN CSR01-RUFIB002-JN
BS0617     END-EXEC.
BS0617
BS0617     IF (SQLCODE                 NOT EQUAL ZEROS )
BS0617     OR (SQLWARN0                EQUAL 'W'       )
BS0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0617         MOVE 'RUFIB002'         TO ERR-DBD-TAB
BS0617         MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
BS0617         MOVE 'OPEN'             TO ERR-FUN-COMANDO
BS0617         MOVE SQLCODE            TO ERR-SQL-CODE
BS0617         MOVE '4952'             TO ERR-LOCAL
BS0617         MOVE SPACES             TO ERR-SEGM
BS0617         PERFORM 9000-ROTINA-ERRO
BS0617
BS0617     MOVE 'N'                    TO WRK-FIM-RUFIB002.
BS0617     MOVE 'N'                    TO WRK-EXIGE-PRODUT.
BS0617
BS0617*------------------------------------------------------------*
BS0617 4952-99-FIM.                    EXIT.
BS0617*------------------------------------------------------------*
BS0617
BS0617
      *------------------------------------------------------------*
       4953-FETCH-CSR01-RUFIB002-JN    SECTION.
      *------------------------------------------------------------*
           EXEC SQL
             FETCH CSR01-RUFIB002-JN
             INTO
               :RUECV014.CCONTR
             , :RUECV014.CSEQ-PSSOA
             , :RUFIV002.CORIGE-REC
                   :WRK-CORIGE-REC-NULL
             , :RUFIV002.CFNALD-RURAL
                   :WRK-CFNALD-RURAL-NULL
                   :WRK-CATVDD-RURAL-NULL
             , :RUFIV002.DVCTO-CONTR
             , :RUFIV002.VFINAN
             , :RUFIV002.QPROTV-ADQUI
                   :WRK-QPROTV-ADQUI-NULL
             , :RUFIV002.CCDULA
             , :RUFIV002.CREFT-BACEN
             , :RUFIV002.CUND-MEDD-FINCD
                   :WRK-CUND-MEDD-FINCD-NULL
             , :RUFIV002.CJUNC-DEPDC
             , :RUFIV002.CCONTR
             , :RUFIV002.DIMPRE-CONTR-RURAL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUFIB002'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-RUFIB002
               GO TO 4953-99-FIM
           END-IF.

           IF  WRK-CFNALD-RURAL-NULL  LESS  ZEROS
               MOVE ZEROS              TO  CFNALD-RURAL OF RUFIV002
           END-IF.

           IF  WRK-CATVDD-RURAL-NULL  LESS  ZEROS
               MOVE ZEROS              TO  CATVDD-RURAL OF RUFIV002
           END-IF.

           IF  WRK-CORIGE-REC-NULL  LESS  ZEROS
               MOVE ZEROS              TO  CORIGE-REC OF RUFIV002
           END-IF.

               MOVE ZEROS              TO  QPROTV-ADQUI OF RUFIV002
           END-IF.

           IF  WRK-CUND-MEDD-FINCD-NULL  LESS  ZEROS
               MOVE ZEROS              TO  CUND-MEDD-FINCD OF RUFIV002
           END-IF.

      *------------------------------------------------------------*
       4953-99-FIM.                    EXIT.
      *------------------------------------------------------------*
BS0617*------------------------------------------------------------*
BS0617 4954-CLOSE-CSR01-RUFIB002-JN    SECTION.
BS0617*------------------------------------------------------------*
BS0617
BS0617     EXEC SQL
BS0617       CLOSE CSR01-RUFIB002-JN
BS0617     END-EXEC.
BS0617
BS0617     IF (SQLCODE                 NOT EQUAL ZEROS AND + 100)
BS0617     OR (SQLWARN0                EQUAL 'W'                )
BS0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0617         MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
BS0617         MOVE 'CLOSE'            TO ERR-FUN-COMANDO
BS0617         MOVE SQLCODE            TO ERR-SQL-CODE
BS0617         MOVE '4954'             TO ERR-LOCAL
BS0617         MOVE SPACES             TO ERR-SEGM
BS0617         PERFORM 9000-ROTINA-ERRO
BS0617     END-IF.
BS0617
BS0617*------------------------------------------------------------*
BS0617 4954-99-FIM.                    EXIT.
BS0617*------------------------------------------------------------*
BS0617*------------------------------------------------------------*
BS0617 4955-TRATAR-CURSOR-PRODUT       SECTION.
BS0617*------------------------------------------------------------*
BS0617
BS0617     PERFORM 4956-SELECT-COUNT-RURCB099.
BS0617
BS0617     IF  WRK-QTD-EXIGE-PRODUT    NOT EQUAL ZEROS
BS0617     AND QPROTV-ADQUI            OF RUFIV002
BS0617                                 EQUAL ZEROS
BS0617         MOVE 'S'                TO WRK-EXIGE-PRODUT
BS0617     END-IF.
BS0617
BS0617
BS0617     PERFORM 4953-FETCH-CSR01-RUFIB002-JN.
BS0617
BS0617*------------------------------------------------------------*
BS0617 4955-99-FIM.                    EXIT.
BS0617*------------------------------------------------------------*
BS0617
BS0617*------------------------------------------------------------*
BS0617 4956-SELECT-COUNT-RURCB099      SECTION.
BS0617*------------------------------------------------------------*
BS0617
BS0617     MOVE CORIGE-REC   OF RUFIV002 TO WRK-CORIGE-REC
BS0617     MOVE CFNALD-RURAL OF RUFIV002 TO WRK-CFNALD-RURAL
BS0617     MOVE CATVDD-RURAL OF RUFIV002 TO WRK-CATVDD-RURAL
BS0617     MOVE VFINAN  OF RUFIV002      TO WRK-VFINAN
BS0617     MOVE ZEROS                    TO WRK-QTD-EXIGE-PRODUT
BS0617     MOVE ZEROS                    TO WRK-NREGRA-PROTV-ADQUI
BS0617
BS0617
BS0617       SELECT
BS0617         COUNT(*)
BS0617       INTO
BS0617         :WRK-QTD-EXIGE-PRODUT
BS0617       FROM  DB2PRD.TREGRA_PROTV_ADQUI
BS0617       WHERE CORIGE_REC_INIC    <= :WRK-CORIGE-REC
BS0617         AND CORIGE_REC_FNAL    >= :WRK-CORIGE-REC
BS0617         AND CATVDD_RURAL_INIC  <= :WRK-CATVDD-RURAL
BS0617         AND CATVDD_RURAL_FNAL  >= :WRK-CATVDD-RURAL
BS0617         AND CFNALD_RURAL_INIC  <= :WRK-CFNALD-RURAL
BS0617         AND CFNALD_RURAL_FNAL  >= :WRK-CFNALD-RURAL
BS0617         AND VMIN_FINAN_RURAL   <= :WRK-VFINAN
BS0617         AND CSIT_PROTV_ADQUI    = 1
BS0617         AND DINIC_VGCIA        <= :RUFIV002.DIMPRE-CONTR-RURAL
BS0617         AND IFNULL(DFIM_VGCIA, '31.12.3999' )
BS0617                                >= CURRENT DATE
BS0617         AND NREGRA_PROTV_ADQUI >= :WRK-NREGRA-PROTV-ADQUI
BS0617
BS0617     END-EXEC.
BS0617
BS0617     IF (SQLCODE                   NOT EQUAL ZEROS AND -305)
BS0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0617         MOVE 'RURCB099'         TO ERR-DBD-TAB
BS0617         MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
BS0617         MOVE 'SELECT'           TO ERR-FUN-COMANDO
BS0617         MOVE SQLCODE            TO ERR-SQL-CODE
BS0617         MOVE '4956'             TO ERR-LOCAL
BS0617         MOVE SPACES             TO ERR-SEGM
BS0617         PERFORM 9000-ROTINA-ERRO
BS0617     END-IF.
BS0617
BS0617*------------------------------------------------------------*
BS0617 4956-99-FIM.                    EXIT.
BS0617*------------------------------------------------------------*
EG1215
EG1215
EG0517*----------------------------------------------------------------*
EG0517 5000-CHAMAR-RUEC8810            SECTION.
EG0517*----------------------------------------------------------------*
EG0517
EG0517
EG0517     MOVE  'RUEC8810'            TO  WRK-MODULO
EG0517     CALL  WRK-MODULO            USING RUEC65-AREA
EG0517
EG0517     IF  RUEC65-COD-RET          EQUAL 15
EG0517         GO TO 5000-CHAMAR-RUEC8810
EG0517     END-IF.
EG0517
EG0517     EVALUATE RUEC65-COD-RET
EG0517         WHEN ZEROS
EG0517             MOVE WRK-M61        TO WRK-MENS01
EG0517         WHEN 01 THRU 08
EG0517             MOVE RUEC65-COD-RET TO WRK-M59-RC
EG0517             MOVE WRK-M59        TO WRK-MENS01
EG0517         WHEN 09 THRU 14
EG0517             MOVE RUEC65-COD-RET  TO WRK-M59-RC
EG0517             MOVE RUEC65-MENSAGEM TO WRK-MENS01
EG0517         WHEN 15 THRU 98
EG0517             MOVE RUEC65-COD-RET  TO WRK-M59-RC
EG0517             MOVE WRK-M59         TO WRK-MENS01
EG0517         WHEN 99
EG0517             MOVE RUEC65-SQLCA    TO SQLCA
EG0517             MOVE RUEC65-ERRO-AREA TO ERRO-AREA
EG0517             AND (ERR-MQ-FILA      EQUAL
EG0517                                   'RCOR.QR.REQ.RCOR.TMNF.01')
EG0517             AND (ERR-TIPO-ACESSO  EQUAL  'MQ')
EG0517             AND (ERR-MQ-FUNCAO    EQUAL  'OPEN')
EG0517                 MOVE WRK-M66      TO WRK-MENS01
EG0517             ELSE
EG0517                 PERFORM 9000-ROTINA-ERRO
EG0517             END-IF
EG0517     END-EVALUATE.
EG0517
EG0517*----------------------------------------------------------------*
EG0517 5000-99-FIM.                    EXIT.
EG0517*----------------------------------------------------------------*
EG0517
BR1217/------------------------------------------------------------*
BR1217 6000-OBTER-RESP               SECTION.
BR1217*------------------------------------------------------------*
BR1217     INITIALIZE WRK-LKG-AREA
BR1217
BR1217     MOVE 'RUEC9160'           TO WRK-PGM-CHAMADOR
BR1217     MOVE INP-AGENCIA-T916     TO WRK-AGENCIA
BR1217
BR1217     IF CFLIAL-CGC             OF RUECV014
BR1217                               NOT EQUAL ZEROS
BR1217        MOVE CCGC-CPF          OF RUECV014
BR1217                               TO WRK-NUMCGC
BR1217        MOVE CFLIAL-CGC        OF RUECV014
BR1217                               TO WRK-FILIAL
BR1217        MOVE CCTRL-CPF-CGC     OF RUECV014
BR1217                               TO WRK-CTRL-CGC
BR1217        MOVE 'J'               TO WRK-TIPO-PESSOA
BR1217     ELSE
BR1217        MOVE CCGC-CPF          OF RUECV014
BR1217                               TO WRK-NUMCPF
7C5.10        MOVE SPACES            TO WRK-FILLER
BR1217        MOVE CCTRL-CPF-CGC     OF RUECV014
BR1217                               TO WRK-CTRL-CPF
BR1217        MOVE 'F'               TO WRK-TIPO-PESSOA
BR1217     END-IF.
BR1217
7C0625     MOVE                 1 TO WRK-NUM-ENVIO

BR1217                                      ERRO-AREA.
BR1217

BR1217     IF WRK-COD-RET            EQUAL ZEROS
.            INITIALIZE WRK-LKG-AREA
.
.            MOVE 'RUEC9160'           TO WRK-PGM-CHAMADOR
.            MOVE INP-AGENCIA-T916     TO WRK-AGENCIA
.            MOVE INP-CEDULA-T916      TO WRK-CEDULA
.
.            IF CFLIAL-CGC             OF RUECV014
.                                      NOT EQUAL ZEROS
.               MOVE CCGC-CPF          OF RUECV014
.                                      TO WRK-NUMCGC
.               MOVE CFLIAL-CGC        OF RUECV014
.                                      TO WRK-FILIAL
.               MOVE CCTRL-CPF-CGC     OF RUECV014
.                                      TO WRK-CTRL-CGC
.               MOVE 'J'               TO WRK-TIPO-PESSOA
.            ELSE
.               MOVE CCGC-CPF          OF RUECV014
7C5.10          MOVE SPACES            TO WRK-FILLER
.               MOVE CCTRL-CPF-CGC     OF RUECV014
.                                      TO WRK-CTRL-CPF
.               MOVE 'F'               TO WRK-TIPO-PESSOA
.            END-IF
.
CC1811***
BR1811     EXEC SQL
BR1811          SET :WRK-TIMESTAMP-1S  = CURRENT_TIMESTAMP + 5 SECOND
BR1811     END-EXEC
BR1811
BR1811     PERFORM UNTIL WRK-TIMES-1S  GREATER WRK-TIMESTAMP-1S
BR1811         CALL 'POOL7600'         USING WRK-AREA-POOL7600
BR1811
BR1811         STRING WRK-PO-TIMESTAMP(01:04) '-'
BR1811                WRK-PO-TIMESTAMP(05:02) '-'
BR1811                WRK-PO-TIMESTAMP(07:02) '-'
BR1811                WRK-PO-TIMESTAMP(09:02) '.'
BR1811                WRK-PO-TIMESTAMP(11:02) '.'
BR1811                WRK-PO-TIMESTAMP(13:02) '.'
BR1811                WRK-PO-TIMESTAMP(15:06)
CC1811     END-PERFORM

.            CALL WRK-MODULO-RUEC8840  USING  WRK-LKG-AREA
.                                             ERRO-AREA
.
.
.            IF WRK-COD-RET            EQUAL ZEROS
.               MOVE WRK-CONTROLE-BACEN
.                                      TO     INP-CTRBAC-T916
.                                             OUT-CTRBAC-T916
.            ELSE
.               MOVE WRK-MENSAGEM-R    TO WRK-MENS01
.
7C0625       END-IF
BR1217     ELSE
BR1217        MOVE WRK-MENSAGEM-R    TO WRK-MENS01
BR1217
BR1217     END-IF.
BR1217
BR1217 6000-99-FIM. EXIT.

BR1217 6500-TRATAR-RETORNO           SECTION.
BR1217*------------------------------------------------------------*
BR1217
BR1217     MOVE INP-CTRBAC-T916      TO CCTRL-RQUIS-FINCR OF RUECB09C.
BR1217     MOVE 'COR0006R1'          TO WRK-CMSGEM-RECOR
BR1217     MOVE 'COR0006E'           TO WRK-CMSGEM-RECOR-E
BR1217     MOVE ZEROS                TO NMNTRC-MSGEM-RURAL
BR1217                                                    OF RUECB09C.
BR1217     EXEC SQL
BR1217       SELECT CMSGEM_RECOR
BR1217        INTO :RUECB09C.CMSGEM-RECOR
BR1217                 :WRK-CMSGEM-RECOR-NULL
BR1217       FROM  DB2PRD.TMNTRC_MSGEM_RURAL
BR1217        WHERE CCTRL_RQUIS_FINCR   = :RUECB09C.CCTRL-RQUIS-FINCR
BR1217        AND  (CMSGEM_RECOR        = :WRK-CMSGEM-RECOR
BR1217         OR   CMSGEM_RECOR        = :WRK-CMSGEM-RECOR-E)
BR1217        AND   NMNTRC_MSGEM_RURAL >= :RUECB09C.NMNTRC-MSGEM-RURAL
BR1217     END-EXEC.
BR1217
BR1217     IF  ( SQLCODE   NOT  EQUAL ZEROS AND +100) OR
BR1217         ( SQLWARN0       =    'W'   )
BR1217           MOVE 'TMNTRC_MSGEM_RURAL'     TO   ERR-DBD-TAB
BR1217           MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
BR1217           MOVE SQLCODE                  TO   ERR-SQL-CODE
BR1217           MOVE '6500'                   TO   ERR-LOCAL
BR1217           MOVE SPACES                   TO   ERR-SEGM
BR1217           PERFORM   9000-ROTINA-ERRO
BR1217     END-IF.
BR1217
BR1217     IF SQLCODE                          EQUAL +100
BR1217        MOVE 'AGUARDANDO RETORNO DO BACEN'
BR1217                                         TO WRK-MENS01
BR1217        GO TO 6500-99-FIM
BR1217     END-IF.
BR1217
BR1217     IF  WRK-CMSGEM-RECOR-NULL           EQUAL -1
BR1217         MOVE SPACES                   TO CMSGEM-RECOR OF RUECB09C
BR1217     END-IF.
BR1217
BR1217     IF CMSGEM-RECOR OF RUECB09C         EQUAL 'COR0006E'
BR1217        MOVE 'INCONSISTENCIA NA MSG DE RETORNO RCOR'
BR1217                                         TO WRK-MENS01
BR1217     END-IF.
BR1217
BR1217 6500-99-FIM. EXIT.
BR1217
BR0718*----------------------------------------------------------------*
BR0718 7000-CHECAR-EXIGE-IMOVEL        SECTION.
BR0718*----------------------------------------------------------------*
BR0718
BR0718     MOVE 'S'                    TO WRK-EXIGE-IMOVEL.
BR0718
BR0718     MOVE  INP-AGENCIA-T916      TO CJUNC-DEPDC OF RUECV007
BR0718     MOVE  INP-CEDULA-T916       TO CCDULA      OF RUECV007
BR0718
BR0718     EXEC SQL
BR0718       SELECT
BR0718         CFNALD_RURAL
BR0718       , CATVDD_RURAL
BR0718       , CMODLD_RURAL
BR0718       , CPRODT_RURAL
BR0718       , CVAR_PRODT_RURAL
BR0718       , CCESTA_SAFRA_RURAL
BR0718       , CINDCD_CSCIO
BR0718       , CTPO_AGROP_RURAL
BR0718       , CTPO_INTGC_RURAL
BR0718       , CINDCD_UND_PRDTV
BR0718       , CTPO_IGCAO_RURAL
BR0718       , CTPO_CTIVO_RURAL
BR0718       , CCICLO_PROD_RURAL
BR0718       , DCDULA
PRI499       , CCLIMT_RURAL_BACEN
STE001       , VALUE(COUTRO_TRATO_SOLO, 0)
BR0718       INTO
BR0718         :RUECV007.CFNALD-RURAL
BR0718       , :RUECV007.CATVDD-RURAL
BR0718       , :RUECV007.CMODLD-RURAL
BR0718       , :RUECV007.CPRODT-RURAL
BR0718       , :RUECV007.CVAR-PRODT-RURAL
BR0718       , :RUECV007.CCESTA-SAFRA-RURAL
BR0718       , :RUECV007.CINDCD-ZNMTO
BR0718       , :RUECV007.CINDCD-CSCIO
BR0718       , :RUECV007.CTPO-AGROP-RURAL
BR0718       , :RUECV007.CTPO-INTGC-RURAL
BR0718       , :RUECV007.CTPO-IGCAO-RURAL
BR0718       , :RUECV007.CTPO-CTIVO-RURAL
BR0718       , :RUECV007.CCICLO-PROD-RURAL
BR0718       , :RUECV007.DCDULA
PRI499       , :RUECV007.CCLIMT-RURAL-BACEN
STE001       , :RUECV007.COUTRO-TRATO-SOLO
BR0718       FROM  DB2PRD.V01CEDULA_TEMPR
BR0718       WHERE CJUNC_DEPDC         = :RUECV007.CJUNC-DEPDC
BR0718       AND   CCDULA              = :RUECV007.CCDULA
BR0718     END-EXEC.
BR0718
BR0718     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
BR0718        (SQLWARN0                EQUAL 'W')
BR0718         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
BR0718         MOVE 'CEDULA_TEMPR'     TO  ERR-DBD-TAB
BR0718         MOVE 'SELECT'           TO  ERR-FUN-COMANDO
BR0718         MOVE SQLCODE            TO  ERR-SQL-CODE
BR0718         MOVE '7000'             TO  ERR-LOCAL
BR0718         MOVE SPACES             TO  ERR-SEGM
BR0718         PERFORM 9000-ROTINA-ERRO
BR0718     END-IF.
BR0718     IF  SQLCODE                 EQUAL  +100
BR0718         GO TO 7000-99-EXIT
BR0718     END-IF.
BR0718*
BR0718     INITIALIZE  RUEC58-AREA.
BR0718     MOVE 'RUEC8100'                     TO RUEC58-PGM-CHAMADOR
BR0718     MOVE ZEROS                          TO RUEC58-CONTRATO
BR0718     MOVE CJUNC-DEPDC        OF RUECV007 TO RUEC58-AGENCIA
BR0718     MOVE CCDULA             OF RUECV007 TO RUEC58-CEDULA
BR0718     MOVE CFNALD-RURAL       OF RUECV007 TO RUEC58-FINALIDADE
BR0718     MOVE CATVDD-RURAL       OF RUECV007 TO RUEC58-ATIVIDADE
BR0718     MOVE CMODLD-RURAL       OF RUECV007 TO RUEC58-MODALIDADE
BR0718     MOVE CPRODT-RURAL       OF RUECV007 TO RUEC58-PRODUTO
BR0718     MOVE CVAR-PRODT-RURAL   OF RUECV007 TO RUEC58-VARIEDADE
BR0718     MOVE CCESTA-SAFRA-RURAL OF RUECV007 TO RUEC58-CESTA-SAFRA
BR0718     MOVE CINDCD-ZNMTO       OF RUECV007 TO RUEC58-ZONEAMENTO
BR0718     MOVE CINDCD-CSCIO       OF RUECV007 TO RUEC58-CONSORCIO
BR0718     MOVE CTPO-AGROP-RURAL   OF RUECV007 TO RUEC58-TIPO-AGROP
BR0718     MOVE CTPO-INTGC-RURAL   OF RUECV007 TO RUEC58-TIPO-INTEG
BR0718     MOVE CINDCD-UND-PRDTV   OF RUECV007 TO RUEC58-GRAO-SEMENTE
BR0718     MOVE CTPO-IGCAO-RURAL   OF RUECV007 TO RUEC58-TIPO-IRRIG
BR0718     MOVE CCICLO-PROD-RURAL  OF RUECV007 TO RUEC58-CICLO-PROD
PRI499     MOVE CCLIMT-RURAL-BACEN OF RUECV007 TO RUEC58-CLIMA-PROD
STE001     MOVE COUTRO-TRATO-SOLO  OF RUECV007
STE001                             TO WRK-N05
STEFAB     MOVE WRK-N05S               TO RUEC58-COUTRO-TRATO-SOLO
BR0718     MOVE DCDULA             OF RUECV007 TO RUEC58-DCDULA
BR0718
BR0718     CALL  WRK-RUEC8800          USING RUEC58-AREA
BR0718                                       IO-PCB
BR0718                                       ALT-PCB.
BR0718
BR0718      IF RUEC58-COD-RET          EQUAL 99
BR0718         MOVE RUEC58-SQLCA       TO SQLCA
BR0718         MOVE RUEC58-ERRO-AREA   TO ERRO-AREA
BR0718         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR0718         MOVE SPACES             TO ERR-SEGM
BR0718         MOVE ERR-PGM            TO ERR-MODULO
BR0718         PERFORM 9000-ROTINA-ERRO
BR0718     ELSE
BR0718         IF RUEC58-COD-RET       GREATER 0
BR0718            MOVE SPACES          TO RUEC58-EMPREEND
BR0718     END-IF.
BR0718
BR0718     IF  RUEC58-PRODUTO          EQUAL ( 593 OR 598 OR 591
BR0718                                    OR   591 OR 592 OR 592
BR0718                                    OR   605 OR 605 OR 643
BR0718                                    OR   151 OR 615 OR 613
BR0519                                    OR   613 OR 696 OR 445
BR0519                                    OR   717 OR 718 OR 726)
BR0718         MOVE 'N'                TO WRK-EXIGE-IMOVEL
BR0718     END-IF.
BR0718
BR0919***  INCIDENTE: IN3351458
BR0919***  I* RUEC58-EMPREEND          *QUAL '22605830000408'
BR0919***                              *R    '22605830990408'
BR0919***                              *R    '14030560000408'
BR0919***      MO*E 'N'                *O WRK-EXIGE-IMOVEL
BR0919***  E*D-I*.
BR0919***  INCIDENTE: IN3351458
BR0919
DS0620***  INCIDENTE: IN4146358
DS0620     IF RUEC58-EMPREEND          EQUAL '14030560000408'
WI0521                                 OR    '24031300074408'
DS0620         MOVE 'N'                TO WRK-EXIGE-IMOVEL
DS0620     END-IF.
DS0620***  INCIDENTE: IN4146358

BR0718*---------------------------------------------------------------*
BR0718 7000-99-EXIT.                   EXIT.
BR0718*---------------------------------------------------------------*
BR0718
       9000-ROTINA-ERRO SECTION.

           MOVE    WRK-COD-USER            TO    ERR-COD-USER
           MOVE    WRK-COD-DEPTO           TO    ERR-COD-DEPTO
           MOVE   'SENHAS02'               TO    ERR-PGM
           MOVE   'RUEC9160'               TO    ERR-MODULO

           CALL   'POOL7100'            USING    IO-PCB
                                                 ERRO-AREA
                                                 ALT-PCB
                                                 SQLCA


       9000-99-ROTINA-ERRO. EXIT.

PW6062*----------------------------------------------------------------*
PW6062* SECTION PARA CHAMADA DA ROTINA DE CONVERSAO DE AGENCIA
PW6062*----------------------------------------------------------------*
PW6062*
PW6062*----------------------------------------------------------------*
PW6062 9990-10-CHAMA-ROTINA-FX-AG SECTION.
PW6062*----------------------------------------------------------------*
PW6062*
PW6062     CALL        WRK-MODULO-MESUX0 USING WRK-AREA-MESUX0.
PW6062*
PW6062     MOVE        ZEROS       TO          WRK-MESUX0-AGENCIA.
PW6062*
PW6062*----------------------------------------------------------------*
PW6062 9990-90-EXIT. EXIT.
PW6062*----------------------------------------------------------------*
PW6062*
