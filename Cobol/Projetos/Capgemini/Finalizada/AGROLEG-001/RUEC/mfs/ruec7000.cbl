      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. RUEC7000.
       AUTHOR.RODNEI MARINS  KRYWOJ.
      *===============================================================*
      *                   C P M   S I S T E M A S                     *
      *---------------------------------------------------------------*
      *                                                               *
      *      PROGRAMA     : RUEC7000                                  *
      *      PROGRAMADOR  : RODNEI MARINS KRYWOJ  - C P M             *
      *      ANALISTA     : RODNEI BABOLIM MARTOS - C P M (GP 82)     *
      *      DATA         : 25/06/1998                                *
      *                                                               *
      *      OBJETIVO     : VALIDAR ACESSOS A PROGRAMAS PERMITIDOS    *
      *                     PELO USUARIO.                             *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      *      BANCO DE DADOS:                                          *
      *         DB2                                                   *
      *           TABLE                          INCLUDE/BOOK         *
      *                                                               *
      *           DB2PRD.MATR_CDULA_RURAL          RUECB041           *
      *           DB2PRD.NAVEGACAO_RURAL           RUECB042           *
      *           DB2PRD.V01CEDULA_TEMPR           RUECB007           *
      *           DB2PRD.V01PSSOA_CDULA            RUECV008           *
      *           DB2PRD.V01INCON_CDULA            RUECV019           *
      *           DB2PRD.V01TPO_PROTR_RURAL        RURCV022           *
      *           DB2PRD.V01ORIGEM_RECURSO         RURCV000           *
      *           DB2PRD.V01ATIVIDADE_RURAL        RURCV004           *
      *           DB2PRD.V01MUNICIPIO_RURAL        RURCV014           *
      *           DB2PRD.V01RAZMES_ORIGREC         RURCV040           *
      *           DB2PRD.SEQ_TAXA_RURAL            RURCB051           *
      *           DB2PRD.V01CTRL_CDULA             RUECV001           *
      *           DB2PRD.V50DEPDC_BDSCO            MESUV050           *
POL097*           DB2PRD.TAPOIO_UTILZ_MDULO        RUECB048           *
PW9931*           DB2PRD.TPRODT_RURAL_TEMPR        RUECB09H           *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      *      TELAS                                                    *
      *      -----                                                    *
      *      RUECT700                                                 *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      *      MODULOS CHAMADOS                                         *
      *      ----------------                                         *
      *      - POOL0660                                               *
      *      - POOL5000                                               *
      *      - POOL7100                                               *
ACT181*      - POOL1205 - VERIFICA DIA - SE  FERIADO/NORMAL           *
LMC   *      - CMRU309C - ACESSO A TABELA CRURB0L5                    *
      *                                                               *
      *===============================================================*

      *===============================================================*
STF   *                                                               *
STF   *            S T E F A N I N I  C O N S U L T O R I A           *
STF   *                                                               *
STF   *===============================================================*
STF   *                                                               *
STF   *                          ALTERACAO                            *
STF   *                      -----------------                        *
STF   *                                                               *
STF   *   PROGRAMADOR   : DANIEL ALVES     - STEFANINI                *
STF   *   ANALISTA      : ALDO JOSE GARCIA - CPM SISTEMAS             *
STF   *   DATA          : 21/05/2001                                  *
STF   *   OBJETIVO      : PASSAR A STARTAR A IMPRESSAO DA CEDULA EM 4 *
STF   *                   VIAS                                        *
STF   *                                                               *
STF   *   BANCO DE DADOS:                                             *
STF   *      DB2                                                      *
STF   *        TABLE                          INCLUDE/BOOK            *
STF   *                                                               *
STF   *        DB2PRD.OBS_DEVLC_RURAL           RUECB045              *
      *                                                               *
      *===============================================================*
      *                                                               *
      *                  A T O S    O R I G I N                       *
      *                                                               *
      *===============================================================*
      *                                                               *
      *                      ULTIMA  ALTERACAO                        *
      *                      -----------------                        *
      *                                                               *
      *   PROGRAMADOR   : DIRCEU           - ATOS ORIGIN              *
      *   ANALISTA      : DIRCEU           - ATOS ORIGIN              *
      *   DATA          : 10/02/2003                                  *
      *   OBJETIVO      : SEPARAR OS PROCEDIMENTOS DE INCLUSAO DE     *
      *                   CEDULA DO TRATAMENTO DE CEDULAS PENDENTES   *
      *                   E IMPRESSAO                                 *
      *                                                               *
      *===============================================================*
      *                       A L T E R A C A O                       *
      *---------------------------------------------------------------*
      *    PROGRAMADOR.:   ELIZANDRO NEGRI         - CPM PATO BRANCO  *
      *    ANALISTA CPM:   FELIPE SOMMER           - CPM PATO BRANCO  *
      *    ANALISTA....:   MARCOS TADEU            - G&P / GP. 10     *
      *    DATA........:   09/03/2006                                 *
      *---------------------------------------------------------------*
      *    OBJETIVO....:   COMPARTILHAR TRANSACOES GERENCIAIS, VERIFI-*
      *      CANDO SE USUARIO E GERENTE ATIVO MIDLE E SE A AGENCIA  A-*
      *      CESSADA E SUA SUB-SEDE.                                  *
      *---------------------------------------------------------------*
      *    BOOK'S......:                                              *
      *    I#MIDD01 - AREA DE COMUNICACAO COM MODULO MIDD9000         *
      *---------------------------------------------------------------*
      *    MODULOS.....:                                              *
      *    MIDD9000 - MODULO DE VALIDACAO DE ACESSO DE FUNCIONARIOS A *
      *               A CLIENTES MIDDLE (AGENCIA/CONTA CORRENTE)      *
      *    POOL0081 - ACESSO DINAMICO A MODULOS                       *
      *===============================================================*
POL126*                       A L T E R A C A O                       *
POL126*---------------------------------------------------------------*
POL126*    ANALISTA....:   MARINA FUJITA -  ALTRAN                    *
POL126*    DATA........:   ABRIL/2006                                 *
POL126*    PROJETO.....:   POL 05/0126-01  2004/2149/5-00 HISTORICO DE*
POL126*                    CEDULAS (HISTORICO DEVOLUCOES)             *
POL126*    OBJETIVO....:   SUBSTITUIR TABELA RUECB045 PELA RUECB064   *
POL126*===============================================================*
POL129*                       A L T E R A C A O                       *
POL129*---------------------------------------------------------------*
POL129*    ANALISTA....:   MARINA FUJITA -  ALTRAN                    *
POL129*    DATA........:   ABRIL/2006                                 *
POL129*    PROJETO.....:   POL 05/0129-01  2004/2057/5-00 AVISO EXCLU-*
POL129*                    CEDULA RURAL                               *
POL129*    OBJETIVO....:   INCLUIR NOVA COLUNA DDSPDO_AG NO RUECB007  *
POL128*===============================================================*
POL128*                       A L T E R A C A O                       *
POL128*---------------------------------------------------------------*
POL128*    ANALISTA....:   MARINA FUJITA -  ALTRAN                    *
POL128*    DATA........:   MAIO/2006                                  *
POL128*    PROJETO.....:   POL 05/0128-01  2005/1147/5-00             *
POL128*                    CRIAR TAB. DIFERENCIADA SEGURADORA (EGF)   *
POL128*    OBJETIVO....:   INCLUIR NOVO COD. SEGURADORA RUECB007      *
POL128*===============================================================*
ACT181*              U L T I M A   A L T E R A C A O                  *
ACT181*===============================================================*
ACT181*                                                               *
ACT181*     ANALISTA ......: LEANDRO LIMA         - BRAD  / GP.70     *
ACT181*     ANALISTA ......: HEYDI RAFAELA MARIGO - ALTRAN            *
ACT181*     DATA ..........: 03/12/2007                               *
ACT181*                                                               *
ACT181*     OBJETIVO ......: VERIFICAR ULTIMO DIA DO ANO SE           *
ACT181*                      FERIADO / UTIL.                          *
ACT181*                                                               *
ACT181*     PROJETO .......: ACT 07/181-01  ANPR 2007/0164/3-03       *
ACT181*                      PROCEDIMENTO FINAL DE ANO                *
ACT181*                                                               *
ACT181*===============================================================*
ACT115*                          A L T E R A C A O                    *
ACT115*****************************************************************
ACT115*                                                               *
ACT115*    ANALISTA    :  MARINA FUJITA - ALTRAN                      *
ACT115*    DATA        :  AGOSTO/2007                                 *
ACT115*    PROJETO     :  ACT 07/0115-01  2007/0865/5-00              *
ACT115*                   ALTERAR REGRAS ENVIO RECOR/BACEN            *
ACT115*    OBJETIVO    :  TRATAR CAMPO RUECB007.DIMPRE_CONTR_RURAL    *
ACT115*                   INICIALIZAR COM 01.01.0001                  *
ACT115*                                                               *
ACT115*****************************************************************
PW1577*==============================================================*
PW1577*               U L T I M A   A L T E R A C A O                *
PW1577*==============================================================*
PW1577*                                                              *
PW1577*    ANALISTA ...:   KATIA VIEIRA           - BRAD / GP.70     *
PW1577*    ANALISTA ...:   HEYDI RAFAELA MARIGO   - SONDA PROCWORK   *
PW1577*    DATA .......:   NOV/2009                                  *
PW1577*                                                              *
PW1577*    OBJETIVO ...:   EXCLUIR REGISTROS RELACIONADOS A AREA DA  *
PW1577*                    TABELA RUECB048.                          *
PW1577*                                                              *
PW1577*    PROJETO ....:   PWT 09/1577  ANPR 2008/0536/5-02          *
PW1577*                    REGULARIZAR CEDULAS RURAIS CARTORIOS.     *
PW1577*                                                              *
PW1577*==============================================================*
POL097*                       A  L  T  E  R  A  C  A  O              *
POL097*                       -------------------------              *
POL097*                                                              *
POL097*   ANALISTA      : MARINA FUJITA - POLEN TECHNOLOGIES         *
POL097*   DATA          : JULHO/2005 - REALTERADO EM ABRIL/2010      *
POL097*   PROJETO       : POL 0097/04 - RUEC X CADU                  *
POL097*   OBJETIVO      : SUBSTITUIR TABELAS E PROCEDIMENTOS FICA    *
POL097*                   POR CADU E CADASTRO RURAL RUEC             *
POL097*                                                              *
      *==============================================================*
PW6062*                            A L T E R A C A O                 *
PW6062*==============================================================*
PW6062*                                                              *
PW6062*     ANALISTA ......: MARINA FUJITA        - SONDA PROCWORK   *
PW6062*     DATA ..........: JANEIRO/2011                            *
PW6062*     PROJETO........: PWT 10/6062  2009/34565/5-09 10 11      *
PW6062*     OBJETIVO ......: ACESSO MULTIPLA JUNCAO DCO              *
PW6062*                      ADEQUAR/INCLUIR ACESSO MIDD9000         *
PW6062*==============================================================*
PW9931*                            A L T E R A C A O                 *
PW9931*==============================================================*
PW9931*                                                              *
PW9931*     ANALISTA ......: DANILO KIOROGLO      - SONDA IT         *
PW9931*     DATA ..........: JUNHO/2012                              *
PW9931*     PROJETO........: PWT 12//9931 NOVO RECOR                 *
PW9931*     OBJETIVO ......: CONTEMPLAR NOVOS CAMPOS COM PESQUISAS   *
PW9931*                      NAS TABELAS CORRESPONDENTES.            *
PW9931*==============================================================*
201503*                   A L T E R A C A O                           *
201503*===============================================================*
201503*                                                               *
201503*     ANALISTA ......: EVANDRO GUIMARAES    - BSI               *
201503*     DATA ..........: MARCO/2015                               *
201503*     OBJETIVO ......: ALTERAR CALCULO DA RECEITA BRUTA E       *
201503*                      PRODUCAO ESPERADA.                       *
201503*===============================================================*
201504*                   A L T E R A C A O                           *
201504*===============================================================*
201504*                                                               *
201504*     ANALISTA ......: EDSON MATSUMOTO      - BSI               *
201504*     DATA ..........: ABRIL/2015                               *
201504*     OBJETIVO ......: VERIFICAR AGENCIA MIGRADA E SE ESTA EM   *
201504*                      MANUTENCAO NO CRUR E BSI 12/0214         *
201504*===============================================================*
201503*===============================================================*
201504*                   A L T E R A C A O                           *
201504*===============================================================*
201504*                                                               *
201504*     ANALISTA ......: EVANDRO GUIMARAES    - BSI               *
201504*     DATA ..........: OUTUBRO/2015                             *
201504*     OBJETIVO ......: INCLUIR OPCAO 'NAO SE APLICA' PARA       *
201504*                      CONSORCIO E ZONEAMENTO                   *
201504*===============================================================*
LMC   *===============================================================*
LMC   *                   A L T E R A C A O                           *
LMC   *===============================================================*
LMC   *                                                               *
LMC   *     ANALISTA ......: LEONARDO MANÇO CAMARGO - BSI             *
LMC   *     DATA ..........: SETEMBRO/2016                            *
LMC   *     OBJETIVO ......: INCLUIR TIPO DE BENEFICIARIO             *
LMC   *===============================================================*
EG0817*===============================================================*
EG0817*                   A L T E R A C A O                           *
EG0817*===============================================================*
EG0817*                                                               *
EG0817*     ANALISTA ......: EVANDRO GUIMARAES - BSI                  *
EG0817*     DATA ..........: AGOSTO/2017                              *
EG0817*     OBJETIVO ......: IMPEDIR INCLUSAO DE NUMERO DE CEDULA     *
EG0817*                      DUPLICADO                                *
EG0817*===============================================================*
EG0817
EG0917*===============================================================*
EG0917*                   A L T E R A C A O                           *
EG0917*===============================================================*
EG0917*                                                               *
EG0917*     ANALISTA ......: EVANDRO GUIMARAES - BSI                  *
EG0917*     DATA ..........: AGOSTO/2017                              *
EG0917*     OBJETIVO ......: EXIGIR DAP PARA ORIGEM 37                *
EG0917*===============================================================*
EG0917
BR0118*---------------------------------------------------------------*
BR0118*                                                               *
BR0118*        ANALISTA : EDSON MATSUMOTO - BRQ SOLUTIONS             *
BR0118*        DATA     : JANEIRO / 2018                              *
BR0118*        OBJETIVO : TRATAR PGPAF                                *
BR0118*                                                               *
CAR   *===============================================================*
CAR   *                                                               *
CAR   *        ANALISTA : EVANDRO GUIMARAES - BRQ IT                  *
CAR   *        DATA     : MAIO / 2018                                 *
CAR   *        OBJETIVO : TRATAR TIPO DE BENEFICIARIO ANTES DO IMOVEL *
CAR   *                                                               *
CAR   *===============================================================*
RO4801*                   A L T E R A C A O                           *
RO4801*===============================================================*
RO4801*                                                               *
RO4801*     ANALISTA : ROSANA DE LIMA OLIVEIRA - DS BRADESCO          *
RO4801*     DATA     : ABRIL / 2020                                   *
RO4801*     OBJETIVO : DEMANDA REGULATORIA - FGPP - RESOLUCAO 4.801   *
RO4801*                                                               *
RO4801*===============================================================*
RO0520*                   A L T E R A C A O                           *
RO0520*===============================================================*
RO0520*                                                               *
RO0520*     ANALISTA : ROSANA DE LIMA OLIVEIRA - DS BRADESCO          *
RO0520*     DATA     : MAIO  / 2020                                   *
RO0520*     OBJETIVO : DEMANDA REGULATORIA - FGPP - RESOLUCAO 4.801   *
RO0520*                - AJUSTES REGRAS PARA ORIGEM 030               *
RO0520*                - NAO VERIFICAR O PORTE NO PSDC PARA ORIGEM 41 *
RO0520*                - AJUSTES DEVIDO A APONTAMENTOS HEXAVISION     *
RO0520*                (COMANDO IF SEM CLAUSULA END-IF/IF ENCADEADOS) *
      *===============================================================*
DS0620*===============================================================*
DS0620*                   A L T E R A C A O                           *
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
DS0121*     ANALISTA : VIVIANE PICUSSA         - DS BRADESCO          *
DS0121*     DATA     : JANEIRO / 2021                                 *
DS0121*     OBJETIVO : CORRECAO DO IN5246086                          *
DS0121*                NAO EXIGIR IMOVEL PARA O EMPREENDIMENTO        *
DS0121*                BACEN 14036780000408 / RURAL 769 TANGERINA     *
DS0121*                NAO EXIGIR IMOVEL DE APLICACAO                 *
DS0121*                                                               *
DS0121*===============================================================*
101020*                   A L T E R A C A O                           *
101020*===============================================================*
101020*                                                               *
101020*     ANALISTA : ELAINE DENAMI           - FOURSYS              *
101020*     DATA     : OUTUBRO / 2020                                 *
101020*     OBJETIVO : RESOLUCAO 4790                                 *
101020*                INCLUIR TELA PARA SELECAO DE FORMA PAGAMENTO   *
101020*===============================================================*
WI0421*                   A L T E R A C A O                           *
WI0421*===============================================================*
WI0421*                                                               *
WI0421*     ANALISTA : MILTON JANUARIO         - WIPRO                *
WI0421*     DATA     : ABRIL / 2021                                   *
WI0421*     OBJETIVO : IN5624443 - PERMITIR INCLUIR OS CAMPOS         *
WI0421*                CAMPOS QUANTIDADE E ÁREA PARA O EMPREENDIMENTO *
WI0421*                BACEN  23271300193002                          *
WI0421*===============================================================*
WI0520*                                                               *
WI0520*     ANALISTA : MILTON JANUARIO         - WIPRO                *
WI0520*     DATA     : MAIO  / 2021                                   *
WI0520*     OBJETIVO : CORRECAO DO IN5713251                          *
WI0520*                NAO EXIGIR IMOVEL PARA O EMPREENDIMENTO        *
WI0520*                INDUSTRIALIZACAO / PECUARIA DE BOVINOS         *
WI0520*                CEMPTO = 24031300074408                        *
WI0520*                                                               *
WI0520*================================================================*
BRQ001*     ANALISTA : WILLIAN - BRQ CURITIBA                          *
BRQ001*     DATA     : 06/09/2021                                      *
BRQ001*     OBJETIVO : TRATAMENTO PARA NOVOS CAMPOS DO CATALOGO SICOR  *
BRQ001*     PROJETO  : SICOR V5.02                                     *
BRQ001*================================================================*
WI1121*                   A L T E R A C A O                           *
WI1121*================================================================*
WI1121*     ANALISTA : JULIANO G ALVES - WIPRO                         *
WI1121*     DATA     : 10/11/2021                                      *
WI1121*     OBJETIVO : TRATAMENTO PARA NOVOS CAMPOS DO CATALOGO SICOR  *
WI1121*     PROJETO  : SICOR V5.03                                     *
WI1121*================================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
      *================================================================*
B0923 * ANALISTA REPONSAVEL: BELGA JUNIOR - FOURSYS                    *
B0923 * DATA...............: SETEMBRO - 2023                           *
B0923 * OBJETIVOS..........: RESOLUCAO 5.078 - REDUTOR DE TAXA - CAR   *
      *================================================================*
MJ0124*    ALTERACAO        : MJ0124                                   *
MJ0124*    DATA             : 12/01/2024                               *
MJ0124*    ANALISTA         : MILTON JANUARIO - BRADESCO               *
MJ0124*    - INCLUIR O ITEM 05 - SILAGEM                               *
202502*================================================================*
.     * ANALISTA REPONSAVEL: GUILHERME ABREU                           *
.     * DATA...............: FEVEREIRO- 2024                           *
.     * OBJETIVOS..........: NOTICIAS SICOR DE 16092024                *
.     *                      PROJETO UNIDADE ANIMAL                    *
202502*================================================================*
7COMMP*================================================================*
.     * ANALISTA REPONSAVEL: PRISCILA PEREIRA                          *
.     * DATA...............: MARCO - 2025                              *
.     * OBJETIVOS..........: SQUAD FLASH                               *
7COMMP*================================================================*
CCM01A*================================================================*
.     * ANALISTA REPONSAVEL: CRISTIANY MACAN                           *
.     * DATA...............: JANEIRO - 2026                            *
.     * OBJETIVOS..........: AGROLEG-404                               *
CCM01A*================================================================*
BR0226*================================================================*
.     * ANALISTA REPONSAVEL: BRUNO NOVELO                              *
.     * DATA...............: JANEIRO - 2026                            *
.     * OBJETIVOS..........: AGROLEG-259                               *
BR0226*================================================================*
      *================================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.


      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      ***                DEFINICAO DE AREAS E FUNCOES
      *---------------------------------------------------------------*

       77  WRK-GU                      PIC X(04)         VALUE 'GU'.
       77  WRK-CHNG                    PIC X(04)         VALUE 'CHNG'.
       77  WRK-ISRT                    PIC X(04)         VALUE 'ISRT'.

WI1121 01  WRK-COD-USERX               PIC X(07)         VALUE SPACES.
WI1121 01  WRK-COD-USERN               REDEFINES
WI1121     WRK-COD-USERX               PIC 9(07).

       77  WRK-COD-USER                PIC 9(07)         VALUE ZEROS.
       77  WRK-FUNCAO                  PIC X(04)         VALUE SPACES.
       77  WRK-NOVA-OPCAO              PIC 9(02)         VALUE ZEROS.
       77  WRK-OPCAO-ATUAL             PIC 9(02)         VALUE ZEROS.
       77  WRK-TELA                    PIC X(08)         VALUE SPACES.

       77  WRK-PERM-OK                 PIC X(01)         VALUE SPACES.
       77  WRK-MODULO                  PIC X(08)         VALUE SPACES.
       77  WRK-NPROD1-ANT              PIC X(40)         VALUE SPACES.
       77  WRK-ORIGCD-ANT              PIC X(03)         VALUE ZEROS.
       77  IND                         PIC 9(05)         VALUE ZEROS.
PW9931 77  WRK-IND                     PIC 9(05)  COMP-3 VALUE ZEROS.
       77  IND1                        PIC 9(05)         VALUE ZEROS.
       77  IND2                        PIC 9(05)         VALUE ZEROS.
       77  WRK-CRSUMO1                 PIC X(05)         VALUE SPACES.
       77  WRK-CRSUMO2                 PIC X(05)         VALUE SPACES.
       77  WRK-CRSUMO3                 PIC X(05)         VALUE SPACES.
       77  WRK-CRSUMO4                 PIC X(05)         VALUE SPACES.
       77  WRK-CRSUMO5                 PIC X(05)         VALUE SPACES.
       77  WRK-CRSUMO6                 PIC X(05)         VALUE SPACES.
       77  WRK-CMUN-RURAL-ANT          PIC 9(09) COMP-3  VALUE ZEROS.
PRI499 77  WRK-CRUR2G0C                PIC X(08)       VALUE 'CRUR2G0C'.
0524   77  WRK-CRUR2G2C                PIC X(08)       VALUE 'CRUR2G2C'.
       77  WRK-RUEC7704                PIC X(08)       VALUE 'RUEC7704'.
HEX   *77  WRK-RUEC7706                PIC X(08)       VALUE 'RUEC7706'.
       77  WRK-RUEC7708                PIC X(08)       VALUE 'RUEC7708'.
       77  WRK-RUEC7710                PIC X(08)       VALUE 'RUEC7710'.
POL097 77  WRK-RUEC7718                PIC X(08)       VALUE 'RUEC7718'.
POL097 77  WRK-RUEC7720                PIC X(08)       VALUE 'RUEC7720'.
POL097 77  WRK-RUEC7722                PIC X(08)       VALUE 'RUEC7722'.
PW9931 77  WRK-RUEC8800                PIC X(08)       VALUE 'RUEC8800'.
       77  WRK-EXISTE                  PIC X(01)       VALUE SPACE.
HEX   *77  WRK-FLAG-COOPER             PIC X(01)       VALUE SPACE.
HEX   *77  WRK-NULL-CCONTR             PIC S9(04) COMP VALUE ZEROS.

STEFA2 01  WRK-ENTRADA-X.
STEFA2     03  WRK-ENTRADA-X1          PIC X(12)       VALUE SPACES.
STEFA2     03  WRK-ENTRADA-X2          PIC X(02)       VALUE SPACES.

STEFA2 01  WRK-ENTRADA-N.
STEFA2     03  WRK-ENTRADA-N1          PIC X(11)       VALUE SPACES.
STEFA2     03  WRK-ENTRADA-N2          PIC X(01)       VALUE ','.
STEFA2     03  WRK-ENTRADA-N3          PIC X(02)       VALUE SPACES.

       77  WRK-ENTRADA                 PIC X(14).
       77  WRK-ENTRADA-2               PIC X(18)       VALUE SPACES.
       77  WRK-SAIDA                   PIC X(14).
       77  WRK-SAIDA-2                 PIC X(18)       VALUE SPACES.
       77  WRK-TAMANHO                 PIC 9(08)       VALUE 14140902.
       77  WRK-TAMANHO-2               PIC 9(08)       VALUE 18181202.
PW1577 77  WRK-WRESUL-CONS-MDULO-NULL  PIC S9(04) COMP   VALUE ZEROS.
PW9931 77  WRK-FLAG-PROD-CONS          PIC X(01)       VALUE SPACE.
PW9931 77  WRK-CPRODT-RURAL-NULL       PIC S9(04) COMP   VALUE ZEROS.
PW9931 77  WRK-CUSUAR-RESP-NULL        PIC S9(04) COMP   VALUE ZEROS.
PW9931 77  WRK-IMODLD-RURAL-NULL       PIC S9(04) COMP   VALUE ZEROS.
PW9931 77  WRK-NPROD-RURAL             PIC  9(05) COMP-3 VALUE ZEROS.
WI1121 77  WRK-CTPO-SOLO-AGCLT-NULL    PIC S9(04) COMP   VALUE ZEROS.
WI1121 77  WRK-CCICLO-CTVDA-NULL       PIC S9(04) COMP   VALUE ZEROS.
WI1121 77  WRK-CMANUT-SOLO-NULL        PIC S9(04) COMP   VALUE ZEROS.
EG1216
EG1216*----------------------------------------------------------------*
EG1216 01  FILLER                      PIC X(32)           VALUE
EG1216     '*      AREAS DA POOL7600       *'.
EG1216*----------------------------------------------------------------*
EG1216
EG1216 01  WRK-7600-DATA-HORA.
EG1216     05 WRK-7600-DT-JULIANA      PIC 9(05) COMP-3    VALUE ZEROS.
EG1216     05 WRK-7600-DT-AAMMDD       PIC 9(07) COMP-3    VALUE ZEROS.
EG1216     05 WRK-7600-DT-AAAAMMDD     PIC 9(09) COMP-3    VALUE ZEROS.
EG1216     05 WRK-7600-TI-HHMMSS       PIC 9(07) COMP-3    VALUE ZEROS.
EG1216     05 WRK-7600-TI-HHMMSSMMMMMM PIC 9(13) COMP-3    VALUE ZEROS.
EG1216     05 WRK-7600-TI-TIMESTAMP    PIC X(20)           VALUE SPACES.
EG1216
EG1216*----------------------------------------------------------------*
EG1216 01  FILLER                      PIC  X(32)         VALUE
EG1216     '**** AUXILIARES DE DATA      ***'.
EG1216*----------------------------------------------------------------*
EG1216
EG1216 01  WRK-DATA-DB2-DAP.
EG1216     10 WRK-DIA-DB2-DAP          PIC  9(02)         VALUE ZEROS.
EG1216     10 FILLER                   PIC  X(01)         VALUE '.'.
EG1216     10 WRK-MES-DB2-DAP          PIC  9(02)         VALUE ZEROS.
EG1216     10 FILLER                   PIC  X(01)         VALUE '.'.
EG1216     10 WRK-ANO-DB2-DAP          PIC  9(04)         VALUE ZEROS.
EG1216
EG1216 01  WRK-DATA-ISO                PIC  9(08)         VALUE ZEROS.
EG1216 01  FILLER                      REDEFINES WRK-DATA-ISO.
EG1216     10 WRK-ANO-ISO              PIC  9(04).
EG1216     10 WRK-MES-ISO              PIC  9(02).
EG1216     10 WRK-DIA-ISO              PIC  9(02).
EG1216
EG1216 01  WRK-DATA-ISO-9              PIC  9(09)         VALUE ZEROS.
EG1216 01  FILLER                      REDEFINES WRK-DATA-ISO-9.
EG1216     10 FILLER                   PIC  9(01).
EG1216     10 WRK-DATA-ISO-8           PIC  9(08).
EG1216
EG1216*----------------------------------------------------------------*
EG1216 01  FILLER                      PIC  X(32)         VALUE
EG1216     '**** AREA DE NULIDADE EG1216 ***'.
EG1216*----------------------------------------------------------------*
EG1216
EG1216 01  FILLER.
EG1216     10 WRK-V07-QUND-ANMAL-N     PIC S9(04) COMP    VALUE ZEROS.
EG1216     10 WRK-V07-NUMERO-DAP-N     PIC S9(04) COMP    VALUE ZEROS.
EG1216     10 WRK-V07-VALID-DAP-N      PIC S9(04) COMP    VALUE ZEROS.
EG1216     10 WRK-V07-CGRP-AGCLT-N     PIC S9(04) COMP    VALUE ZEROS.
BR1218     10 WRK-V07-VEGF-N           PIC S9(04) COMP    VALUE ZEROS.
EG1216
EG1216*----------------------------------------------------------------*
EG1216 01  FILLER                      PIC  X(32)         VALUE
EG1216     '**** HOSTS PARA CAMPOS DAP   ***'.
EG1216*----------------------------------------------------------------*
EG1216
EG1216 01  WRK-HOSTS-DAP.
EG1216     10 WRK-CTERM-DAP            PIC  X(08)         VALUE SPACES.
EG1216     10 WRK-WRESUL-DAP.
EG1216        49 WRK-WRESUL-DAP-LEN    PIC S9(04) COMP    VALUE ZEROS.
EG1216        49 WRK-WRESUL-DAP-TEXT   PIC  X(300)        VALUE SPACES.
EG1216     10 FILLER                   REDEFINES WRK-WRESUL-DAP.
EG1216        49 FILLER                PIC S9(04) COMP.
EG1216        49 WRK-NRODAP            PIC  X(25).
EG1216        49 WRK-VALDTD            PIC  X(02).
EG1216        49 WRK-VALDTM            PIC  X(02).
EG1216        49 WRK-VALDTA            PIC  X(04).
BR0118        49 WRK-GPPRONA           PIC  X(01).
EG1216        49 FILLER                PIC  X(266).
EG1216
EG1216*----------------------------------------------------------------*
LMC
LMC    01 FILLER.
LMC       03 WRK-CTPO-BNEFC-RURAL-NULL PIC S9(04) COMP   VALUE ZEROS.

EG0617*----------------------------------------------------------------*
EG0617 01  FILLER                      PIC  X(32)         VALUE
EG0617     '* AREAS DE NULIDADE RUECB056 ***'.
EG0617*----------------------------------------------------------------*
EG0617
EG0617 77  WRK-CNIRF-IMOV-RURAL-NULL   PIC S9(04)  COMP VALUE ZEROS.
EG0617 77  WRK-CCCIR-NULL              PIC S9(04)  COMP VALUE ZEROS.
EG0617 77  WRK-CREG-AMBTL-RURAL-NULL   PIC S9(04)  COMP VALUE ZEROS.
BRQ001 77  WRK-PPRESV-AMBTL-PPRIE-NULL PIC S9(04)  COMP VALUE ZEROS.
BRQ001 77  WRK-COUTGA-AGUA-IMOV-NULL   PIC S9(04)  COMP VALUE ZEROS.
BRQ001 77  WRK-DVALDD-OUTGA-AGUA-NULL  PIC S9(04)  COMP VALUE ZEROS.
EG0617
BR1218 77  WRK-EXIGE-AREA              PIC X(01)    VALUE SPACES.
BR0718 77  WRK-EXIGE-IMOVEL            PIC X(01)    VALUE SPACES.
BR0718 77  WRK-CHECA-AREA-FICHA        PIC X(01)    VALUE SPACES.
BR0718
BR0718 01  WRK-CH10.
BR0718     03  WRK-ZD5                 PIC 9(05)    VALUE ZEROS.
BR0718     03  FILLER                  PIC X(05)    VALUE SPACES.
BR0718
201503 01  WRK-AREAF-CH14.
201503     03  FILLER                  PIC 9(03)    VALUE ZEROS.
201503     03  WRK-AREAF-ZD9V2         PIC 9(09)V99 VALUE ZEROS.
201503
201503 01  WRK-CAMPO-CH14.
201503     03  FILLER                  PIC 9(03)    VALUE ZEROS.
201503     03  WRK-CAMPO-ZD9V2         PIC 9(09)V99 VALUE ZEROS.
201503
201503 01  FILLER.
201503     03  WRK-AREAF-PD9V2         PIC 9(09)V99 COMP-3 VALUE ZEROS.
201503     03  WRK-CAMPO-PD9V2         PIC 9(09)V99 COMP-3 VALUE ZEROS.
201503     03  WRK-CPRDTTL-PD9V2       PIC 9(09)V99 COMP-3 VALUE ZEROS.
201503
201503 01  FILLER.
201503     03  WRK-MANTEM-ATTR         PIC X(01)           VALUE SPACES.
201503
MI1008 01  WRK-LIKE-G.
MI1008     03  FILLER                  PIC X(08) VALUE '%COOPER%'.
MI1008     03  WRK-LIKE-GRUPO-G        PIC X(01).
MI1008     03  FILLER                  PIC X(08) VALUE '%GRANDE%'.
MI1008 01  WRK-LIKE-G-R REDEFINES WRK-LIKE-G PIC X(17).
MI1008
MI1008 01  WRK-LIKE-P.
MI1008     03  FILLER                  PIC X(08) VALUE '%COOPER%'.
MI1008     03  WRK-LIKE-GRUPO-P        PIC X(01).
MI1008     03  FILLER                  PIC X(09) VALUE '%PEQUENO%'.
MI1008 01  WRK-LIKE-P-R REDEFINES WRK-LIKE-P PIC X(18).

HEX   *01  WRK-VALOR-1                 PIC 9(13)V99 COMP-3 VALUE ZEROS.
HEX   *01  WRK-VALOR-2                 PIC 9(13)V99 COMP-3 VALUE ZEROS.
PW9931 01  WRK-VALOR-TOT               PIC 9(13)V99 COMP-3 VALUE ZEROS.

EG1216 01  WRK-X02-902                 PIC X(02)         VALUE SPACES.
EG1216 01  FILLER                      REDEFINES WRK-X02-902.
EG1216     05 WRK-X02-902-N            PIC 9(02).

EG1216 01  WRK-X04-904                 PIC X(04)         VALUE SPACES.
EG1216 01  FILLER                      REDEFINES WRK-X04-904.
EG1216     05 WRK-X04-904-N            PIC 9(04).

BR7518 01  WRK-ALPFA-1.
BR7518     05  WRK-NUMERICO-1          PIC 9(01) VALUE ZEROS.

       01  WRK-MENSAGEM.
           03  FILLER                  PIC X(04)         VALUE SPACES.
           03  WRK-TRANSACAO.
               05  WRK-TRANS-PREFIX    PIC X(04)         VALUE SPACES.
               05  FILLER              PIC X(04)         VALUE SPACES.
           03  FILLER                  PIC X(1988)       VALUE SPACES.

       01  WRK-COD-DEPTO6              PIC 9(06)         VALUE ZEROS.
       01  FILLER                      REDEFINES         WRK-COD-DEPTO6.
           03  WRK-COD-DEPTO2          PIC 9(02).
           03  WRK-COD-DEPTO           PIC 9(04).
WI1121     03  WRK-COD-DEPTOX          REDEFINES
WI1121         WRK-COD-DEPTO           PIC X(04).

       01  WRK-TABELA.
           03  WRK-TABELA-CAMPO        PIC  X(18)      VALUE SPACES.
           03  WRK-TABELA-ITEM REDEFINES WRK-TABELA-CAMPO.
               05  WRK-TAB-ITEM OCCURS  18      TIMES.
                   07  WRK-BYTE        PIC  X(01).

       01  WRK-CEDULA.
           03  WRK-ANO-CEDULA          PIC 9(04)         VALUE ZEROS.
           03  WRK-NUM-CEDULA          PIC 9(05)         VALUE ZEROS.
       01  WRK-CEDULA-R REDEFINES WRK-CEDULA PIC 9(09).

       01  WRK-CGC-CPF.
           03  WRK-NUM-CGC             PIC 9(09)         VALUE ZEROS.
           03  WRK-FIL-CGC             PIC 9(05)         VALUE ZEROS.

201504 01  WRK-CPSSOA-JURID            PIC  9(010)     COMP-3 VALUE
201504                                                  2269651.

202502 01  WRK-UNIDADE-ANIMAL          PIC X(01)         VALUE SPACES.
       01  WRK-CONTROLE                PIC X(02)         VALUE SPACES.
       01  WRK-INCONSIS                PIC X(01)         VALUE SPACES.
       01  WRK-INCONSIS-BYTE           PIC X(01)         VALUE SPACES.
       01  WRK-INCONS-AREAF            PIC X(01)         VALUE SPACES.
EG1216 01  WRK-INCONS-DAP              PIC X(01)         VALUE SPACES.
       01  WRK-INCONS-QTDEF            PIC X(01)         VALUE SPACES.
       01  WRK-INCONS-CAMPO            PIC X(01)         VALUE SPACES.
MI1008 01  WRK-INCONS-GRUPO            PIC X(01)         VALUE SPACES.
MI1008 01  WRK-INCONS-PORTE            PIC X(01)         VALUE SPACES.
       01  WRK-PROCESSA-TELA           PIC X(01)         VALUE SPACES.
       01  WRK-CONTINUACAO             PIC X(01)         VALUE SPACES.
       01  WRK-RECEITA-BRUTA           PIC X(01)         VALUE 'N'.

ACT181 01  WRK-MENS01                  PIC  X(79)   VALUE SPACES.

ACT181 01      FILLER.
ACT181     03  WRK-DDMMAAAA-COR        PIC 9(08).
ACT181     03  FILLER  REDEFINES  WRK-DDMMAAAA-COR.
ACT181         05  DIA-C               PIC 9(02).
ACT181         05  MES-C               PIC 9(02).
ACT181         05  ANO-C               PIC 9(04).
ACT181     03  WRK-AAAAMMDD-FIM        PIC 9(08).
ACT181     03  FILLER  REDEFINES  WRK-AAAAMMDD-FIM.
ACT181         05  ANO-F               PIC 9(04).
ACT181         05  MES-F               PIC 9(02).
ACT181         05  DIA-F               PIC 9(02).
ACT181     03  WRK-DDMMAAAA-FIM        PIC 9(08).
ACT181     03  FILLER  REDEFINES  WRK-DDMMAAAA-FIM.
ACT181         05  F-DIA               PIC 9(02).
ACT181         05  F-MES               PIC 9(02).
ACT181         05  F-ANO               PIC 9(04).

PW9931 01  WRK-DATA-CEDULA              PIC  X(010) VALUE SPACES.

ACT181*---------------------------------------------------------------*
ACT181***          AREAS AUXILIXARES DA POOL1205                      *
ACT181*---------------------------------------------------------------*

ACT181 01  WRK-DADOS-POOL1205.
ACT181     05  WRK-1205-DT-ENVIADA      PIC  9(008) VALUE ZEROS COMP-3.
ACT181     05  WRK-1205-OPCAO           PIC  X(001) VALUE SPACES.
ACT181     05  WRK-1205-DT-JULIANA      PIC  9(007) VALUE ZEROS COMP-3.
ACT181     05  WRK-1205-DT-EDITADA      PIC  X(010) VALUE SPACES.
ACT181     05  WRK-1205-DT-GREGORI      PIC  9(008) VALUE ZEROS.
ACT181     05  WRK-1205-DIA-SEMANA      PIC  X(013) VALUE SPACES.
ACT181     05  WRK-1205-MES-EXTENSO     PIC  X(009) VALUE SPACES.
ACT181     05  WRK-1205-DT-UT-ANTER     PIC  9(008) VALUE ZEROS.
ACT181     05  WRK-1205-DT-UT-POSTE     PIC  9(008) VALUE ZEROS.

ACT181 01  WRK-1205-MENSAGEM           PIC  X(50)    VALUE SPACES.

201504*---------------------------------------------------------------*
201504***          AREA PARA ACESSO AO CMRU2A0T
201504*---------------------------------------------------------------*
201504
201504 01  WRK-AREA-CMRU2A0T.
201504     05  WRK-CMRUWA0A-S-SQLCA        PIC X(136)      VALUE SPACES.
201504     05  WRK-CMRUWA0A-S-ERRO-AREA    PIC X(107)      VALUE SPACES.
201504     COPY   CMRUWA0A.
LMC
LMC   *---------------------------------------------------------------*
LMC   ***          AREA PARA ACESSO AO CMRU309C
LMC   *---------------------------------------------------------------*
LMC    01  WRK-CMRU309C.
LMC        COPY   'CMRUW09I'.
LMC
201504*---------------------------------------------------------------*
201504***          AREA PARA ACESSO AO CMRU2AKT
201504*---------------------------------------------------------------*
201504
201504*01  WRK-AREA-CMRU2AKT.
201504*****COPY   CMRUWAKC.
201504
HEX   *01  WRK-MSG-ERRO-TELA.
HEX   *    05  WRK-COD-MSG-AUX           PIC  X(008)     VALUE SPACES.
HEX   *    05  WRK-MSG-ERRO-AUX          PIC  X(071)     VALUE SPACES.
201504
      *---------------------------------------------------------------*
      ***          INCLUSAO DA POOL7100  -  TRATA ERRO
      *---------------------------------------------------------------*

       COPY POL7100C.

      *---------------------------------------------------------------*
      ***          AREA DE COMINICACAO COM MOD. MIDD9000
      *---------------------------------------------------------------*

       COPY 'I#MIDD01'.

HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       COPY 'I#HEXA01'.
HEXA  *
HEXA  *
      *---------------------------------------------------------------*
      *** DEFINICAO DA AREA PARA RET-CODE DIFERENTES DE ZEROS(CRUR236G)
      *---------------------------------------------------------------*
       01  WRK-ERRO-CRUR236G.
           03  FILLER                  PIC X(47)         VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA CRUR236G *'.
           03  FILLER                  PIC X(12)         VALUE
               '* RET.COD = '.
           03  WRK-RETURN-CODE1        PIC 9(02)BB       VALUE ZEROS.
           03  FILLER                  PIC X(08)         VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO1         PIC 9(02)         VALUE ZEROS.

      *---------------------------------------------------------------*
      *** DEFINICAO DA AREA PARA RET-CODE DIFERENTES DE ZEROS(POOL0660)
      *---------------------------------------------------------------*

       01  WRK-ERRO-POOL0660.
           03  FILLER                  PIC X(47)         VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA POOL0660 *'.
           03  FILLER                  PIC X(12)         VALUE
               '* RET.COD = '.
           03  WRK-RETURN-CODE         PIC 9(02)BB.
           03  FILLER                  PIC X(08)         VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO          PIC 9(02)         VALUE ZEROS.

PRI499*----------------------------------------------------------------*
PRI499 01  FILLER                      PIC  X(050)         VALUE
PRI499     '*** AREA DE ERRO DA CRUR2G0C ***'.
PRI499*----------------------------------------------------------------*
PRI499
PRI499 01  WRK-ERRO-CRUR2G0C.
PRI499     05  FILLER                  PIC  X(045)         VALUE
PRI499         '* RETURN-CODE DIFERENTE DE ZEROS NO CRUR2G0C '.
PRI499     05  FILLER                  PIC  X(012)         VALUE
PRI499         '- RET.COD = '.
PRI499     05  WRK-RETURN-CRUR2G0C     PIC  9(002)         VALUE ZEROS.
PRI499     05  FILLER                  PIC  X(011)         VALUE
PRI499         ' - LOCAL = '.
PRI499     05  WRK-LOCAL-CRUR2G0C      PIC  9(002)         VALUE ZEROS.
PRI499     05  FILLER                  PIC  X(002)         VALUE ' *'.

      *---------------------------------------------------------------*
      ***                  DEFINICAO DE ATRIBUTOS
      *---------------------------------------------------------------*

       01  FILLER.
           03  TAB-ATRIBUTOS.

      *****************************************************************
      *    BRILHANTE, DESPROT, COM POS. CURSOR, NUM                   *
      *                                         ---                   *
               05  WRK-49369           PIC S9(008) COMP VALUE +49369.
      *****************************************************************
      *    NORMAL, DESPROT, COM POS. CURSOR, NUM                      *
      *                                      ---                      *
               05  WRK-49361           PIC S9(008) COMP VALUE +49361.
      *****************************************************************
      *    PROTEGIDO (NUMERICO OU ALFA)                               *
      *               -------- -- ----                                *
               05  WRK-225             PIC S9(008) COMP VALUE +225.
      *****************************************************************
      *    BRILHANTE, DESPROT, COM POS. CURSOR, ALFA                  *
      *                                         ----                  *
               05  WRK-49353           PIC S9(008) COMP VALUE +49353.
EG1216         05  FILLER              REDEFINES WRK-49353.
EG1216             10 FILLER           PIC  X(002).
EG1216             10 WRK-49353-X      PIC  X(002).
      *****************************************************************
      *    PROTEGIDO, BRILHANTE (NUMERICO OU ALFA)                    *
      *                          -------- -- ----                     *
               05  WRK-169             PIC S9(008) COMP VALUE +169.
      *****************************************************************
      *    PROTEGIDO, BRILHANTE, CURSOR (NUMERICO OU ALFA)            *
      *                                  -------- -- ----             *
               05  WRK-49385           PIC S9(008) COMP VALUE +49385.
      *****************************************************************
      *    NORMAL, DESPROT, CURSOR, ALFA                              *
      *                             ----                              *
               05  WRK-49345           PIC S9(008) COMP VALUE +49345.
      *****************************************************************
      *    NORMAL, DESPROT, NUM                                       *
      *                                                               *
               05  WRK-209             PIC S9(008) COMP VALUE +209.
      *****************************************************************
      *    NORMAL, DESPROT, ALFA                                      *
      *                                                               *
               05  WRK-193             PIC S9(008) COMP VALUE +193.
      *****************************************************************

           03  FILLER REDEFINES TAB-ATRIBUTOS OCCURS 9 TIMES
                                INDEXED BY IND-ATRIB.
               05  TAB-ATRIB           PIC S9(008) COMP.
201503         05  FILLER REDEFINES TAB-ATRIB.
201503             07  FILLER          PIC  X(002).
201503             07  WRK-TAB-ATRIB   PIC S9(004) COMP.

      *---------------------------------------------------------------*
      ***                  TABELA DE MENSAGENS
      *---------------------------------------------------------------*

       01  FILLER.
HEX   *    05  WRK-M2                  PIC X(79)         VALUE
HEX   *    'USUARIO NAO AUTORIZADO'.
           05  WRK-M3                  PIC X(79)         VALUE
           'ATUALIZE O CAMPO AREA/QTDE NO RURC (ALT.EMPREENDIMENTOS ***
      -    'AVISE O DECRAN ***)'.
           05  WRK-M4                  PIC X(79)         VALUE
           'NA INCLUSAO, PREENCHIMENTO OBRIGATORIO DA AGENCIA E CONTA'.
           05  WRK-M5.
               10  FILLER              PIC X(61)         VALUE
               'TAXA DE JUROS/CM NAO CADASTRADA NO RURC, ORIG/FIN/TP/PRO
      -    'D = '.
               10  M5-CAMPO1           PIC 9(03).
               10  FILLER              PIC X(01) VALUE '/'.
               10  M5-CAMPO2           PIC 9(03).
               10  FILLER              PIC X(01) VALUE '/'.
               10  M5-CAMPO3           PIC 9(03).
               10  FILLER              PIC X(01) VALUE '/'.
               10  M5-CAMPO4           PIC 9(03).
HEX   *    05  WRK-M6                  PIC X(79)         VALUE
HEX   *    'FINALIDADE NAO CADASTRADA - VERIFIQUE !!!'.
           05  WRK-M7.
               10  FILLER              PIC X(21)         VALUE
               'FINAL./ORI.REC/ATIV. '.
               10  M7-CAMPO1           PIC 9(03).
               10  FILLER              PIC X(01) VALUE '/'.
               10  M7-CAMPO2           PIC 9(03).
               10  FILLER              PIC X(01) VALUE '/'.
               10  M7-CAMPO3           PIC 9(03).
               10  FILLER              PIC X(27)         VALUE
               ' NAO CAD. P/ OBTER A ATIV.'.
           05  WRK-M8                  PIC X(79)         VALUE
           'FIN. P/AREA ACIMA DE 1000 HA, NECESSITA LAUDO MEDICAO - <ENT
      -    'ER> PARA CONTINUAR'.
           05  WRK-M9.
               10  FILLER              PIC X(17)         VALUE
               'TIPO DE PRODUTOR '.
               10  M9-CAMPO            PIC 9(03).
               10  FILLER              PIC X(27)         VALUE
               ' NAO CADASTRADO - VERIFIQUE'.
           05  WRK-M10.
               10  FILLER              PIC X(30)         VALUE
               'AREA DE NAVEGACAO DO TERMINAL '.
               10  M10-CAMPO           PIC X(08).
               10  FILLER              PIC X(26)         VALUE
               ' NAO ENCONTRADO'.
           05  WRK-M11                 PIC X(79)         VALUE
           'TP.EXPLOR DEVE SER: ARRENDAMENTO,PROPRIA,COMODATO,PARCERIA,M
      -    'EEIRO OU CONDOMINIO'.
HEX   *    05  WRK-M12                 PIC X(79)         VALUE
HEX   *    'CEDULA BLOQUEADA - <ENTER> P/ ALTERAR OU <PF11> P/ APROV. DC
HEX   *    'R'.
           05  WRK-M13                 PIC X(79)         VALUE
MI1608     'SE O PROPONENTE NAO FOR O TITULAR DA CONTA, DIGITE O CNPJ/CP
MI1608-    'F OU NOME'.
HEX   *    05  WRK-M14                 PIC X(79)         VALUE
HEX   *    'OPCAO EM DESENVOLVIMENTO - EM CASO DE DUVIDA CONTATE O DEF'.
HEX   *    05  WRK-M15                 PIC X(79)         VALUE
HEX   *    'POSICIONE O CURSOR NO PRODUTO, ORIGEM OU DENOMINACAO'.
HEX   *    05  WRK-M16                 PIC X(79)         VALUE
HEX   *    'ERRO NA CHAMADA AO MODULO RUEC7706'.
           05  WRK-M17                 PIC X(79)         VALUE
           'CAMPO DE PREENCHIMENTO OBRIGATORIO PARA ESTE PRODUTO'.
           05  WRK-M18                 PIC X(79)         VALUE
           'CONTEUDO DO CAMPO INVALIDO'.
HEX   *    05  WRK-M19                 PIC X(79)         VALUE
HEX   *    'POSICIONE O CURSOR NA OPCAO DESEJADA E TECLE <ENTER>'.
           05  WRK-M20                 PIC X(79)         VALUE
           'AGENCIA DEVE SER IGUAL AO CODIGO DO DEPARTAMENTO'.
HEX   *    05  WRK-M21                 PIC X(79)         VALUE
HEX   *    'MENSAGEM LIMPA'.
HEX   *    05  WRK-M22                 PIC X(79)         VALUE
HEX   *    'TIPO PRODUTOR NAO ATIVO - <ENTER> P/ ALTERAR OU <PF11> P/ AP
HEX   *    'ROV. DEF'.
HEX   *    05  WRK-M23                 PIC X(79)         VALUE
HEX   *    'TITULAR IMPEDIDO - <ENTER> P/ ALTERAR OU <PF11> P/ APROV. DC
HEX   *    'R'.
           05  WRK-M24                 PIC X(79)         VALUE
MI1608     'CNPJ/CPF INVALIDO'.
           05  WRK-M25.
               10  FILLER              PIC X(18)         VALUE
               'CEDULA BLOQUEADA, '.
               10  M25-CAMPO           PIC X(38)         VALUE SPACES.
               10  FILLER              PIC X(23)         VALUE
               ' ,ALTERE OU CONTATE DEF'.
           05  WRK-M26.
               10  M26-CAMPO-A         PIC X(23).
               10  FILLER              PIC X(45)         VALUE
               ' ,ALTERE E TECLE <ENTER> OU <PF11> P/ APROV. '.
               10  M26-CAMPO-B         PIC X(11).
           05  WRK-M27                 PIC X(79)         VALUE
           'OBRIGATORIO DIGITACAO DA VIRGULA DECIMAL - VERIFIQUE E TECLE
      -    ' <ENTER>'.
           05  WRK-M28.
               10  FILLER              PIC X(37)         VALUE
               'CODIGO DE ATIVIDADE NAO ENCONTRADO = '.
               10  M28-CAMPO           PIC X(11).
               10  FILLER              PIC X(31).
           05  WRK-M29.
               10  M29-CAMPO-A         PIC X(40).
               10  FILLER              PIC X(26)         VALUE
               ' ,ALTERE OU PF11 P/APROV. '.
               10  M29-CAMPO-B         PIC X(11).
MI1008     05  WRK-M30                 PIC X(79)         VALUE
MI1008     'GRUPO, INFORME 1 OU 2'.
MI1008     05  WRK-M31                 PIC X(79)         VALUE
MI1008     'PORTE, INFORME "PEQUENO" OU "GRANDE" '.
MI1008     05  WRK-M32.
MI1008         10  FILLER              PIC X(62)         VALUE
MI1008     'NAO EXISTE COOPERATIVA CAD.TAB.RURCV022, PARA A CHAVE: GRUPO
MI1008-    ': '.
MI1008         10  WRK-M32-GRUPO       PIC X(01).
MI1008         10  FILLER              PIC X(08)         VALUE
MI1008     ' PORTE: '.
MI1008         10  WRK-M32-PORTE       PIC X(08).

POL097     05  WRK-M331                PIC X(79)         VALUE
POL097     'INCONSISTENCIA NA CHAMADA AO MODULO RUEC7722 PESQ IMOV. 1'.

POL097     05  WRK-M332                PIC X(79)         VALUE
POL097     'INCONSISTENCIA NA CHAMADA AO MODULO RUEC7722 PESQ IMOV. 2'.

POL097     05  WRK-M333                PIC X(79)         VALUE
POL097     'INCONSISTENCIA NA CHAMADA AO MODULO RUEC7722 PESQ IMOV. 3'.

POL097     05  WRK-M34A                PIC X(79)         VALUE
POL097     'INCONSISTENCIA NA CHAMADA AO MODULO RUEC7718 VALIDAC.CAD'.

POL097     05  WRK-M34                 PIC X(79)         VALUE
POL097     'INCONSISTENCIA NA CHAMADA AO MODULO RUEC7720 PESQ. DATAS'.

POL097     05  WRK-M35                 PIC X(79)         VALUE
POL097     'CCLUB INEXISTENTE NO SISTEMA CADU'.

POL097     05  WRK-M36                 PIC X(79)         VALUE
POL097     'CCLUB INEXISTENTE NO SISTEMA RUEC'.

POL097     05  WRK-M37                 PIC X(79)         VALUE
POL097     'CADASTRO RURAL COM ERROS - CORRIGIR'.

POL097     05  WRK-M38                 PIC X(79)         VALUE
POL097     'CADASTRO RURAL INEXISTENTE P/ PESSOA SELECIONADA'.

PW6062     05  WRK-M39                 PIC X(79)         VALUE
PW6062     'USUARIO NAO AUTORIZADO A ACESSAR ESTA AGENCIA'.

PW9931     05  WRK-M40                 PIC X(79)         VALUE
PW9931     'CODIGO DA MODALIDADE NAO ENCONTRADO'.
PW9931     05  WRK-M41                 PIC X(79)         VALUE
PW9931     'CODIGO DA VARIEDADE NAO ENCONTRADO'.
PW9931     05  WRK-M42                 PIC X(79)         VALUE
PW9931     'CODIGO DA CESTA NAO ENCONTRADO'.
PW9931     05  WRK-M43                 PIC X(79)         VALUE
EG1015     'CODIGO DE ZONEAMENTO DIFERENTE DE S/N/A'.
PW9931     05  WRK-M44                 PIC X(79)         VALUE
EG1015     'CODIGO DE CONSORCIO DIFERENTE DE S/N/A'.
PW9931     05  WRK-M45                 PIC X(79)         VALUE
PW9931     'CODIGO DA AGROPECUARIA NAO ENCONTRADO'.
PW9931     05  WRK-M46                 PIC X(79)         VALUE
PW9931     'CODIGO DA INTEGRACAO NAO ENCONTRADO'.
PW9931     05  WRK-M47                 PIC X(79)         VALUE
MJ0124     'CODIGO DE GRAO/SEMENTE/MUDAS DIFERENTE DE G/S/M/SI'.
PW9931     05  WRK-M48                 PIC X(79)         VALUE
PW9931     'CODIGO DA IRRIGACAO NAO ENCONTRADO'.
PW9931     05  WRK-M49                 PIC X(79)         VALUE
PW9931     'CODIGO DO CULTIVO NAO ENCONTRADO'.
PW9931     05  WRK-M50                 PIC X(79)         VALUE
PW9931     'CODIGO DE CICLO NAO ENCONTRADO'.
PW9931     05  WRK-M51                 PIC X(79)         VALUE
PW9931     'PREENCHA ZONEAMENTO E CONSORCIO E TECLE ENTER'.
PW9931     05  WRK-M53                 PIC X(79)         VALUE
PW9931     'SEM CORRESPONDENTE NO SISTEMA DE PRODUCAO'.
PW9931     05  WRK-M54                 PIC X(79)         VALUE
PW9931     'PREENCHA RECEITA BRUTA E TECLE ENTER'.
201504     05  WRK-M55                 PIC X(79)         VALUE
201504     'AGENCIA SEM PERMISSAO, UTILIZE O SISTEMA CRUR'.
201504*****05  WRK-M56                 PIC X(79)         VALUE
201504*****'FICHA EM MANUTENCAO NO SISTEMA CRUR             '.
201504     05  WRK-M57.
201504         07 FILLER               PIC X(40)         VALUE
201504            'PROBLEMAS NO ACESSO AO CMRUWA0T - RC = '.
201504         07 WRK-M57-RC           PIC 9(02)         VALUE ZEROS.
LMC        05  WRK-M58                 PIC X(79)         VALUE
LMC        'CODIGO DO BENEFICIARIO NAO ENCONTRADO'.
WI1121     05  WRK-M59                 PIC X(79)         VALUE
WI1121     'DADOS DE ZONEAMENTO AGRICOLA DE RISCO CLIMATICO NAO INFORMAD
WI1121-    'DOS'.
PRI499     05  WRK-M60                 PIC X(79)         VALUE
PRI499     'CODIGO DE CLIMA NAO ENCONTRADO'.
0524       05  WRK-M61                 PIC X(79)         VALUE
0524       'CODIGO DE MANEJO NAO ENCONTRADO'.
STEFA2     05  WRK-M62                 PIC X(79)         VALUE
STEFA2     'EMPREENDIMENTO FINANC EXIGE PREENCHIMENTO DA TELA DE UNID AN
STEFA2-    'IMAL - TECLE PF6'.
STEFA2*   X05  WRK-M63                 PIC X(79)         VALUE
STEFA2*   X'EMPREENDIMENTO FINANCIADO NAO EXISTE, SOLICITAR CADASTRAMENT
STEFA2*   X'O'.
      *--------------------------------------------------------*
      * DEFINICAO DAS AREAS UTILIZADAS NO MODULO    "POOL7600"
      *--------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DT-JULIANA          PIC 9(05) COMP-3.
           05  WRK-DT-AAMMDD           PIC 9(07) COMP-3.
           05  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3.
           05  WRK-TI-HHMMSS           PIC 9(07) COMP-3.
           05  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3.
           05  WRK-TIMESTAMP           PIC X(20).

       01  FILLER.
           05  WRK-DATA                PIC 9(09) VALUE ZEROS.
           05  WRK-DATA-R  REDEFINES WRK-DATA.
               10  FILLER              PIC X(01).
               10  ANO                 PIC 9(04).
               10  MES                 PIC 9(02).
               10  DIA                 PIC 9(02).
           05  WRK-DATA-RED.
               10  DIA                 PIC 9(02).
               10  FILLER              PIC X(01) VALUE '/'.
               10  MES                 PIC 9(02).
               10  FILLER              PIC X(01) VALUE '/'.
               10  ANO                 PIC 9(04).
           05  WRK-DATA-DB2.
               10  DIA                 PIC 9(02).
               10  FILLER              PIC X(01) VALUE '.'.
               10  MES                 PIC 9(02).
               10  FILLER              PIC X(01) VALUE '.'.
               10  ANO                 PIC 9(04).

      /----------------------------------------------------------------*
      ***  TELA RUECT700            - INPUT
      *----------------------------------------------------------------*
       01  INPUT-T700.
           02  FILLER                           PIC  X(18).
           02  INP-PFK-T700                     PIC  X(01).
           02  INP-SENHAS-T700                  PIC  X(37).
           02  INP-NOPCAO-T700                  PIC  X(02).
           02  INP-POSI-T700.
               03  INP-LIN-T700                 PIC S9(04) COMP.
               03  INP-COL-T700                 PIC S9(04) COMP.
           02  INP-DADOS-T700.
201503         03  INP-ANOME-T700               PIC  X(40) VALUE SPACES.
201503         03  INP-AGRUPO-T700              PIC  X(01) VALUE SPACES.
201503         03  INP-FASE-T700                PIC  X(01) VALUE SPACES.
201503         03  INP-AAGENC-T700-X.
201503             05  INP-AAGENC-T700          PIC  9(05) VALUE ZEROS.
201503         03  INP-ACONTA-T700-X.
201503             05  INP-ACONTA-T700          PIC  9(07) VALUE ZEROS.
201503         03  INP-ACGCNUM-T700-X.
201503             05  INP-ACGCNUM-T700         PIC  9(09) VALUE ZEROS.
201503         03  INP-ACGCFIL-T700-X.
201503             05  INP-ACGCFIL-T700         PIC  9(04) VALUE ZEROS.
201503         03  INP-APORTE-T700              PIC  X(07) VALUE SPACES.
201503         03  WRK-INP-ACODMOD-T700-X.
201503             05  WRK-INP-ACODMOD-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-INP-ACODVAR-T700-X.
201503             05  WRK-INP-ACODVAR-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-INP-ACODCES-T700-X.
201503             05  WRK-INP-ACODCES-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-INP-AZONEAM-T700         PIC  X(01) VALUE SPACES.
201503         03  WRK-INP-ACODAGR-T700-X.
201503             05  WRK-INP-ACODAGR-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-INP-ACODINT-T700-X.
201503             05  WRK-INP-ACODINT-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-INP-ACODIRR-T700-X.
201503             05  WRK-INP-ACODIRR-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-INP-ACONSOR-T700         PIC  X(01) VALUE SPACES.
201503         03  INP-ANPROD1-T700             PIC  X(40) VALUE SPACES.
201503         03  INP-AORIGCD-T700-X.
201503             05  INP-AORIGCD-T700         PIC  9(03) VALUE ZEROS.
201503         03  INP-AAREAF-T700-X.
201503             05  INP-AAREAF-T700       PIC  9(12)V99 VALUE ZEROS.
201503         03  INP-AATIVCD-T700-X.
201503             05  INP-AATIVCD-T700         PIC  9(03) VALUE ZEROS.
201503         03  INP-POSPROC-T700-X.
201503             05  INP-POSPROC-T700      PIC  9(02)V9  VALUE ZEROS.
201503         03  INP-AUXDCR-T700-X.
201503             05  INP-AUXDCR-T700          PIC  9(01) VALUE ZEROS.
201503         03  INP-ACTRL-T700-X.
201503             05  INP-ACTRL-T700           PIC  9(02) VALUE ZEROS.
201503         03  WRK-INP-TPREC-T700           PIC  9(01) VALUE ZEROS.
201503         03  INP-ATTRCA-T700              PIC  X(01) VALUE SPACES.
201503         03  WRK-INP-ACODGRS-T700-X.
201503             05  WRK-INP-ACODGRS-T700     PIC  9(01) VALUE ZEROS.
201503         03  WRK-INP-ACODCUL-T700-X.
201503             05  WRK-INP-ACODCUL-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-INP-ACODCIC-T700-X.
201503             05  WRK-INP-ACODCIC-T700     PIC  9(03) VALUE ZEROS.
PRI499         03  WRK-INP-ACODCLI-T700-X.
PRI499             05  WRK-INP-ACODCLI-T700     PIC  9(02) VALUE ZEROS.
201503         03  INP-AIMOVEL-T700             PIC  X(25) VALUE SPACES.
201503         03  INP-AQTDEF-T700-X.
201503             05  INP-AQTDEF-T700          PIC  9(12)V99
201503                                                     VALUE ZEROS.
201503         03  INP-ATPEXP-T700              PIC  X(20) VALUE SPACES.
201503         03  INP-ACAMPO-T700-X.
201503             05  INP-ACAMPO-T700          PIC  9(12)V99
201503                                                     VALUE ZEROS.
201503         03  WRK-INP-ARECBR-T700-X.
201503             05  WRK-INP-ARECBR-T700      PIC  9(16)V99
201503                                                     VALUE ZEROS.
201503         03  WRK-INP-ACPRDTTL-T700-X.
201503             05  WRK-INP-ACPRDTTL-T700    PIC  9(12)V99
201503                                                     VALUE ZEROS.
LMC            03  WRK-INP-ATPOBEN-T700-X.
LMC                05  WRK-INP-ATPOBEN-T700     PIC  9(03) VALUE ZEROS.
               03  INP-AGENC-T700-X.
                   05  INP-AGENC-T700           PIC  9(05).
               03  INP-CONTA-T700-X.
                   05  INP-CONTA-T700           PIC  9(07).
               03  INP-CGC-CPF-T700-X.
                   05  INP-CGCNUM-T700-X.
                       07  INP-CGCNUM-T700      PIC  9(09).
                   05  INP-CGCFIL-T700-X.
                       07  INP-CGCFIL-T700      PIC  9(04).
                   05  INP-CTRL-T700-X.
                       07  INP-CTRL-T700        PIC  9(02).
               03  INP-NOME-T700                PIC  X(40).
               03  INP-SITUAC-T700              PIC  X(11).
               03  INP-MENSIT-T700              PIC  X(27).
               03  INP-NPROD1-T700              PIC  X(40).
               03  INP-HIFEN-T700               PIC  X(01).
               03  INP-NPROD2-T700              PIC  X(22).
               03  INP-ORIGCD-T700-X.
                   05  INP-ORIGCD-T700          PIC  9(03).
               03  INP-ORIGDS-T700              PIC  X(20).
               03  INP-ATIVCD-T700-X.
                   05  INP-ATIVCD-T700          PIC  9(03).
               03  INP-ATIVDS-T700              PIC  X(16).
PW9931         03  WRK-INP-CODMOD-T700-X.
PW9931             05  WRK-INP-CODMOD-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-INP-DESMOD-T700          PIC  X(20) VALUE SPACES.
PW9931         03  WRK-INP-CODVAR-T700-X.
PW9931             05  WRK-INP-CODVAR-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-INP-DESVAR-T700          PIC  X(22) VALUE SPACES.
PW9931         03  WRK-INP-CODCES-T700-X.
PW9931             05  WRK-INP-CODCES-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-INP-DESCES-T700          PIC  X(20) VALUE SPACES.
PW9931         03  WRK-INP-ZONEAM-T700          PIC  X(01) VALUE SPACES.
PW9931         03  WRK-INP-CONSOR-T700          PIC  X(01) VALUE SPACES.
PW9931         03  WRK-INP-CODAGR-T700-X.
PW9931             05  WRK-INP-CODAGR-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-INP-DESAGR-T700          PIC  X(15) VALUE SPACES.
PW9931         03  WRK-INP-CODINT-T700-X.
PW9931             05  WRK-INP-CODINT-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-INP-DESINT-T700          PIC  X(14) VALUE SPACES.
PW9931         03  WRK-INP-CODGRS-T700-X.
PW9931             05  WRK-INP-CODGRS-T700      PIC  9(01) VALUE ZEROS.
PW9931         03  WRK-INP-DESGRS-T700          PIC  X(15) VALUE SPACES.
PW9931         03  WRK-INP-CODIRR-T700-X.
PW9931             05  WRK-INP-CODIRR-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-INP-DESIRR-T700          PIC  X(14) VALUE SPACES.
PW9931         03  WRK-INP-CODCUL-T700-X.
PW9931             05  WRK-INP-CODCUL-T700      PIC  9(03) VALUE ZEROS.
PRI499         03  WRK-INP-DESCUL-T700          PIC  X(13) VALUE SPACES.
PW9931         03  WRK-INP-CODCIC-T700-X.
PW9931             05  WRK-INP-CODCIC-T700      PIC  9(03) VALUE ZEROS.
PRI499         03  WRK-INP-DESCIC-T700          PIC  X(12) VALUE SPACES.
PRI499         03  WRK-INP-CODCLI-T700-X.
PRI499             05  WRK-INP-CODCLI-T700      PIC  9(02) VALUE ZEROS.
PRI499         03  WRK-INP-DESCLI-T700          PIC  X(12) VALUE SPACES.
0524           03  WRK-INP-CODMAN-T700-X.
0524               05  WRK-INP-CODMAN-T700      PIC  9(02) VALUE ZEROS.
0524           03  WRK-INP-DESMAN-T700          PIC  X(12) VALUE SPACES.
               03  INP-AREAF-T700-X.
                   05  INP-AREAF-T700           PIC  9(12)V99
                                                           VALUE ZEROS.
               03  INP-QTDEF-T700-X.
                   05  INP-QTDEF-T700           PIC  9(12)V99
                                                           VALUE ZEROS.
               03  INP-LITERAL-T700             PIC  X(21) VALUE SPACES.
               03  INP-CAMPO-T700-X.
                   05  INP-CAMPO-T700           PIC  9(12)V99
                                                           VALUE ZEROS.
201503         03  WRK-INP-LPRDTTL-T700         PIC  X(21) VALUE SPACES.
201503         03  WRK-INP-CPRDTTL-T700-X.
201503             05  WRK-INP-CPRDTTL-T700     PIC  9(12)V99
201503                                                     VALUE ZEROS.
MI1608         03  INP-LITER2-T700              PIC  X(15) VALUE SPACES.
MI1608         03  INP-GRUPO-T700               PIC  X(01) VALUE SPACES.
MI1608         03  INP-LITER3-T700              PIC  X(06) VALUE SPACES.
MI1608         03  INP-PORTE-T700               PIC  X(07) VALUE SPACES.
               03  INP-TPEXP-T700               PIC  X(20) VALUE SPACES.
               03  INP-IMOVEL-T700              PIC  X(25) VALUE SPACES.
PW9931         03  WRK-INP-CTIPCE-T700-X.
PW9931             05  WRK-INP-CTIPCE-T700      PIC  9(01) VALUE ZEROS.
PW9931         03  WRK-INP-DTIPCE-T700          PIC  X(15) VALUE SPACES.
PW9931         03  WRK-INP-RECBR-T700-X.
PW9931             05  WRK-INP-RECBR-T700       PIC  9(16)V99
PW9931                                                     VALUE ZEROS.
LMC            03  WRK-INP-TPOBEN-T700-X.
.                  05  WRK-INP-TPOBEN-T700      PIC  9(03) VALUE ZEROS.
.              03  WRK-INP-DESCBE-T700          PIC  X(50) VALUE SPACES.
LMC
EG1216         03  WRK-INP-NRODAP-T700          PIC  X(25) VALUE SPACES.
BR0118         03  WRK-INP-GPPRONA-T700         PIC  X(03) VALUE SPACES.
EG1216         03  WRK-INP-VALDTD-T700          PIC  X(02) VALUE SPACES.
EG1216         03  FILLER REDEFINES WRK-INP-VALDTD-T700.
EG1216             05 WRK-INP-VALDTD-T700-R     PIC  9(02).
EG1216         03  WRK-INP-VALDTM-T700          PIC  X(02) VALUE SPACES.
EG1216         03  FILLER REDEFINES WRK-INP-VALDTM-T700.
EG1216             05 WRK-INP-VALDTM-T700-R     PIC  9(02).
EG1216         03  WRK-INP-VALDTA-T700          PIC  X(04) VALUE SPACES.
EG1216         03  FILLER REDEFINES WRK-INP-VALDTA-T700.
EG1216             05 WRK-INP-VALDTA-T700-R     PIC  9(04).
201503*    02  FILLER REDEFINES INP-DADOS-T700.
201503*        03  WRK-INP-ESCONDIDOS           PIC X(277).
PRI499*        03  WRK-INP-ESCONDIDOS           PIC X(280).
201503*        03  FILLER                       PIC X(678).
PRI499*        03  FILLER                       PIC X(692).
201503     02  INP-MENSAG-T700                  PIC  X(79).

      /----------------------------------------------------------------*
      ***  TELA RUECT700            - OUTPUT
      *----------------------------------------------------------------*
       01  OUTPUT-T700.
PRI499     02  OUT-LL-T700                  PIC S9(04) COMP VALUE +1194.
           02  OUT-ZZ-T700                  PIC  9(04) COMP VALUE  0.
           02  OUT-SENHAS-T700                  PIC  X(37).
           02  OUT-NOPCAO-ATTR-T700             PIC  9(04) COMP.
           02  OUT-NOPCAO-T700                  PIC  X(02).
           02  OUT-DATA-T700                    PIC  X(10).
           02  OUT-DADOS-T700.
201503         03  OUT-ANOME-T700               PIC  X(40) VALUE SPACES.
201503         03  OUT-AGRUPO-T700              PIC  X(01) VALUE SPACES.
201503         03  OUT-FASE-T700                PIC  X(01) VALUE SPACES.
201503         03  OUT-AAGENC-T700-X.
201503             05  OUT-AAGENC-T700          PIC  9(05) VALUE ZEROS.
201503         03  OUT-ACONTA-T700-X.
201503             05  OUT-ACONTA-T700          PIC  9(07) VALUE ZEROS.
201503         03  OUT-ACGCNUM-T700-X.
201503             05  OUT-ACGCNUM-T700         PIC  9(09) VALUE ZEROS.
201503         03  OUT-ACGCFIL-T700-X.
201503             05  OUT-ACGCFIL-T700         PIC  9(04) VALUE ZEROS.
201503         03  OUT-APORTE-T700              PIC  X(07) VALUE SPACES.
201503         03  WRK-OUT-ACODMOD-T700-X.
201503             05  WRK-OUT-ACODMOD-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-OUT-ACODVAR-T700-X.
201503             05  WRK-OUT-ACODVAR-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-OUT-ACODCES-T700-X.
201503             05  WRK-OUT-ACODCES-T700     PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-OUT-AZONEAM-T700         PIC  X(01) VALUE SPACES.
201503         03  WRK-OUT-ACODAGR-T700-X.
201503             05  WRK-OUT-ACODAGR-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-OUT-ACODINT-T700-X.
201503             05  WRK-OUT-ACODINT-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-OUT-ACODIRR-T700-X.
201503             05  WRK-OUT-ACODIRR-T700     PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-OUT-ACONSOR-T700         PIC  X(01) VALUE SPACES.
201503         03  OUT-ANPROD1-T700             PIC  X(40) VALUE SPACES.
201503         03  OUT-AORIGCD-T700-X.
201503             05  OUT-AORIGCD-T700         PIC  9(03) VALUE ZEROS.
201503         03  OUT-AAREAF-T700-X.
201503             05  OUT-AAREAF-T700      PIC  9(12)V99  VALUE ZEROS.
201503         03  OUT-AATIVCD-T700-X.
201503             05  OUT-AATIVCD-T700         PIC  9(03) VALUE ZEROS.
201503         03  OUT-POSPROC-T700-X.
201503             05  OUT-POSPROC-T700     PIC  9(02)V9   VALUE ZEROS.
201503         03  OUT-AUXDCR-T700-X.
201503             05  OUT-AUXDCR-T700          PIC  9(01) VALUE ZEROS.
201503         03  OUT-ACTRL-T700-X.
201503             05  OUT-ACTRL-T700           PIC  9(02) VALUE ZEROS.
201503         03  WRK-OUT-TPREC-T700           PIC  9(01) VALUE ZEROS.
201503         03  OUT-ATTRCA-T700              PIC  X(01) VALUE SPACES.
201503         03  WRK-OUT-ACODGRS-T700-X.
201503             05  WRK-OUT-ACODGRS-T700     PIC  9(01) VALUE ZEROS.
201503         03  WRK-OUT-ACODCUL-T700-X.
201503             05  WRK-OUT-ACODCUL-T700     PIC  9(03) VALUE ZEROS.
201503         03  WRK-OUT-ACODCIC-T700-X.
201503             05  WRK-OUT-ACODCIC-T700     PIC  9(03) VALUE ZEROS.
PRI499         03  WRK-OUT-ACODCLI-T700-X.
PRI499             05  WRK-OUT-ACODCLI-T700     PIC  9(02) VALUE ZEROS.
201503         03  OUT-AIMOVEL-T700             PIC  X(25) VALUE SPACES.
201503         03  OUT-AQTDEF-T700-X.
201503             05  OUT-AQTDEF-T700      PIC  9(12)V99  VALUE ZEROS.
201503         03  OUT-ATPEXP-T700              PIC  X(20) VALUE SPACES.
201503         03  OUT-ACAMPO-T700-X.
201503             05  OUT-ACAMPO-T700      PIC  9(12)V99  VALUE ZEROS.
201503         03  WRK-OUT-ARECBR-T700-X.
201503             05  WRK-OUT-ARECBR-T700  PIC  9(16)V99  VALUE ZEROS.
201503         03  WRK-OUT-ACPRDTTL-T700-X.
201503             05  WRK-OUT-ACPRDTTL-T700    PIC  9(12)V99
201503                                                     VALUE ZEROS.
LMC            03  WRK-OUT-ATPOBEN-T700-X.
LMC                05  WRK-OUT-ATPOBEN-T700     PIC  9(03) VALUE ZEROS.
               03  OUT-AGENC-ATTR-T700          PIC  9(04) COMP.
               03  OUT-AGENC-T700-X.
                   05  OUT-AGENC-T700           PIC  9(05).
               03  OUT-CONTA-ATTR-T700          PIC  9(04) COMP.
               03  OUT-CONTA-T700-X.
                   05  OUT-CONTA-T700           PIC  9(07).
               03  OUT-CGCNUM-ATTR-T700         PIC  9(04) COMP.
               03  OUT-CGCNUM-T700-X.
                   05  OUT-CGCNUM-T700          PIC  9(09).
               03  OUT-CGCFIL-ATTR-T700         PIC  9(04) COMP.
               03  OUT-CGCFIL-T700-X.
                   05  OUT-CGCFIL-T700          PIC  9(04).
               03  OUT-CTRL-ATTR-T700           PIC  9(04) COMP.
               03  OUT-CTRL-T700-X.
                   05  OUT-CTRL-T700            PIC  9(02).
               03  OUT-NOME-ATTR-T700           PIC  9(04) COMP.
               03  OUT-NOME-T700                PIC  X(40).
               03  OUT-SITUAC-T700              PIC  X(11).
               03  OUT-MENSIT-T700              PIC  X(27).
               03  OUT-NPROD1-ATTR-T700         PIC  9(04) COMP.
               03  OUT-NPROD1-T700              PIC  X(40).
               03  OUT-HIFEN-T700               PIC  X(01).
               03  OUT-NPROD2-T700              PIC  X(22).
               03  OUT-ORIGCD-ATTR-T700         PIC  9(04) COMP.
               03  OUT-ORIGCD-T700-X.
                   05  OUT-ORIGCD-T700          PIC  9(03).
               03  OUT-ORIGDS-T700              PIC  X(20).
               03  OUT-ATIVCD-ATTR-T700         PIC  9(04) COMP.
               03  OUT-ATIVCD-T700-X.
                   05  OUT-ATIVCD-T700          PIC  9(03).
               03  OUT-ATIVDS-T700              PIC  X(16).
PW9931         03  WRK-OUT-CODMOD-ATTR-T700     PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-CODMOD-T700-X.
PW9931             05  WRK-OUT-CODMOD-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-OUT-DESMOD-T700          PIC  X(20) VALUE SPACES.
PW9931         03  WRK-OUT-CODVAR-ATTR-T700     PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-CODVAR-T700-X.
PW9931             05  WRK-OUT-CODVAR-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-OUT-DESVAR-T700          PIC  X(22) VALUE SPACES.
PW9931         03  WRK-OUT-CODCES-ATTR-T700     PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-CODCES-T700-X.
PW9931             05  WRK-OUT-CODCES-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-OUT-DESCES-T700          PIC  X(20) VALUE SPACES.
PW9931         03  WRK-OUT-ZONEAM-ATTR-T700     PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-ZONEAM-T700          PIC  X(01) VALUE SPACES.
PW9931         03  WRK-OUT-CONSOR-ATTR-T700     PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-CONSOR-T700          PIC  X(01) VALUE SPACES.
PW9931         03  WRK-OUT-CODAGR-ATTR-T700     PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-CODAGR-T700-X.
PW9931             05  WRK-OUT-CODAGR-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-OUT-DESAGR-T700          PIC  X(15) VALUE SPACES.
PW9931         03  WRK-OUT-CODINT-ATTR-T700     PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-CODINT-T700-X.
PW9931             05  WRK-OUT-CODINT-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-OUT-DESINT-T700          PIC  X(14) VALUE SPACES.

PW9931         03  WRK-OUT-CODGRS-ATTR-T700     PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-CODGRS-T700-X.
PW9931             05  WRK-OUT-CODGRS-T700      PIC  9(01) VALUE ZEROS.
PW9931         03  WRK-OUT-DESGRS-T700          PIC  X(15) VALUE SPACES.
PW9931         03  WRK-OUT-CODIRR-ATTR-T700     PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-CODIRR-T700-X.
PW9931             05  WRK-OUT-CODIRR-T700      PIC  9(03) VALUE ZEROS.
PW9931         03  WRK-OUT-DESIRR-T700          PIC  X(14) VALUE SPACES.

PW9931         03  WRK-OUT-CODCUL-ATTR-T700     PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-CODCUL-T700-X.
PW9931             05  WRK-OUT-CODCUL-T700      PIC  9(03) VALUE ZEROS.
PRI499         03  WRK-OUT-DESCUL-T700          PIC  X(13) VALUE SPACES.
PW9931         03  WRK-OUT-CODCIC-ATTR-T700     PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-CODCIC-T700-X.
PW9931             05  WRK-OUT-CODCIC-T700      PIC  9(03) VALUE ZEROS.
PRI499         03  WRK-OUT-DESCIC-T700          PIC  X(12) VALUE SPACES.
PRI499         03  WRK-OUT-CODCLI-ATTR-T700     PIC  9(04) COMP
PRI499                                                     VALUE ZEROS.
PRI499         03  WRK-OUT-CODCLI-T700-X.
PRI499             05  WRK-OUT-CODCLI-T700      PIC  9(02) VALUE ZEROS.
PRI499         03  WRK-OUT-DESCLI-T700          PIC  X(12) VALUE SPACES.
0524           03  WRK-OUT-CODMAN-ATTR-T700     PIC  9(04) COMP
0524                                                       VALUE ZEROS.
0524           03  WRK-OUT-CODMAN-T700-X.
0524               05  WRK-OUT-CODMAN-T700      PIC  9(02) VALUE ZEROS.
0524           03  WRK-OUT-DESMAN-T700          PIC  X(12) VALUE SPACES.
               03  OUT-AREAF-ATTR-T700          PIC  9(04) COMP.
               03  OUT-AREAF-T700-X.
                   05  OUT-AREAF-T700           PIC  ZZZ.ZZZ.ZZ9,99.
               03  OUT-QTDEF-ATTR-T700          PIC  9(04) COMP.
               03  OUT-QTDEF-T700-X.
                   05  OUT-QTDEF-T700           PIC  ZZZ.ZZZ.ZZ9,99.
               03  OUT-LITERAL-T700             PIC  X(21).
               03  OUT-CAMPO-ATTR-T700          PIC  9(04) COMP.
               03  OUT-CAMPO-T700-X.
                   05  OUT-CAMPO-T700           PIC  ZZZ.ZZZ.ZZ9,99.
201503         03  WRK-OUT-LPRDTTL-T700         PIC  X(21) VALUE SPACES.
201503         03  WRK-OUT-CPRDTTL-ATTR-T700    PIC  9(04) COMP
201503                                                     VALUE ZEROS.
201503         03  WRK-OUT-CPRDTTL-T700-X       PIC  X(14) VALUE SPACES.
201503         03  FILLER REDEFINES WRK-OUT-CPRDTTL-T700-X.
201503             05  WRK-OUT-CPRDTTL-T700     PIC  ZZZ.ZZZ.ZZ9,99.
MI1608         03  OUT-LITER2-T700              PIC  X(15).
MI1008         03  OUT-GRUPO-ATTR-T700          PIC  9(04) COMP.
MI1008         03  OUT-GRUPO-T700               PIC  X(01).
MI1608         03  OUT-LITER3-T700              PIC  X(06).
MI1008         03  OUT-PORTE-ATTR-T700          PIC  9(04) COMP.
MI1008         03  OUT-PORTE-T700               PIC  X(07).
               03  OUT-TPEXP-ATTR-T700          PIC  9(04) COMP.
               03  OUT-TPEXP-T700               PIC  X(20).
               03  OUT-IMOVEL-ATTR-T700         PIC  9(04) COMP.
               03  OUT-IMOVEL-T700              PIC  X(25).
PW9931         03  OUT-CTIPCE-ATTR-T700         PIC  9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-CTIPCE-T700-X.
PW9931             05  WRK-OUT-CTIPCE-T700      PIC  9(01) VALUE ZEROS.
PW9931         03  WRK-OUT-DTIPCE-T700          PIC  X(15) VALUE SPACES.
PW9931         03  WRK-OUT-RECBR-ATTR-T700      PIC S9(04) COMP
PW9931                                                     VALUE ZEROS.
PW9931         03  WRK-OUT-RECBR-T700-X.
PW9931             05  WRK-OUT-RECBR-T700       PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
PW9931                                                     VALUE ZEROS.
LMC            03  WRK-OUT-TPOBEN-ATTR-T700     PIC S9(04) COMP
.                                                          VALUE ZEROS.
.              03  WRK-OUT-TPOBEN-T700-X.
.                  05  WRK-OUT-TPOBEN-T700      PIC  9(03) VALUE ZEROS.
.              03  WRK-OUT-DESCBE-T700          PIC  X(50) VALUE SPACES.
LMC
EG1216         03  WRK-OUT-NRODAP-ATTR-T700     PIC  X(02) VALUE SPACES.
EG1216         03  WRK-OUT-NRODAP-T700          PIC  X(25) VALUE SPACES.
BR0118         03  WRK-OUT-GPPRONA-ATTR-T700    PIC  X(02) VALUE SPACES.
BR0118         03  WRK-OUT-GPPRONA-T700         PIC  X(03) VALUE SPACES.
EG1216         03  WRK-OUT-VALDTD-ATTR-T700     PIC  X(02) VALUE SPACES.
EG1216         03  WRK-OUT-VALDTD-T700          PIC  X(02) VALUE SPACES.
EG1216         03  WRK-OUT-VALDTM-ATTR-T700     PIC  X(02) VALUE SPACES.
EG1216         03  WRK-OUT-VALDTM-T700          PIC  X(02) VALUE SPACES.
EG1216         03  WRK-OUT-VALDTA-ATTR-T700     PIC  X(02) VALUE SPACES.
EG1216         03  WRK-OUT-VALDTA-T700          PIC  X(04) VALUE SPACES.
201503*    02  FILLER REDEFINES OUT-DADOS-T700.
LMC   *        03  WRK-OUT-ESCONDIDOS           PIC X(277).
PRI499*        03  WRK-OUT-ESCONDIDOS           PIC X(280).
201503*        03  FILLER                       PIC X(748).
           02  OUT-MENSAG-T700                  PIC  X(79).

      /----------------------------------------------------------------*
      ***  AREA DA POOL0660 COM MASCARA DA TELA RUECT700 - SEGMENTO 00
      *----------------------------------------------------------------*
       01  660-RUECT700.
LMC        05  660-LL-AREA-T700        PIC  9(04)  COMP  VALUE 0168.
PRI499     05  660-LL-MENSAGEM-T700    PIC  9(04)  COMP  VALUE 1194.
           05  660-SENHAS-T700         PIC  9(04)  COMP  VALUE 0037.
           05  660-NOPCAO-T700         PIC  9(04)  COMP  VALUE 3004.
           05  660-DATA-T700           PIC  9(04)  COMP  VALUE 0010.
201503     05  660-OCULT01-T700        PIC  9(04)  COMP  VALUE 0016.
201503     05  660-OCULT02-T700        PIC  9(04)  COMP  VALUE 0017.
201503     05  660-OCULT03-T700        PIC  9(04)  COMP  VALUE 0024.
201503     05  660-OCULT04-T700        PIC  9(04)  COMP  VALUE 0024.
201503     05  660-OCULT05-T700        PIC  9(04)  COMP  VALUE 0013.
201503     05  660-OCULT06-T700        PIC  9(04)  COMP  VALUE 0010.
PRI499     05  660-OCULT07-T700        PIC  9(04)  COMP  VALUE 0022.
201503     05  660-OCULT08-T700        PIC  9(04)  COMP  VALUE 0006.
201503     05  660-OCULT09-T700        PIC  9(04)  COMP  VALUE 0079.
201503     05  660-OCULT10-T700        PIC  9(04)  COMP  VALUE 0014.
201503     05  660-OCULT11-T700        PIC  9(04)  COMP  VALUE 0014.
201503     05  660-OCULT12-T700        PIC  9(04)  COMP  VALUE 0014.
201503     05  660-OCULT13-T700        PIC  9(04)  COMP  VALUE 0014.
201503     05  660-OCULT14-T700        PIC  9(04)  COMP  VALUE 0003.
201503     05  660-OCULT15-T700        PIC  9(04)  COMP  VALUE 0004.
201503     05  660-OCULT16-T700        PIC  9(04)  COMP  VALUE 0002.
LMC        05  660-OCULT17-T700        PIC  9(04)  COMP  VALUE 0003.
201503     05  660-AGENC-T700          PIC  9(04)  COMP  VALUE 3007.
           05  660-CONTA-T700          PIC  9(04)  COMP  VALUE 3009.
           05  660-CGCNUM-T700         PIC  9(04)  COMP  VALUE 3011.
           05  660-CGCFIL-T700         PIC  9(04)  COMP  VALUE 3006.
           05  660-CTRL-T700           PIC  9(04)  COMP  VALUE 3004.
           05  660-NOME-T700           PIC  9(04)  COMP  VALUE 3042.
           05  660-SITUAC-T700         PIC  9(04)  COMP  VALUE 0011.
           05  660-MENSIT-T700         PIC  9(04)  COMP  VALUE 0027.
           05  660-NPROD1-T700         PIC  9(04)  COMP  VALUE 3042.
           05  660-HIFEN-T700          PIC  9(04)  COMP  VALUE 0001.
           05  660-NPROD2-T700         PIC  9(04)  COMP  VALUE 0022.
           05  660-ORIGCD-T700         PIC  9(04)  COMP  VALUE 3005.
           05  660-ORIGDS-T700         PIC  9(04)  COMP  VALUE 0020.
           05  660-ATIVCD-T700         PIC  9(04)  COMP  VALUE 3005.
           05  660-ATIVDS-T700         PIC  9(04)  COMP  VALUE 0016.
PW9931     05  WRK-660-CODMOD-T700     PIC  9(04)  COMP  VALUE 3005.
PW9931     05  WRK-660-DESMOD-T700     PIC  9(04)  COMP  VALUE 0020.
PW9931     05  WRK-660-CODVAR-T700     PIC  9(04)  COMP  VALUE 3005.
PW9931     05  WRK-660-DESVAR-T700     PIC  9(04)  COMP  VALUE 0022.
PW9931     05  WRK-660-CODCES-T700     PIC  9(04)  COMP  VALUE 3005.
PW9931     05  WRK-660-DESCES-T700     PIC  9(04)  COMP  VALUE 0020.
PW9931     05  WRK-660-ZONEAM-T700     PIC  9(04)  COMP  VALUE 3003.
PW9931     05  WRK-660-CONSOR-T700     PIC  9(04)  COMP  VALUE 3003.
PW9931     05  WRK-660-CODAGR-T700     PIC  9(04)  COMP  VALUE 3005.
PW9931     05  WRK-660-DESAGR-T700     PIC  9(04)  COMP  VALUE 0015.
PW9931     05  WRK-660-CODINT-T700     PIC  9(04)  COMP  VALUE 3005.
PW9931     05  WRK-660-DESINT-T700     PIC  9(04)  COMP  VALUE 0014.
PW9931     05  WRK-660-CODGRS-T700     PIC  9(04)  COMP  VALUE 3003.
PW9931     05  WRK-660-DESGRS-T700     PIC  9(04)  COMP  VALUE 0015.
PW9931     05  WRK-660-CODIRR-T700     PIC  9(04)  COMP  VALUE 3005.
PW9931     05  WRK-660-DESIRR-T700     PIC  9(04)  COMP  VALUE 0014.
PW9931     05  WRK-660-CODCUL-T700     PIC  9(04)  COMP  VALUE 3005.
PRI499     05  WRK-660-DESCUL-T700     PIC  9(04)  COMP  VALUE 0013.
PW9931     05  WRK-660-CODCIC-T700     PIC  9(04)  COMP  VALUE 3005.
PRI499     05  WRK-660-DESCIC-T700     PIC  9(04)  COMP  VALUE 0012.
PRI499     05  WRK-660-CODCLI-T700     PIC  9(04)  COMP  VALUE 3004.
PRI499     05  WRK-660-DESCLI-T700     PIC  9(04)  COMP  VALUE 0012.
0524       05  WRK-660-CODMAN-T700     PIC  9(04)  COMP  VALUE 3004.
0524       05  WRK-660-DESMAN-T700     PIC  9(04)  COMP  VALUE 0012.
           05  660-AREAF-T700          PIC  9(04)  COMP  VALUE 3016.
           05  660-QTDEF-T700          PIC  9(04)  COMP  VALUE 3016.
           05  660-LITERAL-T700        PIC  9(04)  COMP  VALUE 0021.
           05  660-CAMPO-T700          PIC  9(04)  COMP  VALUE 2016.
201503     05  WRK-660-LITERAL-T700    PIC  9(04)  COMP  VALUE 0021.
201503     05  WRK-660-CAMPO-T700      PIC  9(04)  COMP  VALUE 2016.
MI1608     05  660-LITER2-T700         PIC  9(04)  COMP  VALUE 0015.
MI1008     05  660-GRUPO-T700          PIC  9(04)  COMP  VALUE 2003.
MI1608     05  660-LITER3-T700         PIC  9(04)  COMP  VALUE 0006.
MI1008     05  660-PORTE-T700          PIC  9(04)  COMP  VALUE 2009.
           05  660-TPEXP-T700          PIC  9(04)  COMP  VALUE 3022.
           05  660-IMOVEL-T700         PIC  9(04)  COMP  VALUE 3027.
PW9931     05  WRK-660-CTIPCE-T700     PIC  9(04)  COMP  VALUE 3003.
PW9931     05  WRK-660-DTIPCE-T700     PIC  9(04)  COMP  VALUE 0015.
PW9931     05  WRK-660-RECBR-T700      PIC  9(04)  COMP  VALUE 3020.
LMC        05  WRK-660-TPOBEN-T700     PIC  9(04)  COMP  VALUE 3005.
LMC        05  WRK-660-DESCBE-T700     PIC  9(04)  COMP  VALUE 0050.
EG1216     05  WRK-660-NRODAP-T700     PIC  9(04)  COMP  VALUE 3027.
BR0118     05  WRK-660-GPPRONA-T700    PIC  9(04)  COMP  VALUE 3005.
EG1216     05  WRK-660-VALDTD-T700     PIC  9(04)  COMP  VALUE 3004.
EG1216     05  WRK-660-VALDTM-T700     PIC  9(04)  COMP  VALUE 3004.
EG1216     05  WRK-660-VALDTA-T700     PIC  9(04)  COMP  VALUE 3006.
           05  660-MENSAG-T700         PIC  9(04)  COMP  VALUE 0079.

      *----------------------------------------------------------------*
      *       AREA DE COMUNICACAO ENTRE PROGRAMAS DA ROTINA RUEC       *
      *----------------------------------------------------------------*

       01  COMU-AREA.
           05  COMU-LL                 PIC S9(04) COMP VALUE +147.
           05  COMU-ZZ                 PIC  9(04) COMP VALUE ZEROS.
           05  COMU-TRANCODE           PIC  X(15).
           05  COMU-SENHA              PIC  X(37).
           05  COMU-NOPCAO             PIC  X(02).
           05  COMU-COD-TERM           PIC  X(08).
           05  COMU-STA-RET            PIC  X(02).
           05  COMU-MENSAGEM           PIC  X(79).

      *----------------------------------------------------------------*
      *      AREA PARA CHAMADA AOS MODULOS DA ROTINA RUEC              *
      *----------------------------------------------------------------*
       COPY 'I#RUEC07'.

      *----------------------------------------------------------------*
      * BOOK DA AREA DE COMUNICACAO (NAVEGACAO) ENTRE PROGRAMAS DO RUEC*
      *----------------------------------------------------------------*
       COPY 'I#RUEC00'.

      *----------------------------------------------------------------*
POL097* BOOK DA AREA DE COMUNICACAO COM MODULO RUEC7718                *
      *----------------------------------------------------------------*
       COPY 'I#RUEC20'.

      *----------------------------------------------------------------*
POL097* BOOK DA AREA DE COMUNICACAO COM MODULO RUEC7720                *
      *----------------------------------------------------------------*
       COPY 'I#RUEC21'.

      *----------------------------------------------------------------*
POL097* BOOK DA AREA DE COMUNICACAO COM MODULO RUEC7722                *
      *----------------------------------------------------------------*
       COPY 'I#RUEC22'.
      *----------------------------------------------------------------*
POL097*      LAY-OUT DA TABELA RUECB048 DE APOIO                       *
      *----------------------------------------------------------------*
       COPY 'I#RUEC31'.

      *----------------------------------------------------------------*
PW9931*      AREA PARA O MODULO RUEC8800                               *
      *----------------------------------------------------------------*
       COPY 'I#RUEC58'.

BR1218*----------------------------------------------------------------*
BR1218 01  FILLER                      PIC  X(032)         VALUE
BR1218     '*** AREA DO MODULO CMRU308L  ***'.
BR1218*----------------------------------------------------------------*
BR1218
BR1218 01  WRK-AREA-CMRU308L.
BR1218     COPY CMRUW08I.

PRI499*----------------------------------------------------------------*
PRI499 01  FILLER                      PIC  X(032)         VALUE
PRI499     '*** AREA DO MODULO CRUR2G0C  ***'.
PRI499*----------------------------------------------------------------*
PRI499
PRI499 01  WRK-AREA-CRUR2G0C.
PRI499     COPY CRURW00W.
PRI499     COPY CRURWG0I.

PRI499*----------------------------------------------------------------*
PRI499 01  FILLER                      PIC  X(032)         VALUE
PRI499     '*** AREA DO MODULO CRUR2G2C  ***'.
PRI499*----------------------------------------------------------------*
PRI499
0524   01  WRK-AREA-CRUR2G2C.
0524       COPY CRURW00W.
           COPY CRURWG2Y.
STEFA2*----------------------------------------------------------------*
.      01  FILLER                      PIC  X(032)         VALUE
.          '*** AREA DO MODULO CRUR236G  ***'.
.     *----------------------------------------------------------------*
.      01  WRK-AREA-CRUR236G.
.          COPY CRURW236.
STEFA2
      *---------------------------------------------------------------*
      ***                       TABELAS DB2
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE  SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE  RUECB041
           END-EXEC.

           EXEC SQL
               INCLUDE  RUECB042
           END-EXEC.

POL126**   EXEC SQL
POL126**       INCLUDE  RUECB045
POL126**   END-EXEC.

           EXEC SQL
               INCLUDE  RUECB007
           END-EXEC.

           EXEC SQL
               INCLUDE  RUECV008
           END-EXEC.

           EXEC SQL
               INCLUDE  RUECV019
           END-EXEC.

           EXEC SQL
               INCLUDE  RURCV022
           END-EXEC.

           EXEC SQL
               INCLUDE  RURCV000
           END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RURCB003
PW9931     END-EXEC.

           EXEC SQL
               INCLUDE  RURCV004
           END-EXEC.

           EXEC SQL
               INCLUDE  RURCV014
           END-EXEC.

           EXEC SQL
               INCLUDE  RURCV040
           END-EXEC.

           EXEC SQL
               INCLUDE  RURCB051
           END-EXEC.

           EXEC SQL
               INCLUDE  RUECV001
           END-EXEC.

           EXEC SQL
               INCLUDE  MESUV050
           END-EXEC.

PW1577     EXEC SQL
PW1577         INCLUDE  RUECB048
PW1577     END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RUECB061
PW9931     END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RURCB082
PW9931     END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RURCB083
PW9931     END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RURCB085
PW9931     END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RURCB087
PW9931     END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RURCB088
PW9931     END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RURCB089
PW9931     END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RURCB090
PW9931     END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RURCB091
PW9931     END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RUECB09H
PW9931     END-EXEC.

PW9931     EXEC SQL
PW9931         INCLUDE  RURCB006
PW9931     END-EXEC.

EG0617*---------------------------------------------------------------*
EG0617 01  FILLER                      PIC X(32)           VALUE
EG0617     '*** INCLUDE RUECB056         ***'.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     EXEC SQL
EG0617         INCLUDE  RUECB056
EG0617     END-EXEC.
EG0617
EG0817*---------------------------------------------------------------*
EG0817 01  FILLER                      PIC X(32)           VALUE
EG0817     '*** INCLUDE RUFIB002         ***'.
EG0817*---------------------------------------------------------------*
EG0817
EG0817     EXEC SQL
EG0817         INCLUDE  RUFIB002
EG0817     END-EXEC.
EG0817
PW1577*===============================================================*
PW1577* DEFINICAO DO CURSOR PARA OBTER AREA TEMPORARIA QUALIFICACAO   *
PW1577*===============================================================*
PW1577
PW1577     EXEC SQL
PW1577         DECLARE CSR01-RUECB048 CURSOR WITH HOLD FOR
PW1577          SELECT CTERM,
PW1577                 CIDTFD_MDULO,
PW1577                 CSEQ_CTRL_MDULO,
PW1577                 WRESUL_CONS_MDULO
PW1577          FROM   DB2PRD.TAPOIO_UTILZ_MDULO
PW1577          WHERE  CTERM        = :RUECB048.CTERM
PW1577            AND ((CIDTFD_MDULO BETWEEN 15 AND 20 )
EG1216            OR   CIDTFD_MDULO = 35 )
PW1577     END-EXEC.

      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*

       01  IO-PCB.
           03  IO-LTERM                PIC X(08).
           03  FILLER                  PIC X(02).
           03  IO-STATUS               PIC X(02).
           03  FILLER                  PIC X(12).
           03  IO-MODNAME              PIC X(08).

       01  ALT-PCB.
           03  ALT-LTERM               PIC X(08).
           03  FILLER                  PIC X(02).
           03  ALT-STATUS              PIC X(02).
           03  FILLER                  PIC X(12).
           03  ALT-MODNAME             PIC X(08).

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

           ENTRY       'DLITCBL'       USING   IO-PCB ALT-PCB.

HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU                           *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.
HEXA  *
           MOVE        WRK-GU          TO      WRK-FUNCAO.

      *---------------------------------------------------------------*
       00000-INICIO-RUEC7000 SECTION.
      *---------------------------------------------------------------*

           CALL    'POOL5000'      USING       WRK-FUNCAO
                                               IO-PCB
                                               ALT-PCB
                                               WRK-MENSAGEM
                                               WRK-TELA
                                               WRK-NOVA-OPCAO
                                               WRK-COD-USER
                                               WRK-OPCAO-ATUAL
                                               WRK-COD-DEPTO6.

           IF  RETURN-CODE EQUAL 04 OR 12
               GOBACK.

           MOVE        SPACES              TO   OUT-DADOS-T700
                                                WRK-INCONSIS
                                                WRK-INCONSIS-BYTE
                                                WRK-INCONS-AREAF
EG1216                                          WRK-INCONS-DAP
                                                WRK-INCONS-QTDEF
                                                WRK-INCONS-CAMPO
MI1708                                          WRK-INCONS-GRUPO
MI1708                                          WRK-INCONS-PORTE
                                                WRK-CONTINUACAO
                                                WRK-PROCESSA-TELA
                                                WRK-CRSUMO1
                                                WRK-CRSUMO2
                                                WRK-CRSUMO3
                                                WRK-CRSUMO4
                                                WRK-CRSUMO5
                                                WRK-CRSUMO6

           PERFORM 10000-PROCESSAR-MSG.

           GO TO 00000-INICIO-RUEC7000.

      *---------------------------------------------------------------*
       00000-99-FIM. EXIT.
      *---------------------------------------------------------------*

201504*---------------------------------------------------------------*
201504 1000-VERIFICA-AGE-MIGRADA      SECTION.
201504*---------------------------------------------------------------*
201504
201504     INITIALIZE CMRUWA0A-REGISTRO-ENTRADA.
201504
201504     MOVE 1                      TO CMRUWA0A-E-OPC-PESQ
201504     MOVE WRK-CPSSOA-JURID       TO CMRUWA0A-E-CPSSOA-JURID
201504     MOVE INP-AGENC-T700         TO CMRUWA0A-E-NSEQ-UND-ORGNZ
201504     MOVE 'CMRU2A0T'             TO WRK-MODULO.
201504
201504     CALL WRK-MODULO             USING WRK-AREA-CMRU2A0T.
201504
201514     IF  CMRUWA0A-S-COD-RETORNO  EQUAL 12
201514         MOVE WRK-CMRUWA0A-S-ERRO-AREA  TO  ERRO-AREA
201514         MOVE WRK-CMRUWA0A-S-SQLCA   TO  SQLCA
201514         PERFORM 99999-ROTINA-ERRO
201514     END-IF.

201504     IF  CMRUWA0A-S-COD-RETORNO  NOT EQUAL ZEROS
201504         MOVE CMRUWA0A-S-COD-RETORNO
201504                                 TO WRK-M57-RC
201504         MOVE WRK-M57            TO OUT-MENSAG-T700
201504         MOVE TAB-ATRIB(1)       TO OUT-AGENC-ATTR-T700
201504         MOVE '*'                TO WRK-INCONSIS
201504     END-IF.
201504
201504     IF  CMRUWA0A-S-CSIT-REG(1)  EQUAL 'B'
201504         MOVE    WRK-M55         TO  OUT-MENSAG-T700
201504         MOVE TAB-ATRIB(1)       TO OUT-AGENC-ATTR-T700
201504         MOVE '*'                TO WRK-INCONSIS
201504     END-IF.
201504
201504*---------------------------------------------------------------*
201504 1000-99-FIM. EXIT.
201504*---------------------------------------------------------------*
201504
201504*---------------------------------------------------------------*
201504*1100-VERIFICA-FICHA-CRUR       SECTION.
201504*---------------------------------------------------------------*
201504
201504****XINITIALIZE CMRUWAKC-REGISTRO.
201504
201504****XMOVE WRK-CPSSOA-JURID       TO CMRUWA0A-E-CPSSOA-JURID
201504****XMOVE CCLUB-RUEC             TO CMRUWAKC-E-CCLUB
201504****XMOVE 'CMRU2AKT'             TO WRK-MODULO.
201504
201504****XCALL WRK-MODULO             USING WRK-AREA-CMRU2AKT.
201504
201504****XIF  CMRUWAKC-COD-RETORNO  NOT EQUAL ZEROS
201504********XMOVE CMRUWAKC-COD-MENSAGEM
201504*********************************TO WRK-COD-MSG-AUX
201504********XMOVE ' - ERRO NA VERIFICACAO DE FICHA NO CRUR  '
201504*********************************TO WRK-MSG-ERRO-AUX
201504********XMOVE WRK-MSG-ERRO-TELA  TO OUT-MENSAG-T700
201504********XMOVE TAB-ATRIB(1)       TO OUT-AGENC-ATTR-T700
201504********XMOVE '*'                TO WRK-INCONSIS
201504****XEND-IF.
201504
201504****XIF  CMRUWAKC-S-CINDCD-FICHA EQUAL 1
201504*       XMOVE    WRK-M56         TO  OUT-MENSAG-T700
201504*       XMOVE TAB-ATRIB(1)       TO OUT-AGENC-ATTR-T700
201504*       XMOVE '*'                TO WRK-INCONSIS
201504****XEND-IF.
201504*
201504*---------------------------------------------------------------*
201504*1100-99-FIM. EXIT.
201504*---------------------------------------------------------------*

101020*---------------------------------------------------------------*
101020 1200-00-OBTEM-FORMA-PAGTO       SECTION.
101020*---------------------------------------------------------------*
101020
101020     MOVE  1,2                   TO  OUT-POSPROC-T700.
101020     MOVE '1'                    TO  OUT-FASE-T700.
101020
101020     MOVE 'RUEC7000'             TO  PROGRAMA-ANT.
101020
101020     PERFORM 11400-MOVE-TELA-OUT-IN.
101020
101020     MOVE '1200'                 TO  ERR-LOCAL.
101020
101020     PERFORM 12030-UPDATE-RUECB042.
101020
101020     MOVE  IO-LTERM              TO  COMU-COD-TERM.
101020     MOVE 'RUEC7000'             TO  COMU-TRANCODE.
101020     MOVE  +147                  TO  COMU-LL.
101020     MOVE  COMU-AREA             TO  WRK-MENSAGEM.
101020     MOVE 'RUEC7600'             TO  WRK-TELA.
101020     MOVE  WRK-CHNG              TO  WRK-FUNCAO.
101020
101020*---------------------------------------------------------------*
101020 1200-99-FIM. EXIT.
101020*---------------------------------------------------------------*

      *---------------------------------------------------------------*
       10000-PROCESSAR-MSG SECTION.
      *---------------------------------------------------------------*

ACT181     PERFORM 14000-VERIFICAR-FIM-ANO.

ACT181     IF  (WRK-MENS01    NOT  EQUAL   SPACES)
ACT181         MOVE  WRK-MENS01    TO  OUT-MENSAG-T700
ACT181         MOVE  'RUECT700'    TO  WRK-TELA
ACT181         PERFORM 13000-POOL0660-RUECT700
ACT181         GO                  TO  10000-99-FIM.

           IF WRK-TRANSACAO NOT EQUAL 'RUEC7000'
              PERFORM 11000-RECEBER-CONTROLE.

           IF WRK-PROCESSA-TELA EQUAL SPACES
             PERFORM 12000-PROCESSAR-RUECT700.

           IF  WRK-TELA  EQUAL 'RUECT700'
201503         PERFORM 2200-CALCULAR-PROD-TOTAL
               PERFORM 13000-POOL0660-RUECT700.

      *---------------------------------------------------------------*
       10000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *  ESSA ROTINA INSERE A TELA QUANDO O PGM RECEBE O CONTROLE     *
      *****************************************************************

      *---------------------------------------------------------------*
       11000-RECEBER-CONTROLE SECTION.
      *---------------------------------------------------------------*

           MOVE  WRK-MENSAGEM              TO   COMU-AREA.

           MOVE  'RUECT700'                TO   WRK-TELA

           MOVE  '1'                       TO   INP-FASE-T700.

           MOVE  'N'                       TO   WRK-PROCESSA-TELA.

           IF  WRK-TRANSACAO   NOT EQUAL           'RUEC7510'  AND
                                   'RUEC7520' AND  'RUEC7706'  AND
                                   'RUEC7708' AND  'RUEC7704'  AND
                                   'RUEC7550' AND  'RUEC7560'  AND
                                   'RUEC7530' AND  'RUEC7010'  AND
                                   'RUEC8700' AND
PW9931                             'RUEC6970' AND  'RUEC6980'  AND
PW9931                             'RUEC6990' AND  'RUEC6140'  AND
PW9931                             'RUEC6150' AND  'RUEC6160'  AND
PW9931                             'RUEC6170' AND  'RUEC6180'  AND
PW9931                             'RUEC6190' AND  'RUEC6230'  AND
WI1121                             'RUEC7750' AND  'RUEC7600' AND
WI1121*                            'RUEC9300'
PRI499                             'RUEC9300' AND  'RUEC6210' AND
0524                               'RUEC6260' AND
202502                             'RUEC7961'
               PERFORM 12015-DELETE-RUECB042
               PERFORM 1100-10-DELETA-025
           END-IF.

PW1577     PERFORM 1100-01-DELETA-RUECB048.

           PERFORM 11200-SELECT-RUECB042

           IF  SQLCODE     EQUAL   +100
               IF  WRK-TRANSACAO   NOT EQUAL       'RUEC7510'  AND
                                   'RUEC7520' AND  'RUEC7706'  AND
                                   'RUEC7708' AND  'RUEC7704'  AND
                                   'RUEC7550' AND  'RUEC7560'  AND
                                   'RUEC7530' AND  'RUEC7010'  AND
                                   'RUEC8700' AND
PW9931                             'RUEC6970' AND  'RUEC6980'  AND
PW9931                             'RUEC6990' AND  'RUEC6140'  AND
PW9931                             'RUEC6150' AND  'RUEC6160'  AND
PW9931                             'RUEC6170' AND  'RUEC6180'  AND
PW9931                             'RUEC6190' AND  'RUEC6230'  AND
WI1121                             'RUEC7750' AND  'RUEC7600' AND
WI1121*                            'RUEC9300'
PRI499                             'RUEC9300' AND  'RUEC6210' AND
0524                               'RUEC6260' AND
202502                             'RUEC7961'
                   MOVE    WRK-M13         TO  OUT-MENSAG-T700
                   GO  TO  11000-99-FIM
               ELSE
                   MOVE    IO-LTERM        TO  M10-CAMPO
                   MOVE    WRK-M10         TO  OUT-MENSAG-T700
                   GO  TO  11000-99-FIM.

POL097     IF PROGRAMA-ANT         NOT EQUAL 'RUEC8700'
POL097        INITIALIZE  R31-AREA-TOTAL
POL097        MOVE    1                   TO  R31-CSEQ-REG
POL097        PERFORM 11201-SELECT-RUECB048
POL097        MOVE    R31-SALVA-RUEC7000  TO  INP-DADOS-T700(1:300)
POL097        INITIALIZE  R31-AREA-TOTAL
POL097        MOVE    2                   TO  R31-CSEQ-REG
POL097        PERFORM 11201-SELECT-RUECB048
POL097        MOVE    R31-SALVA-RUEC7000  TO  INP-DADOS-T700(301:300)
PW9931        INITIALIZE  R31-AREA-TOTAL
PW9931        MOVE    3                   TO  R31-CSEQ-REG
PW9931        PERFORM 11201-SELECT-RUECB048
LMC           MOVE    R31-SALVA-RUEC7000  TO  INP-DADOS-T700(601:322)
TIRAR
STEFA2        IF WRK-TRANSACAO         EQUAL 'RUEC7961'
STEFA2           PERFORM 1770-00-TRATA-RUEC7961
RE1017        END-IF

RE1017        IF WRK-TRANSACAO       EQUAL 'RUEC7750' OR 'RUEC7530'
 .               PERFORM 2530-CONSULTAR-DAP-RUECB048
 .               MOVE WRK-NRODAP         TO WRK-INP-NRODAP-T700
 .               MOVE WRK-VALDTD         TO WRK-INP-VALDTD-T700
 .               MOVE WRK-VALDTM         TO WRK-INP-VALDTM-T700
 .               MOVE WRK-VALDTA         TO WRK-INP-VALDTA-T700
BR0118           MOVE WRK-GPPRONA        TO WRK-INP-GPPRONA-T700
RE1017        END-IF
PW9931     END-IF.

           PERFORM 11300-MOVE-TELA-IN-OUT.

201503     PERFORM 2300-AVALIAR-RECEITA-BRUTA.
201503
           IF  WRK-TRANSACAO       EQUAL       'RUEC7550'
               IF  COMU-STA-RET        EQUAL   'PF'
                   MOVE OUT-ANPROD1-T700   TO  OUT-NPROD1-T700.

           IF  WRK-TRANSACAO       EQUAL       'RUEC7560'
               IF  COMU-STA-RET        EQUAL   'PF'
                   IF OUT-AORIGCD-T700 NOT NUMERIC
                      MOVE OUT-AORIGCD-T700-X  TO  OUT-ORIGCD-T700-X
                      MOVE ZEROS               TO  COD-ORIG-REC
                      PERFORM 12030-UPDATE-RUECB042
                   ELSE
                      MOVE OUT-AORIGCD-T700    TO  OUT-ORIGCD-T700
                                                   COD-ORIG-REC
                      PERFORM 12030-UPDATE-RUECB042.

           IF  COMU-STA-RET        EQUAL   'ER'
               MOVE    COMU-MENSAGEM       TO  OUT-MENSAG-T700
               GO  TO  11000-99-FIM.


           IF  PROGRAMA-ANT        EQUAL   'RUEC7000'
               GO  TO  11000-99-FIM.

           MOVE  'S'                       TO   WRK-CONTINUACAO.
           MOVE  SPACES                    TO   WRK-PROCESSA-TELA.
           MOVE  '.'                       TO   INP-PFK-T700.

      *---------------------------------------------------------------*
       11000-99-FIM. EXIT.
      *---------------------------------------------------------------*

PW1577*---------------------------------------------------------------*
PW1577 1100-01-DELETA-RUECB048  SECTION.
PW1577*---------------------------------------------------------------*
PW1577
PW1577     MOVE IO-LTERM                  TO CTERM OF RUECB048.
PW1577
PW1577     PERFORM 1100-02-ABRIR-CSR01-RUECB048.
PW1577
PW1577     PERFORM 1100-03-FETCH-CSR01-RUECB048.
PW1577
PW1577     PERFORM 1100-04-TRATAR-CSR01-RUECB048
PW1577                  UNTIL SQLCODE EQUAL +100.
PW1577
PW1577     PERFORM 1100-05-CLOSE-CSR01-RUECB048.
PW1577
PW1577*---------------------------------------------------------------*
PW1577 1100-01-99-FIM.  EXIT.
PW1577*---------------------------------------------------------------*

PW1577*---------------------------------------------------------------*
PW1577 1100-02-ABRIR-CSR01-RUECB048 SECTION.
PW1577*---------------------------------------------------------------*
PW1577
PW1577     EXEC SQL
PW1577          OPEN CSR01-RUECB048
PW1577     END-EXEC.
PW1577
PW1577     IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
PW1577        (SQLWARN0              EQUAL  'W')
PW1577         MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
PW1577         MOVE 'TAPOIO_UTILZ_MDULO'     TO   ERR-DBD-TAB
PW1577         MOVE 'OPEN    '               TO   ERR-FUN-COMANDO
PW1577         MOVE SQLCODE                  TO   ERR-SQL-CODE
PW1577         MOVE '0041'                   TO   ERR-LOCAL
PW1577         MOVE SPACES                   TO   ERR-SEGM
PW1577         PERFORM   99999-ROTINA-ERRO
PW1577     END-IF.
PW1577
PW1577*---------------------------------------------------------------*
PW1577 1100-02-99-FIM.  EXIT.
PW1577*---------------------------------------------------------------*

PW1577*---------------------------------------------------------------*
PW1577 1100-03-FETCH-CSR01-RUECB048 SECTION.
PW1577*---------------------------------------------------------------*
PW1577
PW1577     EXEC SQL
PW1577          FETCH CSR01-RUECB048 INTO
PW1577               :RUECB048.CTERM,
PW1577               :RUECB048.CIDTFD-MDULO,
PW1577               :RUECB048.CSEQ-CTRL-MDULO,
PW1577               :RUECB048.WRESUL-CONS-MDULO
PW1577                     :WRK-WRESUL-CONS-MDULO-NULL
PW1577     END-EXEC.
PW1577
PW1577     IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
PW1577        (SQLWARN0              EQUAL  'W')
PW1577         MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
PW1577         MOVE 'TAPOIO_UTILZ_MDULO'     TO   ERR-DBD-TAB
PW1577         MOVE 'FECTH   '               TO   ERR-FUN-COMANDO
PW1577         MOVE SQLCODE                  TO   ERR-SQL-CODE
PW1577         MOVE '0042'                   TO   ERR-LOCAL
PW1577         MOVE SPACES                   TO   ERR-SEGM
PW1577         PERFORM   99999-ROTINA-ERRO
PW1577     END-IF.
PW1577
PW1577     IF (SQLCODE               EQUAL   +100)
PW1577        GO TO 1100-03-99-FIM
PW1577     END-IF.
PW1577
PW1577     IF WRK-WRESUL-CONS-MDULO-NULL  NOT EQUAL ZEROS
PW1577        MOVE SPACES                 TO  WRESUL-CONS-MDULO
PW1577                                    OF  RUECB048
PW1577     END-IF.
PW1577
PW1577*---------------------------------------------------------------*
PW1577 1100-03-99-FIM.  EXIT.
PW1577*---------------------------------------------------------------*

PW1577*---------------------------------------------------------------*
PW1577 1100-04-TRATAR-CSR01-RUECB048 SECTION.
PW1577*---------------------------------------------------------------*
PW1577
EG1216     IF ((WRK-TRANSACAO              EQUAL 'RUEC7750'
                OR 'RUEC7530')
EG1216     AND CIDTFD-MDULO OF RUECB048    EQUAL 35)
EG1216         PERFORM 1100-03-FETCH-CSR01-RUECB048
EG1216         GO TO 1100-04-99-FIM
EG1216     END-IF.
EG1216
PW1577     EXEC SQL
PW1577          DELETE FROM DB2PRD.TAPOIO_UTILZ_MDULO
PW1577             WHERE CURRENT OF CSR01-RUECB048
PW1577     END-EXEC.
PW1577
PW1577     IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
PW1577        (SQLWARN0              EQUAL  'W')
PW1577         MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
PW1577         MOVE 'TAPOIO_UTILZ_MDULO'     TO   ERR-DBD-TAB
PW1577         MOVE 'DELETE  '               TO   ERR-FUN-COMANDO
PW1577         MOVE SQLCODE                  TO   ERR-SQL-CODE
PW1577         MOVE '0043'                   TO   ERR-LOCAL
PW1577         MOVE SPACES                   TO   ERR-SEGM
PW1577         PERFORM   99999-ROTINA-ERRO
PW1577     END-IF.
PW1577
PW1577     PERFORM 1100-03-FETCH-CSR01-RUECB048.
PW1577
PW1577*---------------------------------------------------------------*
PW1577 1100-04-99-FIM.  EXIT.
PW1577*---------------------------------------------------------------*

PW1577*---------------------------------------------------------------*
PW1577 1100-05-CLOSE-CSR01-RUECB048 SECTION.
PW1577*---------------------------------------------------------------*
PW1577
PW1577     EXEC SQL
PW1577          CLOSE CSR01-RUECB048
PW1577     END-EXEC.
PW1577
PW1577     IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
PW1577        (SQLWARN0              EQUAL  'W')
PW1577         MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
PW1577         MOVE 'TAPOIO_UTILZ_MDULO'     TO   ERR-DBD-TAB
PW1577         MOVE 'CLOSE   '               TO   ERR-FUN-COMANDO
PW1577         MOVE SQLCODE                  TO   ERR-SQL-CODE
PW1577         MOVE '0044'                   TO   ERR-LOCAL
PW1577         MOVE SPACES                   TO   ERR-SEGM
PW1577         PERFORM   99999-ROTINA-ERRO
PW1577     END-IF.
PW1577
PW1577*---------------------------------------------------------------*
PW1577 1100-05-99-FIM.  EXIT.
PW1577*---------------------------------------------------------------*

POL097*---------------------------------------------------------------*
POL097 1100-10-DELETA-025 SECTION.
POL097*---------------------------------------------------------------*
POL097
POL097     MOVE IO-LTERM               TO CTERM OF RUECB048.
POL097     MOVE 025                    TO CIDTFD-MDULO OF RUECB048.
POL097     MOVE 1                      TO CSEQ-CTRL-MDULO OF RUECB048.
POL097
POL097     EXEC SQL
POL097          DELETE FROM DB2PRD.TAPOIO_UTILZ_MDULO
POL097           WHERE CTERM           = :RUECB048.CTERM AND
POL097                 CIDTFD_MDULO    = :RUECB048.CIDTFD-MDULO AND
POL097                 CSEQ_CTRL_MDULO = :RUECB048.CSEQ-CTRL-MDULO
POL097     END-EXEC.
POL097
POL097     IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
POL097        (SQLWARN0              EQUAL  'W')
POL097         MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL097         MOVE 'TAPOIO_UTILZ_MDULO'     TO   ERR-DBD-TAB
POL097         MOVE 'DELETE  '               TO   ERR-FUN-COMANDO
POL097         MOVE SQLCODE                  TO   ERR-SQL-CODE
POL097         MOVE '0043A'                  TO   ERR-LOCAL
POL097         MOVE SPACES                   TO   ERR-SEGM
POL097         PERFORM   99999-ROTINA-ERRO
POL097     END-IF.
POL097
POL097     MOVE IO-LTERM               TO CTERM OF RUECB048.
POL097     MOVE 025                    TO CIDTFD-MDULO OF RUECB048.
POL097     MOVE 2                      TO CSEQ-CTRL-MDULO OF RUECB048.
POL097
POL097     EXEC SQL
POL097          DELETE FROM DB2PRD.TAPOIO_UTILZ_MDULO
POL097           WHERE CTERM           = :RUECB048.CTERM AND
POL097                 CIDTFD_MDULO    = :RUECB048.CIDTFD-MDULO AND
POL097                 CSEQ_CTRL_MDULO = :RUECB048.CSEQ-CTRL-MDULO
POL097     END-EXEC.
POL097
POL097     IF (SQLCODE           NOT EQUAL   +0 AND +100)    OR
POL097        (SQLWARN0              EQUAL  'W')
POL097         MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL097         MOVE 'TAPOIO_UTILZ_MDULO'     TO   ERR-DBD-TAB
POL097         MOVE 'DELETE  '               TO   ERR-FUN-COMANDO
POL097         MOVE SQLCODE                  TO   ERR-SQL-CODE
POL097         MOVE '0043B'                  TO   ERR-LOCAL
POL097         MOVE SPACES                   TO   ERR-SEGM
POL097         PERFORM   99999-ROTINA-ERRO
POL097     END-IF.
POL097
POL097*---------------------------------------------------------------*
POL097 1100-10-99-FIM.  EXIT.
POL097*---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11200-SELECT-RUECB042 SECTION.
      *---------------------------------------------------------------*

EG1216     PERFORM 2530-CONSULTAR-DAP-RUECB048.
EG1216
           MOVE    IO-LTERM        TO  CTERM       OF RUECB042.

           EXEC SQL
             SELECT
                   CTERM,
                   WNAVG_CREDT_RURAL
             INTO
                   :RUECB042.CTERM,
                   :RUECB042.WNAVG-CREDT-RURAL
             FROM   DB2PRD.NAVEGACAO_RURAL
             WHERE
                   CTERM              = :RUECB042.CTERM
           END-EXEC.

           IF   (( SQLCODE   NOT  =    ZEROS )  AND
                 ( SQLCODE   NOT  =     +100 ))  OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'NAVEGACAO_RURAL   '     TO   ERR-DBD-TAB
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0001                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM 99999-ROTINA-ERRO.

           IF    ( SQLCODE        =    ZEROS )
                   MOVE  WNAVG-CREDT-RURAL-TEXT  TO   NAVEG-RURAL.

      *---------------------------------------------------------------*
       11200-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
POL097 11201-SELECT-RUECB048 SECTION.
      *---------------------------------------------------------------*

POL097     MOVE    IO-LTERM        TO CTERM        OF RUECB048.
POL097     MOVE    025             TO CIDTFD-MDULO OF RUECB048.
POL097     MOVE    R31-CSEQ-REG    TO CSEQ-CTRL-MDULO OF RUECB048.

POL097     EXEC SQL
POL097       SELECT WRESUL_CONS_MDULO
                   ,CTERM
POL097       INTO   :RUECB048.WRESUL-CONS-MDULO
POL097                :WRK-WRESUL-CONS-MDULO-NULL
                   ,:RUECB048.CTERM
POL097       FROM   DB2PRD.TAPOIO_UTILZ_MDULO
POL097       WHERE  CTERM              = :RUECB048.CTERM AND
POL097              CIDTFD_MDULO       = :RUECB048.CIDTFD-MDULO AND
POL097              CSEQ_CTRL_MDULO    = :RUECB048.CSEQ-CTRL-MDULO
POL097     END-EXEC.

POL097     IF    ( SQLCODE   NOT EQUAL ZEROS )   OR
POL097           ( SQLWARN0      EQUAL 'W'   )
POL097             MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL097             MOVE 'TAPOIO_UTILZ_MDULO'    TO   ERR-DBD-TAB
POL097             MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
POL097             MOVE SQLCODE                  TO   ERR-SQL-CODE
POL097             MOVE '003A'                   TO   ERR-LOCAL
POL097             MOVE SPACES                   TO   ERR-SEGM
POL097             PERFORM   99999-ROTINA-ERRO
POL097     END-IF.

POL097     MOVE WRESUL-CONS-MDULO-TEXT  TO R31-TEXTO.

      *---------------------------------------------------------------*
POL097 11201-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11300-MOVE-TELA-IN-OUT SECTION.
      *---------------------------------------------------------------*

           MOVE    INP-FASE-T700       TO  OUT-FASE-T700
           MOVE    INP-AAGENC-T700-X   TO  OUT-AAGENC-T700-X
           MOVE    INP-ACONTA-T700-X   TO  OUT-ACONTA-T700-X
           MOVE    INP-ACGCNUM-T700-X  TO  OUT-ACGCNUM-T700-X
           MOVE    INP-ACGCFIL-T700-X  TO  OUT-ACGCFIL-T700-X
           MOVE    INP-ACTRL-T700-X    TO  OUT-ACTRL-T700-X
           MOVE    INP-AGENC-T700-X    TO  OUT-AGENC-T700-X
           MOVE    INP-CONTA-T700-X    TO  OUT-CONTA-T700-X
           MOVE    INP-CGCNUM-T700-X   TO  OUT-CGCNUM-T700-X
           MOVE    INP-CGCFIL-T700-X   TO  OUT-CGCFIL-T700-X
           MOVE    INP-CTRL-T700-X     TO  OUT-CTRL-T700-X
           MOVE    INP-NOME-T700       TO  OUT-NOME-T700
           MOVE    INP-SITUAC-T700     TO  OUT-SITUAC-T700
           MOVE    INP-ANOME-T700      TO  OUT-ANOME-T700
           MOVE    INP-MENSIT-T700     TO  OUT-MENSIT-T700
           MOVE    INP-NPROD1-T700     TO  OUT-NPROD1-T700
           MOVE    INP-NPROD2-T700     TO  OUT-NPROD2-T700
           IF      INP-NPROD2-T700     NOT EQUAL SPACES AND LOW-VALUES
                   MOVE    '-'         TO  OUT-HIFEN-T700
           ELSE
                   MOVE    SPACES      TO  OUT-HIFEN-T700.
           MOVE    INP-ORIGCD-T700-X   TO  OUT-ORIGCD-T700-X
           MOVE    INP-ORIGDS-T700     TO  OUT-ORIGDS-T700
           MOVE    INP-ATIVCD-T700-X   TO  OUT-ATIVCD-T700-X
           MOVE    INP-ATIVDS-T700     TO  OUT-ATIVDS-T700

PW9931     MOVE    WRK-INP-CTIPCE-T700-X TO WRK-OUT-CTIPCE-T700-X
PW9931     MOVE    WRK-INP-CODMOD-T700-X TO WRK-OUT-CODMOD-T700-X
PW9931     MOVE    WRK-INP-DESMOD-T700   TO WRK-OUT-DESMOD-T700
PW9931     MOVE    WRK-INP-CODVAR-T700-X TO WRK-OUT-CODVAR-T700-X
PW9931     MOVE    WRK-INP-DESVAR-T700   TO WRK-OUT-DESVAR-T700
PW9931     MOVE    WRK-INP-CODCES-T700-X TO WRK-OUT-CODCES-T700-X
PW9931     MOVE    WRK-INP-DESCES-T700   TO WRK-OUT-DESCES-T700
PW9931     MOVE    WRK-INP-ZONEAM-T700   TO WRK-OUT-ZONEAM-T700
PW9931     MOVE    WRK-INP-CONSOR-T700   TO WRK-OUT-CONSOR-T700
PW9931     MOVE    WRK-INP-CODAGR-T700-X TO WRK-OUT-CODAGR-T700-X
PW9931     MOVE    WRK-INP-DESAGR-T700   TO WRK-OUT-DESAGR-T700
PW9931     MOVE    WRK-INP-CODINT-T700-X TO WRK-OUT-CODINT-T700-X
PW9931     MOVE    WRK-INP-DESINT-T700   TO WRK-OUT-DESINT-T700
PW9931     MOVE    WRK-INP-CODGRS-T700-X TO WRK-OUT-CODGRS-T700-X
PW9931     MOVE    WRK-INP-DESGRS-T700   TO WRK-OUT-DESGRS-T700
PW9931     MOVE    WRK-INP-CODCIC-T700-X TO WRK-OUT-CODCIC-T700-X
PW9931     MOVE    WRK-INP-DESCIC-T700   TO WRK-OUT-DESCIC-T700
PRI499     MOVE    WRK-INP-CODCLI-T700-X TO WRK-OUT-CODCLI-T700-X
PRI499     MOVE    WRK-INP-DESCLI-T700   TO WRK-OUT-DESCLI-T700
0524       MOVE    WRK-INP-CODMAN-T700-X TO WRK-OUT-CODMAN-T700-X
0524       MOVE    WRK-INP-DESMAN-T700   TO WRK-OUT-DESMAN-T700
PW9931     MOVE    WRK-INP-CODCUL-T700-X TO WRK-OUT-CODCUL-T700-X
PW9931     MOVE    WRK-INP-DESCUL-T700   TO WRK-OUT-DESCUL-T700
PW9931     MOVE    WRK-INP-CODIRR-T700-X TO WRK-OUT-CODIRR-T700-X
PW9931     MOVE    WRK-INP-DESIRR-T700   TO WRK-OUT-DESIRR-T700
LMC        MOVE    WRK-INP-TPOBEN-T700-X TO WRK-OUT-TPOBEN-T700-X
LMC        MOVE    WRK-INP-DESCBE-T700   TO WRK-OUT-DESCBE-T700

PW9931     MOVE    WRK-INP-ACODMOD-T700-X TO WRK-OUT-ACODMOD-T700-X
PW9931     MOVE    WRK-INP-ACODVAR-T700-X TO WRK-OUT-ACODVAR-T700-X
PW9931     MOVE    WRK-INP-ACODCES-T700-X TO WRK-OUT-ACODCES-T700-X
PW9931     MOVE    WRK-INP-AZONEAM-T700   TO WRK-OUT-AZONEAM-T700
PW9931     MOVE    WRK-INP-ACONSOR-T700   TO WRK-OUT-ACONSOR-T700
PW9931     MOVE    WRK-INP-ACODAGR-T700-X TO WRK-OUT-ACODAGR-T700-X
PW9931     MOVE    WRK-INP-ACODINT-T700-X TO WRK-OUT-ACODINT-T700-X
PW9931     MOVE    WRK-INP-ACODGRS-T700-X TO WRK-OUT-ACODGRS-T700-X
PW9931     MOVE    WRK-INP-ACODCIC-T700-X TO WRK-OUT-ACODCIC-T700-X
PRI499     MOVE    WRK-INP-ACODCLI-T700-X TO WRK-OUT-ACODCLI-T700-X
PW9931     MOVE    WRK-INP-ACODCUL-T700-X TO WRK-OUT-ACODCUL-T700-X
PW9931     MOVE    WRK-INP-ACODIRR-T700-X TO WRK-OUT-ACODIRR-T700-X
LMC        MOVE    WRK-INP-ATPOBEN-T700-X TO WRK-OUT-ATPOBEN-T700-X

           IF      INP-AREAF-T700      NOT NUMERIC
                   MOVE    INP-AREAF-T700-X    TO  OUT-AREAF-T700-X
           ELSE
               IF  WRK-INCONS-AREAF EQUAL 'S'
                   MOVE    INP-AREAF-T700-X    TO  OUT-AREAF-T700-X
               ELSE
                   MOVE    INP-AREAF-T700      TO  OUT-AREAF-T700.

           IF      INP-QTDEF-T700      NOT NUMERIC
                   MOVE    INP-QTDEF-T700-X    TO  OUT-QTDEF-T700-X
           ELSE
               IF  WRK-INCONS-QTDEF EQUAL 'S'
                   MOVE    INP-QTDEF-T700-X    TO  OUT-QTDEF-T700-X
               ELSE
                   MOVE    INP-QTDEF-T700      TO  OUT-QTDEF-T700.

           MOVE    INP-LITERAL-T700    TO  OUT-LITERAL-T700

           IF      INP-CAMPO-T700      NOT NUMERIC
                   MOVE    INP-CAMPO-T700-X    TO  OUT-CAMPO-T700-X
           ELSE
               IF  WRK-INCONS-CAMPO EQUAL 'S'
                   MOVE    INP-CAMPO-T700-X    TO  OUT-CAMPO-T700-X
               ELSE
                   MOVE    INP-CAMPO-T700      TO  OUT-CAMPO-T700.

           IF      INP-ACAMPO-T700     NOT NUMERIC
                   MOVE    INP-ACAMPO-T700-X   TO  OUT-ACAMPO-T700-X
           ELSE
                   MOVE    INP-ACAMPO-T700     TO  OUT-ACAMPO-T700.
201503
201503     MOVE  WRK-INP-LPRDTTL-T700  TO  WRK-OUT-LPRDTTL-T700
201503
           MOVE    INP-ATTRCA-T700     TO  OUT-ATTRCA-T700
           MOVE    INP-TPEXP-T700      TO  OUT-TPEXP-T700
           MOVE    INP-ATPEXP-T700     TO  OUT-ATPEXP-T700

EG1216     MOVE    WRK-INP-NRODAP-T700  TO  WRK-OUT-NRODAP-T700
EG1216     MOVE    WRK-INP-VALDTD-T700  TO  WRK-OUT-VALDTD-T700
EG1216     MOVE    WRK-INP-VALDTM-T700  TO  WRK-OUT-VALDTM-T700
EG1216     MOVE    WRK-INP-VALDTA-T700  TO  WRK-OUT-VALDTA-T700
BR0118     MOVE    WRK-INP-GPPRONA-T700 TO  WRK-OUT-GPPRONA-T700
EG1216
PW9931     IF      WRK-INP-RECBR-T700   NOT NUMERIC
PW9931             MOVE    WRK-INP-RECBR-T700-X TO  WRK-OUT-RECBR-T700-X
PW9931     ELSE
PW9931             MOVE    WRK-INP-RECBR-T700   TO  WRK-OUT-RECBR-T700
PW9931     END-IF.

PW9931     IF      WRK-INP-ARECBR-T700   NOT NUMERIC
PW9931             MOVE  WRK-INP-ARECBR-T700-X TO  WRK-OUT-ARECBR-T700-X
PW9931     ELSE
PW9931             MOVE  WRK-INP-ARECBR-T700   TO  WRK-OUT-ARECBR-T700
PW9931     END-IF.
PW9931     MOVE    WRK-INP-TPREC-T700  TO  WRK-OUT-TPREC-T700

           MOVE    INP-IMOVEL-T700     TO  OUT-IMOVEL-T700
           MOVE    INP-ANPROD1-T700    TO  OUT-ANPROD1-T700
           MOVE    INP-AORIGCD-T700-X  TO  OUT-AORIGCD-T700-X
           MOVE    INP-AATIVCD-T700-X  TO  OUT-AATIVCD-T700-X
           IF      INP-AAREAF-T700     NOT NUMERIC
                   MOVE    INP-AAREAF-T700-X   TO  OUT-AAREAF-T700-X
           ELSE
                   MOVE    INP-AAREAF-T700     TO  OUT-AAREAF-T700.
           MOVE    INP-POSPROC-T700-X  TO  OUT-POSPROC-T700-X
           MOVE    INP-AUXDCR-T700-X   TO  OUT-AUXDCR-T700-X
           MOVE    INP-AIMOVEL-T700    TO  OUT-AIMOVEL-T700
           IF      INP-AQTDEF-T700     NOT NUMERIC
                   MOVE    INP-AQTDEF-T700-X   TO  OUT-AQTDEF-T700-X
           ELSE
                   MOVE    INP-AQTDEF-T700     TO  OUT-AQTDEF-T700.


           IF      INP-ATTRCA-T700  EQUAL  '1'
                   IF    WRK-INCONS-CAMPO EQUAL 'S'
                         NEXT SENTENCE
                   ELSE
                         MOVE  TAB-ATRIB(9)  TO  OUT-CAMPO-ATTR-T700
           ELSE
                   MOVE  TAB-ATRIB(3)  TO  OUT-CAMPO-ATTR-T700
                   MOVE  SPACES        TO  OUT-CAMPO-T700-X.

MI1708     MOVE    INP-APORTE-T700     TO  OUT-APORTE-T700.
MI1708     MOVE    INP-AGRUPO-T700     TO  OUT-AGRUPO-T700.
MI1708     MOVE    INP-PORTE-T700      TO  OUT-PORTE-T700.
MI1708     MOVE    INP-GRUPO-T700      TO  OUT-GRUPO-T700.
NI1708     MOVE    INP-LITER2-T700     TO  OUT-LITER2-T700.
MI1708     MOVE    INP-LITER3-T700     TO  OUT-LITER3-T700.

MI2008     IF INP-GRUPO-T700 EQUAL '1' OR '2' AND
MI2008        INP-PORTE-T700 EQUAL 'GRANDE' OR 'PEQUENO'
MI2008        PERFORM  120031-CONSISTE-GRUPO-PORTE
MI2008        IF  SQLCODE    NOT EQUAL   +100
MI2308            MOVE  CTPO-PROTR-RURAL OF RURCV022
MI2308                                          TO     COD-TIPO-PRODUTOR
MI2008            MOVE TAB-ATRIB(9) TO   OUT-GRUPO-ATTR-T700
MI2008            MOVE TAB-ATRIB(9) TO   OUT-PORTE-ATTR-T700.

      *---------------------------------------------------------------*
       11300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       11400-MOVE-TELA-OUT-IN SECTION.
      *---------------------------------------------------------------*

           MOVE    OUT-FASE-T700       TO  INP-FASE-T700.
           MOVE    OUT-AAGENC-T700-X   TO  INP-AAGENC-T700-X
           MOVE    OUT-ACONTA-T700-X   TO  INP-ACONTA-T700-X
           MOVE    OUT-ACGCNUM-T700-X  TO  INP-ACGCNUM-T700-X
           MOVE    OUT-ACGCFIL-T700-X  TO  INP-ACGCFIL-T700-X
           MOVE    OUT-ACTRL-T700-X    TO  INP-ACTRL-T700-X
           MOVE    OUT-AGENC-T700-X    TO  INP-AGENC-T700-X
           MOVE    OUT-CONTA-T700-X    TO  INP-CONTA-T700-X
           MOVE    OUT-CGCNUM-T700-X   TO  INP-CGCNUM-T700-X
           MOVE    OUT-CGCFIL-T700-X   TO  INP-CGCFIL-T700-X
           MOVE    OUT-CTRL-T700-X     TO  INP-CTRL-T700-X
           MOVE    OUT-NOME-T700       TO  INP-NOME-T700
           MOVE    OUT-SITUAC-T700     TO  INP-SITUAC-T700
           MOVE    OUT-ANOME-T700      TO  INP-ANOME-T700
           MOVE    OUT-MENSIT-T700     TO  INP-MENSIT-T700
           MOVE    OUT-NPROD1-T700     TO  INP-NPROD1-T700
           MOVE    OUT-NPROD2-T700     TO  INP-NPROD2-T700
           MOVE    OUT-HIFEN-T700      TO  INP-HIFEN-T700
           MOVE    OUT-ORIGCD-T700-X   TO  INP-ORIGCD-T700-X
           MOVE    OUT-ORIGDS-T700     TO  INP-ORIGDS-T700
           MOVE    OUT-ATIVCD-T700-X   TO  INP-ATIVCD-T700-X
           MOVE    OUT-ATIVDS-T700     TO  INP-ATIVDS-T700

PW9931     MOVE    WRK-OUT-CTIPCE-T700-X TO WRK-INP-CTIPCE-T700-X
PW9931     MOVE    WRK-OUT-CODMOD-T700-X TO WRK-INP-CODMOD-T700-X
PW9931     MOVE    WRK-OUT-DESMOD-T700   TO WRK-INP-DESMOD-T700
PW9931     MOVE    WRK-OUT-CODVAR-T700-X TO WRK-INP-CODVAR-T700-X
PW9931     MOVE    WRK-OUT-DESVAR-T700   TO WRK-INP-DESVAR-T700
PW9931     MOVE    WRK-OUT-CODCES-T700-X TO WRK-INP-CODCES-T700-X
PW9931     MOVE    WRK-OUT-DESCES-T700   TO WRK-INP-DESCES-T700
PW9931     MOVE    WRK-OUT-ZONEAM-T700   TO WRK-INP-ZONEAM-T700
PW9931     MOVE    WRK-OUT-CONSOR-T700   TO WRK-INP-CONSOR-T700
PW9931     MOVE    WRK-OUT-CODAGR-T700-X TO WRK-INP-CODAGR-T700-X
PW9931     MOVE    WRK-OUT-DESAGR-T700   TO WRK-INP-DESAGR-T700
PW9931     MOVE    WRK-OUT-CODINT-T700-X TO WRK-INP-CODINT-T700-X
PW9931     MOVE    WRK-OUT-DESINT-T700   TO WRK-INP-DESINT-T700
PW9931     MOVE    WRK-OUT-CODGRS-T700-X TO WRK-INP-CODGRS-T700-X
PW9931     MOVE    WRK-OUT-DESGRS-T700   TO WRK-INP-DESGRS-T700
PW9931     MOVE    WRK-OUT-CODCIC-T700-X TO WRK-INP-CODCIC-T700-X
PW9931     MOVE    WRK-OUT-DESCIC-T700   TO WRK-INP-DESCIC-T700
PRI499     MOVE    WRK-OUT-CODCLI-T700-X TO WRK-INP-CODCLI-T700-X
PRI499     MOVE    WRK-OUT-DESCLI-T700   TO WRK-INP-DESCLI-T700
0524       MOVE    WRK-OUT-CODMAN-T700-X TO WRK-INP-CODMAN-T700-X
0524       MOVE    WRK-OUT-DESMAN-T700   TO WRK-INP-DESMAN-T700
PW9931     MOVE    WRK-OUT-CODCUL-T700-X TO WRK-INP-CODCUL-T700-X
PW9931     MOVE    WRK-OUT-DESCUL-T700   TO WRK-INP-DESCUL-T700
PW9931     MOVE    WRK-OUT-CODIRR-T700-X TO WRK-INP-CODIRR-T700-X
PW9931     MOVE    WRK-OUT-DESIRR-T700   TO WRK-INP-DESIRR-T700
LMC        MOVE    WRK-OUT-TPOBEN-T700-X TO WRK-INP-TPOBEN-T700-X
LMC        MOVE    WRK-OUT-DESCBE-T700   TO WRK-INP-DESCBE-T700

PW9931     MOVE    WRK-OUT-ACODMOD-T700-X TO WRK-INP-ACODMOD-T700-X
PW9931     MOVE    WRK-OUT-ACODVAR-T700-X TO WRK-INP-ACODVAR-T700-X
PW9931     MOVE    WRK-OUT-ACODCES-T700-X TO WRK-INP-ACODCES-T700-X
PW9931     MOVE    WRK-OUT-AZONEAM-T700   TO WRK-INP-AZONEAM-T700
PW9931     MOVE    WRK-OUT-ACONSOR-T700   TO WRK-INP-ACONSOR-T700
PW9931     MOVE    WRK-OUT-ACODAGR-T700-X TO WRK-INP-ACODAGR-T700-X
PW9931     MOVE    WRK-OUT-ACODINT-T700-X TO WRK-INP-ACODINT-T700-X
PW9931     MOVE    WRK-OUT-ACODGRS-T700-X TO WRK-INP-ACODGRS-T700-X
PW9931     MOVE    WRK-OUT-ACODCIC-T700-X TO WRK-INP-ACODCIC-T700-X
PRI499     MOVE    WRK-OUT-ACODCLI-T700-X TO WRK-INP-ACODCLI-T700-X
PW9931     MOVE    WRK-OUT-ACODCUL-T700-X TO WRK-INP-ACODCUL-T700-X
PW9931     MOVE    WRK-OUT-ACODIRR-T700-X TO WRK-INP-ACODIRR-T700-X
LMC        MOVE    WRK-OUT-ATPOBEN-T700-X TO WRK-INP-ATPOBEN-T700-X

           MOVE    OUT-AREAF-T700-X    TO  INP-AREAF-T700-X
           MOVE    OUT-QTDEF-T700-X    TO  INP-QTDEF-T700-X
           MOVE    OUT-LITERAL-T700    TO  INP-LITERAL-T700
           MOVE    OUT-CAMPO-T700-X    TO  INP-CAMPO-T700-X
           MOVE    OUT-ACAMPO-T700-X   TO  INP-ACAMPO-T700-X
EG1216
EG1216     MOVE    WRK-OUT-NRODAP-T700  TO  WRK-INP-NRODAP-T700
EG1216     MOVE    WRK-OUT-VALDTD-T700  TO  WRK-INP-VALDTD-T700
EG1216     MOVE    WRK-OUT-VALDTM-T700  TO  WRK-INP-VALDTM-T700
EG1216     MOVE    WRK-OUT-VALDTA-T700  TO  WRK-INP-VALDTA-T700
BR0118     MOVE    WRK-OUT-GPPRONA-T700 TO  WRK-INP-GPPRONA-T700
201503
201503     MOVE    WRK-OUT-LPRDTTL-T700    TO  WRK-INP-LPRDTTL-T700
201503
           MOVE    OUT-ATTRCA-T700     TO  INP-ATTRCA-T700
           MOVE    OUT-TPEXP-T700      TO  INP-TPEXP-T700
           MOVE    OUT-ATPEXP-T700     TO  INP-ATPEXP-T700
PW9931     MOVE    WRK-OUT-RECBR-T700-X  TO  WRK-INP-RECBR-T700-X
PW9931     MOVE    WRK-OUT-ARECBR-T700-X TO  WRK-INP-ARECBR-T700-X
PW9931     MOVE    WRK-OUT-TPREC-T700  TO  WRK-INP-TPREC-T700
           MOVE    OUT-IMOVEL-T700     TO  INP-IMOVEL-T700
           MOVE    OUT-ANPROD1-T700    TO  INP-ANPROD1-T700
           MOVE    OUT-AORIGCD-T700-X  TO  INP-AORIGCD-T700-X
           MOVE    OUT-AATIVCD-T700-X  TO  INP-AATIVCD-T700-X
           MOVE    OUT-AAREAF-T700-X   TO  INP-AAREAF-T700-X
           MOVE    OUT-POSPROC-T700-X  TO  INP-POSPROC-T700-X
           MOVE    OUT-AUXDCR-T700-X   TO  INP-AUXDCR-T700-X
           MOVE    OUT-AIMOVEL-T700    TO  INP-AIMOVEL-T700
           MOVE    OUT-AQTDEF-T700-X   TO  INP-AQTDEF-T700-X

           IF      INP-ATTRCA-T700  EQUAL  '1'
                   IF    WRK-INCONS-CAMPO EQUAL 'S'
                         NEXT SENTENCE
                   ELSE
                         MOVE  TAB-ATRIB(9)  TO  OUT-CAMPO-ATTR-T700
           ELSE
                   MOVE  TAB-ATRIB(3)  TO  OUT-CAMPO-ATTR-T700
                   MOVE  SPACES        TO  OUT-CAMPO-T700-X.

MI1708     MOVE    OUT-APORTE-T700     TO  INP-APORTE-T700.
MI1708     MOVE    OUT-AGRUPO-T700     TO  INP-AGRUPO-T700.
MI1708     MOVE    OUT-PORTE-T700      TO  INP-PORTE-T700.
MI1708     MOVE    OUT-GRUPO-T700      TO  INP-GRUPO-T700.
MI1708     MOVE    OUT-LITER2-T700     TO  INP-LITER2-T700.
MI1708     MOVE    OUT-LITER3-T700     TO  INP-LITER3-T700.

      *---------------------------------------------------------------*
       11400-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA RUECT700       *
      *****************************************************************

      *---------------------------------------------------------------*
       12000-PROCESSAR-RUECT700 SECTION.
      *---------------------------------------------------------------*

           MOVE    'RUECT700'              TO   WRK-TELA.

           IF  WRK-CONTINUACAO          EQUAL   'S'
               PERFORM 12003-EDITA-CAMPOS-DECIMAIS
               PERFORM 11300-MOVE-TELA-IN-OUT
               PERFORM 12004-RETORNA-INSPECT
               IF  WRK-INCONSIS     EQUAL 'S' OR
                   WRK-INCONS-AREAF EQUAL 'S' OR
                   WRK-INCONS-QTDEF EQUAL 'S' OR
STEFA2*           XWRK-INCONS-QTDEF EQUAL 'A' OR
                   WRK-INCONS-CAMPO EQUAL 'S' OR
                   WRK-INCONS-GRUPO EQUAL 'S' OR
                   WRK-INCONS-PORTE EQUAL 'S'
STEFA2*           XIF  WRK-INCONS-QTDEF
.     *                            XEQUAL 'A'
.     *               XIF  INP-PFK-T700
.     *                            XEQUAL '6'
.     *                   XPERFORM 7200-ACESSAR-CRUR236G
.     *                   XIF WRK-UNIDADE-ANIMAL
.     *                            XEQUAL 'S'
.     *                      XPERFORM 7400-TRATA-PF6
.     *                  XXELSE
.     *                      XMOVE WRK-M63
.     *                            XTO OUT-MENSAG-T700
TIRAR *                      XDISPLAY 'OUT-MENSAG..' OUT-MENSAG-T700
TIRAR *                                       XUPON CONSOLE
.     *                   XEND-IF
.     *               XEND-IF
STEFA2*           XEND-IF

                   GO TO 12000-99-FIM
               ELSE
                   GO  TO  12000-00-VER-CONTINUACAO.

           MOVE    WRK-MENSAGEM           TO  INPUT-T700.
201503
201503     IF  INP-FASE-T700           EQUAL SPACES OR LOW-VALUES
201503         MOVE '1'                TO  INP-FASE-T700
201503     END-IF.
201503
201503     IF  INP-AAGENC-T700-X       EQUAL SPACES OR LOW-VALUES
201503         MOVE ZEROS              TO INP-AAGENC-T700
201503     END-IF.
201503
201503     IF  INP-ACONTA-T700-X       EQUAL SPACES OR LOW-VALUES
201503         MOVE ZEROS              TO INP-ACONTA-T700
201503     END-IF.
201503
           PERFORM 12003-EDITA-CAMPOS-DECIMAIS.

           PERFORM 11300-MOVE-TELA-IN-OUT.

           PERFORM 12004-RETORNA-INSPECT.

           IF  WRK-INCONSIS     EQUAL 'S' OR
               WRK-INCONS-AREAF EQUAL 'S' OR
               WRK-INCONS-QTDEF EQUAL 'S' OR
STEFA2         WRK-INCONS-QTDEF EQUAL 'A' OR
               WRK-INCONS-CAMPO EQUAL 'S' OR
               WRK-INCONS-GRUPO EQUAL 'S' OR
               WRK-INCONS-PORTE EQUAL 'S'
GUI***             IF  WRK-INCONS-QTDEF
.                                   EQUAL 'A'
.                      IF  INP-PFK-T700
.                                   EQUAL '6'
.                          PERFORM 7200-ACESSAR-CRUR236G
.                          IF WRK-UNIDADE-ANIMAL
.                                   EQUAL 'S'
.                             PERFORM 7400-TRATA-PF6
TIRAR**                   XELSE
TIRAR**                      XMOVE WRK-M63
TIRAR**                            XTO OUT-MENSAG-T700
TIRAR**                      XDISPLAY 'OUT-MENSAG..' OUT-MENSAG-T700
TIRAR**                                       XUPON CONSOLE
.                          END-IF
.                      END-IF
GUI**              END-IF
               GO TO 12000-99-FIM.

           IF  INP-PFK-T700 EQUAL '*'
               MOVE    INP-MENSAG-T700     TO   OUT-MENSAG-T700
               GO  TO  12000-99-FIM.

PW9931     IF  INP-PFK-T700 EQUAL 'Z'
PW9931         PERFORM 19000-REINICIA-EMPREENDIMENTO
PW9931     END-IF.

202502     IF  INP-PFK-T700  EQUAL '6'
.              PERFORM 7200-ACESSAR-CRUR236G
.            IF WRK-UNIDADE-ANIMAL   EQUAL 'S'
.              PERFORM 7400-TRATA-PF6
.              GO  TO  12000-99-FIM
.            END-IF
202502     END-IF.

      ****************************************
      ***  TRATAMENTO DE INCLUSAO DO PROGRAMA
      ****************************************
       12000-00-INCLUSAO.

           IF  WRK-CONTINUACAO EQUAL SPACES
               PERFORM 11200-SELECT-RUECB042
               IF  SQLCODE     EQUAL   +100
                   IF  INP-POSPROC-T700 LESS 1
                       NEXT SENTENCE
                   ELSE
                       MOVE    IO-LTERM              TO  M10-CAMPO
                       MOVE    WRK-M10               TO  OUT-MENSAG-T700
                       GO  TO  12000-99-FIM.


      ********************************************************
      ***  VERIFICA E POSICIONA ALTERACAO DOS DADOS DIGITADOS
      ********************************************************

           IF  INP-AGENC-T700-X NOT EQUAL INP-AAGENC-T700-X
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 LESS 1
                   GO  TO  12000-01-TRATA-DADOS-PROPON
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-CONTA-T700-X NOT EQUAL INP-ACONTA-T700-X
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 LESS 1
                   GO  TO  12000-01-TRATA-DADOS-PROPON
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-CGCNUM-T700-X NOT EQUAL INP-ACGCNUM-T700-X
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 LESS 1
                   GO  TO  12000-01-TRATA-DADOS-PROPON
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                  GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-CGCFIL-T700-X NOT EQUAL INP-ACGCFIL-T700-X
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 LESS 1
                   GO  TO  12000-01-TRATA-DADOS-PROPON
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-CTRL-T700-X NOT EQUAL INP-ACTRL-T700-X
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 LESS 1
                   GO  TO  12000-01-TRATA-DADOS-PROPON
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-NOME-T700   NOT EQUAL INP-ANOME-T700
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 LESS 1
                   GO  TO  12000-01-TRATA-DADOS-PROPON
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-NPROD1-T700   NOT EQUAL INP-ANPROD1-T700
               MOVE INP-ANPROD1-T700       TO     WRK-NPROD1-ANT
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               MOVE WRK-NPROD1-ANT         TO     OUT-ANPROD1-T700
               IF  INP-POSPROC-T700 GREATER 2
                   MOVE    'COEXC'         TO     WRK-CRSUMO1
                   MOVE    'MUDIF'         TO     WRK-CRSUMO2
                   MOVE    'CEBLO'         TO     WRK-CRSUMO3
                   MOVE    'TPPRO'         TO     WRK-CRSUMO4
                   MOVE    'PRDBL'         TO     WRK-CRSUMO5
                   MOVE    'ORIBL'         TO     WRK-CRSUMO6
                   PERFORM 12400-LIMPA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-04-TRATA-PROD-FINALIDADE
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-ORIGCD-T700-X NOT EQUAL INP-AORIGCD-T700
               MOVE INP-AORIGCD-T700-X     TO     WRK-ORIGCD-ANT
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               MOVE WRK-ORIGCD-ANT         TO     OUT-AORIGCD-T700-X
               IF  INP-POSPROC-T700 GREATER 3
                   MOVE    'COEXC'         TO     WRK-CRSUMO1
                   MOVE    'MUDIF'         TO     WRK-CRSUMO2
                   MOVE    'CEBLO'         TO     WRK-CRSUMO3
                   MOVE    'TPPRO'         TO     WRK-CRSUMO4
                   MOVE    'ORIBL'         TO     WRK-CRSUMO5
                   PERFORM 12400-LIMPA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-05-TRATA-ORIGEM-RECURSO
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-ATIVCD-T700-X NOT EQUAL INP-AATIVCD-T700
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 GREATER 3
                   MOVE    'COEXC'         TO     WRK-CRSUMO1
                   MOVE    'MUDIF'         TO     WRK-CRSUMO2
                   MOVE    'CEBLO'         TO     WRK-CRSUMO3
                   MOVE    'TPPRO'         TO     WRK-CRSUMO4
                   MOVE    'ORIBL'         TO     WRK-CRSUMO5
                   PERFORM 12400-LIMPA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-05-TRATA-ORIGEM-RECURSO
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-AREAF-T700-X NOT EQUAL INP-AAREAF-T700-X
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 GREATER 3
                   MOVE    'COEXC'         TO     WRK-CRSUMO1
                   MOVE    'MUDIF'         TO     WRK-CRSUMO2
                   MOVE    'CEBLO'         TO     WRK-CRSUMO3
                   MOVE    'TPPRO'         TO     WRK-CRSUMO4
                   MOVE    'ORIBL'         TO     WRK-CRSUMO5
                   PERFORM 12400-LIMPA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-05-CONTINUA
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-QTDEF-T700-X NOT EQUAL INP-AQTDEF-T700-X
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 GREATER 3
                   MOVE    'COEXC'         TO     WRK-CRSUMO1
                   MOVE    'MUDIF'         TO     WRK-CRSUMO2
                   MOVE    'CEBLO'         TO     WRK-CRSUMO3
                   MOVE    'TPPRO'         TO     WRK-CRSUMO4
                   MOVE    'ORIBL'         TO     WRK-CRSUMO5
                   PERFORM 12400-LIMPA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-05-CONTINUA
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-CAMPO-T700-X NOT EQUAL INP-ACAMPO-T700-X
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 GREATER 3
                   MOVE    'COEXC'         TO     WRK-CRSUMO1
                   MOVE    'MUDIF'         TO     WRK-CRSUMO2
                   MOVE    'CEBLO'         TO     WRK-CRSUMO3
                   MOVE    'TPPRO'         TO     WRK-CRSUMO4
                   MOVE    'ORIBL'         TO     WRK-CRSUMO5
                   PERFORM 12400-LIMPA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-05-CONTINUA
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

MI1708     IF  INP-GRUPO-T700   NOT EQUAL INP-AGRUPO-T700 OR
MI1708         INP-PORTE-T700   NOT EQUAL INP-APORTE-T700
MI1708         PERFORM 12050-MOVE-CAMPOS-ANT-ATU
MI1708         IF  INP-POSPROC-T700 GREATER 3
MI1708             MOVE    'COEXC'         TO     WRK-CRSUMO1
MI1708             MOVE    'MUDIF'         TO     WRK-CRSUMO2
MI1708             MOVE    'CEBLO'         TO     WRK-CRSUMO3
MI1708             MOVE    'TPPRO'         TO     WRK-CRSUMO4
MI1708             MOVE    'ORIBL'         TO     WRK-CRSUMO5
MI1708             PERFORM 12400-LIMPA-TAB-INCONS
MI1708                                        VARYING IND2 FROM 1 BY 1
MI1708                                        UNTIL   IND2 GREATER   7
MI1708             GO  TO  12000-05-CONTINUA
MI1708         ELSE
MI1708             PERFORM 12401-LIMPA-TODA-TAB-INCONS
MI1708                                        VARYING IND2 FROM 1 BY 1
MI1708                                        UNTIL   IND2 GREATER   7
MI1708             GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-TPEXP-T700   NOT EQUAL INP-ATPEXP-T700
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 GREATER 6
                   MOVE    'MUDIF'         TO     WRK-CRSUMO1
                   MOVE    'CEBLO'         TO     WRK-CRSUMO2
                   MOVE    'TPPRO'         TO     WRK-CRSUMO3
                   PERFORM 12400-LIMPA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-08-TRATA-DENOM-IMOVEL
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

PW9931     IF  WRK-INP-RECBR-T700-X NOT EQUAL WRK-INP-ARECBR-T700-X
PW9931         PERFORM 12050-MOVE-CAMPOS-ANT-ATU
PW9931         IF  INP-POSPROC-T700 GREATER 6
PW9931             MOVE    'MUDIF'         TO     WRK-CRSUMO1
PW9931             MOVE    'CEBLO'         TO     WRK-CRSUMO2
PW9931             MOVE    'TPPRO'         TO     WRK-CRSUMO3
PW9931             PERFORM 12400-LIMPA-TAB-INCONS
PW9931                                        VARYING IND2 FROM 1 BY 1
PW9931                                        UNTIL   IND2 GREATER   7
PW9931             GO  TO  20000-01-TRATA-RECEITA-BRUTA
PW9931         ELSE
PW9931             PERFORM 12401-LIMPA-TODA-TAB-INCONS
PW9931                                        VARYING IND2 FROM 1 BY 1
PW9931                                        UNTIL   IND2 GREATER   7
PW9931             GO  TO  12000-01-TRATA-DADOS-PROPON.

           IF  INP-IMOVEL-T700  NOT EQUAL INP-AIMOVEL-T700
               PERFORM 12050-MOVE-CAMPOS-ANT-ATU
               IF  INP-POSPROC-T700 GREATER 6
                   MOVE    'MUDIF'         TO     WRK-CRSUMO1
                   MOVE    'CEBLO'         TO     WRK-CRSUMO2
                   MOVE    'TPPRO'         TO     WRK-CRSUMO3
                   PERFORM 12400-LIMPA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-08-TRATA-DENOM-IMOVEL
               ELSE
                   PERFORM 12401-LIMPA-TODA-TAB-INCONS
                                              VARYING IND2 FROM 1 BY 1
                                              UNTIL   IND2 GREATER   7
                   GO  TO  12000-01-TRATA-DADOS-PROPON.

PW9931     IF WRK-INP-TPREC-T700 EQUAL 1
PW9931        MOVE  TAB-ATRIB(8)           TO WRK-OUT-RECBR-ATTR-T700
PW9931     ELSE
PW9931        MOVE  TAB-ATRIB(3)           TO WRK-OUT-RECBR-ATTR-T700
PW9931     END-IF.

           IF  INP-FASE-T700  EQUAL  '3'
               MOVE INP-MENSAG-T700     TO  OUT-MENSAG-T700
               GO TO 12000-99-FIM.


      ****************
      ***  TRATA PFK9
      ****************

           IF  INP-PFK-T700  EQUAL '9'
PW9931         IF ((INP-LIN-T700     EQUAL 07) AND
                   (INP-COL-T700 GREATER 15 AND LESS 81))
                   IF  INP-POSPROC-T700  GREATER 2
                       GO  TO  12000-04-TRATA-PROD-FINALIDADE
                   ELSE
                       GO  TO  12000-01-TRATA-DADOS-PROPON
               ELSE
PW9931         IF ((INP-LIN-T700     EQUAL 08) AND
                   (INP-COL-T700 GREATER 17 AND LESS 54))
                   IF  INP-POSPROC-T700  GREATER 3
                       GO  TO  12000-05-TRATA-ORIGEM-RECURSO
                   ELSE
                       GO  TO  12000-00-VER-POSPROC-TELA
               ELSE
PW9931         IF ((INP-LIN-T700     EQUAL 19) AND
                   (INP-COL-T700 GREATER 30 AND LESS 56))
                   IF  INP-POSPROC-T700  GREATER 6
                       GO  TO  12000-08-TRATA-DENOM-IMOVEL
                   ELSE
                       GO  TO  12000-00-VER-POSPROC-TELA.

PW9931     IF  INP-PFK-T700  EQUAL '9'
PW9931         IF ((INP-LIN-T700     EQUAL 09) AND
PW9931             (INP-COL-T700 GREATER 17 AND LESS 21))
PW9931             IF  INP-POSPROC-T700  GREATER 4,2
PW9931                 GO  TO  16010-00-TRATA-MODALIDADE
PW9931             ELSE
PW9931                 GO  TO  12000-00-VER-POSPROC-TELA
PW9931             END-IF
PW9931         END-IF
PW9931         IF ((INP-LIN-T700     EQUAL 09) AND
PW9931             (INP-COL-T700 GREATER 54 AND LESS 58))
PW9931             IF  INP-POSPROC-T700  GREATER 4,2
PW9931                 GO  TO  16020-00-TRATA-VARIEDADE
PW9931             ELSE
PW9931                 GO  TO  12000-00-VER-POSPROC-TELA
PW9931             END-IF
PW9931         END-IF
PW9931         IF ((INP-LIN-T700     EQUAL 10) AND
PW9931             (INP-COL-T700 GREATER 17 AND LESS 21))
PW9931             IF  INP-POSPROC-T700  GREATER 4,2
PW9931                 GO  TO  16030-00-TRATA-CESTA
PW9931             ELSE
PW9931                 GO  TO  12000-00-VER-POSPROC-TELA
PW9931             END-IF
PW9931         END-IF
PW9931         IF ((INP-LIN-T700     EQUAL 10) AND
PW9931             (INP-COL-T700 GREATER 77 AND LESS 79) AND
PW9931             (WRK-INP-CONSOR-T700 EQUAL 'S'))
PW9931                 GO  TO  16500-00-TRATA-CONSORCIO
PW9931         END-IF
PW9931         IF ((INP-LIN-T700     EQUAL 12) AND
PW9931             (INP-COL-T700 GREATER 30 AND LESS 34))
PW9931             IF  INP-POSPROC-T700  GREATER 4,4
PW9931                 GO  TO  17010-00-TRATA-AGROPECUARIA
PW9931             ELSE
PW9931                 GO  TO  12000-00-VER-POSPROC-TELA
PW9931             END-IF
PW9931         END-IF
PW9931         IF ((INP-LIN-T700     EQUAL 12) AND
PW9931             (INP-COL-T700 GREATER 62 AND LESS 65))
PW9931             IF  INP-POSPROC-T700  GREATER 4,4
PW9931*               XGO  TO  17020-00-TRATA-INTEGRACAO
PW9931                 PERFORM 17020-00-TRATA-INTEGRACAO
PRI499                 GO TO 12000-99-FIM
PW9931             ELSE
PW9931                 GO  TO  12000-00-VER-POSPROC-TELA
PW9931             END-IF
PW9931         END-IF
PW9931         IF ((INP-LIN-T700     EQUAL 13) AND
PW9931             (INP-COL-T700 GREATER 31 AND LESS 33))
PW9931             IF  INP-POSPROC-T700  GREATER 4,4
PW9931                 GO  TO  17030-00-TRATA-GRAO-SEMENTE
PW9931             ELSE
PW9931                 GO  TO  12000-00-VER-POSPROC-TELA
PW9931             END-IF
PW9931         END-IF
PW9931         IF ((INP-LIN-T700     EQUAL 13) AND
PW9931             (INP-COL-T700 GREATER 62 AND LESS 66))
PW9931             IF  INP-POSPROC-T700  GREATER 4,4
PW9931                 GO  TO  17040-00-TRATA-IRRIGACAO
PW9931             ELSE
PW9931                 GO  TO  12000-00-VER-POSPROC-TELA
PW9931             END-IF
PW9931         END-IF
PW9931         IF ((INP-LIN-T700     EQUAL 14) AND
PW9931*            (INP-COL-T700 GREATER 30 AND LESS 34))
PRI499             (INP-COL-T700 GREATER 10 AND LESS 14))
PW9931             IF  INP-POSPROC-T700  GREATER 4,4
PW9931                 GO  TO  17050-00-TRATA-CULTIVO
PW9931             ELSE
PW9931                 GO  TO  12000-00-VER-POSPROC-TELA
PW9931             END-IF
PW9931         END-IF
PW9931         IF ((INP-LIN-T700     EQUAL 14) AND
PW9931*            (INP-COL-T700 GREATER 62 AND LESS 66))
PRI499             (INP-COL-T700 GREATER 40 AND LESS 44))
PW9931             IF  INP-POSPROC-T700  GREATER 4,4
PW9931                 GO  TO  17060-00-TRATA-CICLO
PW9931             ELSE
PW9931                 GO  TO  12000-00-VER-POSPROC-TELA
PW9931             END-IF
PW9931         END-IF
PRI499         IF ((INP-LIN-T700     EQUAL 14) AND
PRI499             (INP-COL-T700 GREATER 64 AND LESS 67))
PRI499             IF  INP-POSPROC-T700  GREATER 4,4
PRI499                 PERFORM 1765-00-TRATA-CLIMA
PRI499             ELSE
PRI499                 GO  TO  12000-00-VER-POSPROC-TELA
PW9931             END-IF
PW9931         END-IF
PW9931     END-IF.

      *****************
      ***  TRATA PFK11
      *****************

           IF  INP-PFK-T700  EQUAL 'A'
               IF  INP-FASE-T700  EQUAL '2'
                   MOVE  '1'        TO  OUT-FASE-T700
                   MOVE  'S'        TO  FLAG-GRAVA-INCONS
                   PERFORM 12200-CARREGA-RESP-APROV
                   IF INP-AUXDCR-T700 EQUAL 1
101020                PERFORM 1200-00-OBTEM-FORMA-PAGTO
101020                GO TO 12000-99-FIM
                   ELSE
                   IF INP-AUXDCR-T700 EQUAL 2
                      GO  TO  12000-06-RETORNO-PF11
                   ELSE
                   IF INP-AUXDCR-T700 EQUAL 3
                      GO  TO  12000-08-RETORNO-PF11
                   ELSE
                   IF INP-AUXDCR-T700 EQUAL 4
                      GO  TO  12000-10-RETORNO-PF11
                   ELSE
                   IF INP-AUXDCR-T700 EQUAL 5
                      GO  TO  12000-11-RETORNO-PF11
                   ELSE
                      GO  TO  12000-05-CONTINUA.

      *************************************
      ***  VERIFICA CONTINUACAO DE RESTART
      *************************************
       12000-00-VER-CONTINUACAO.

           IF  WRK-CONTINUACAO  EQUAL   'S'
               IF PROGRAMA-ANT  EQUAL   'RUEC7510'
                  GO  TO  12000-01-RETORNO-RUEC7510
               END-IF
               IF PROGRAMA-ANT  EQUAL   'RUEC7520'
                  GO  TO  12000-02-RETORNO-RUEC7520
               END-IF
               IF PROGRAMA-ANT  EQUAL   'RUEC7550'
                  GO  TO  12000-04-RETORNO-RUEC7550
               END-IF
               IF PROGRAMA-ANT  EQUAL   'RUEC7560'
                  GO  TO  12000-05-RETORNO-RUEC7560
               END-IF
               IF PROGRAMA-ANT  EQUAL   'RUEC7530'
                  GO  TO  12000-08-RETORNO-RUEC7530
               END-IF
PW9931         IF PROGRAMA-ANT  EQUAL   'RUEC6970'
PW9931*          XGO  TO  16010-00-RETORNO-RUEC6970
PRI499            PERFORM 16010-00-RETORNO-RUEC6970
PW9931         END-IF
PW9931         IF PROGRAMA-ANT  EQUAL   'RUEC6980'
PW9931*          XGO  TO  16020-00-RETORNO-RUEC6980
PW9931            PERFORM 16020-00-RETORNO-RUEC6980
PW9931         END-IF
PW9931         IF PROGRAMA-ANT  EQUAL   'RUEC6990'
PW9931*          XGO  TO  16030-00-RETORNO-RUEC6990
PRI499            PERFORM 16030-00-RETORNO-RUEC6990
PW9931         END-IF
PW9931         IF PROGRAMA-ANT  EQUAL   'RUEC6230'
PW9931*          XGO  TO  16500-00-RETORNO-RUEC6230
PW9931            PERFORM 16500-00-RETORNO-RUEC6230
PW9931         END-IF
PW9931         IF PROGRAMA-ANT  EQUAL   'RUEC6140'
PW9931*          XGO  TO  17010-00-RETORNO-RUEC6140
PRI499            PERFORM 17010-00-RETORNO-RUEC6140
PRI499            GO TO 12000-99-FIM
PW9931         END-IF
PW9931         IF PROGRAMA-ANT  EQUAL   'RUEC6150'
PW9931*          XGO  TO  17020-00-RETORNO-RUEC6150
PW9931            PERFORM 17020-00-RETORNO-RUEC6150
PRI499            GO TO 12000-99-FIM
PW9931         END-IF
PW9931         IF PROGRAMA-ANT  EQUAL   'RUEC6160'
PW9931*          XGO  TO  17030-00-RETORNO-RUEC6160
PRI499            PERFORM 17030-00-RETORNO-RUEC6160
PRI499            GO TO 12000-99-FIM
PW9931         END-IF
PW9931         IF PROGRAMA-ANT  EQUAL   'RUEC6170'
PW9931*          XGO  TO  17040-00-RETORNO-RUEC6170
PRI499            PERFORM 17040-00-RETORNO-RUEC6170
PRI499            GO TO 12000-99-FIM
PW9931         END-IF
PW9931         IF PROGRAMA-ANT  EQUAL   'RUEC6180'
PW9931*          XGO  TO  17050-00-RETORNO-RUEC6180
PRI499            PERFORM 17050-00-RETORNO-RUEC6180
PRI499            GO TO 12000-99-FIM
PW9931         END-IF
PW9931         IF PROGRAMA-ANT  EQUAL   'RUEC6190'
PRI499            PERFORM 17060-00-RETORNO-RUEC6190
PRI499            GO TO 12000-99-FIM
PRI499         END-IF
PRI499         IF PROGRAMA-ANT  EQUAL   'RUEC6210'
PRI499            PERFORM 1766-00-RETORNO-RUEC6210
0524  *           G O 12000-05-CONTINUA
0524              GO TO 12000-99-FIM
PRI499         END-IF
0524           IF PROGRAMA-ANT  EQUAL   'RUEC6260'
0524              PERFORM 1767-00-RETORNO-RUEC6260
0524              GO TO 12000-05-CONTINUA
0524           END-IF
PRI499     END-IF.
LMC
LMC        IF  WRK-CONTINUACAO  EQUAL   'S'
LMC            IF PROGRAMA-ANT  EQUAL   'RUEC7750'
LMC               PERFORM 2397-00-RETORNO-RUEC7750
201609            GO TO 12000-05-CONTINUA
LMC            END-IF
LMC        END-IF.

101020     IF  WRK-CONTINUACAO  EQUAL   'S'
101020         IF PROGRAMA-ANT  EQUAL   'RUEC7600'
101020            GO TO 12000-03-VALIDA-FICHA-CAD
101020         END-IF
101020     END-IF.

WI1121     IF  WRK-CONTINUACAO  EQUAL   'S'
WI1121         IF PROGRAMA-ANT  EQUAL   'RUEC9300'
WI1121            PERFORM 2396-00-RETORNO-RUEC9300
WI1121         END-IF
WI1121     END-IF.

STEFA2     IF  WRK-CONTINUACAO  EQUAL   'S'
.              IF PROGRAMA-ANT  EQUAL   'RUEC7961'
.                 PERFORM 2400-00-RETORNO-RUEC7961
.              END-IF
STEFA2     END-IF.

      *****************************************************************
      ***  VERIF. POSICIONAMENTO NA SITUACAO DE PFK03 NOS PGMS CHAMADOS
      *****************************************************************

PW9931     PERFORM 15020-00-TRATA-NOVO-RECOR THRU 15020-99-FIM.

           IF PROGRAMA-ANT  EQUAL   'RUEC7000'
              IF  INP-POSPROC-T700 EQUAL ZEROS
                  GO  TO  12000-01-TRATA-DADOS-PROPON
              ELSE
              IF  INP-POSPROC-T700 EQUAL 1
                  GO  TO  12000-02-TRATA-SITUACAO-CLIE
              ELSE
              IF  INP-POSPROC-T700 EQUAL 2
                  GO  TO  12000-03-VALIDA-FICHA-CAD
              ELSE
              IF  INP-POSPROC-T700 EQUAL 3
                  GO  TO  12000-04-TRATA-PROD-FINALIDADE
              ELSE
              IF  INP-POSPROC-T700 EQUAL 4
                  GO  TO  12000-05-TRATA-ORIGEM-RECURSO
              ELSE
PW9931        IF  INP-POSPROC-T700 EQUAL 4,2
PW9931            GO TO   16000-00-TRATA-EMPREENDIMENTO
PW9931        ELSE
PW9931        IF  INP-POSPROC-T700 EQUAL 4,3
PW9931*          XGO TO   16040-00-CONTINUA
PRI499            PERFORM 16040-00-CONTINUA
PW9931        ELSE
PW9931        IF  INP-POSPROC-T700 EQUAL 4,4
PW9931*          XGO  TO  17000-00-TRATA-SIST-PRODUCAO
PRI499            PERFORM 1700-00-TRATA-SIST-PRODUCAO
PRI499            GO  TO  12000-99-FIM
PW9931        ELSE
              IF  INP-POSPROC-T700 EQUAL 5
                  GO  TO  12000-06-TRATA-QTDE-CONTRATOS
              ELSE
              IF  INP-POSPROC-T700 EQUAL 6
                  GO  TO  12000-07-TRATA-TAXA-JUROS
              ELSE
              IF  INP-POSPROC-T700 EQUAL 7
                  IF  INP-FASE-T700 EQUAL '2'
                      GO  TO  12000-08-RETORNO-RUEC7530
                  ELSE
                      GO  TO  12000-08-TRATA-DENOM-IMOVEL
              ELSE
              IF  INP-POSPROC-T700 EQUAL 8
                  GO  TO  12000-09-TRATA-LAVOURA
              ELSE
PW9931        IF  INP-POSPROC-T700 EQUAL 8,5
PW9931            GO  TO  16500-00-TRATA-CONSORCIO
PW9931        ELSE
              IF  INP-POSPROC-T700 EQUAL 9
                  GO  TO  12000-10-TRATA-BLOQUEIOS
              ELSE
              IF  INP-POSPROC-T700 EQUAL 10
                  GO  TO  12000-11-TRATA-TIPO-PRODUTOR
              ELSE
PW9931        IF  INP-POSPROC-T700 EQUAL 11
EG1216            PERFORM 2500-CONSISTIR-DAP
EG1216            IF  WRK-INCONS-DAP   EQUAL 'S'
EG1216                GO TO 12000-99-FIM
EG1216            END-IF
PW9931            GO  TO  20000-01-TRATA-RECEITA-BRUTA
PW9931        ELSE
              IF  INP-POSPROC-T700 EQUAL 4,5
                  GO  TO  12000-05-CONTINUA.

101020     IF PROGRAMA-ANT  EQUAL   'RUEC7000'
101020        IF  INP-POSPROC-T700 EQUAL 1,2
101020            PERFORM 1200-00-OBTEM-FORMA-PAGTO
101020            GO TO 12000-99-FIM
101020        END-IF
101020     END-IF.

      *********************************************************
      ***  VERIFICA POSICIONAMENTO DO RETORNO DA TELA RUECT700
      *********************************************************
       12000-00-VER-POSPROC-TELA.

           IF  INP-POSPROC-T700 EQUAL ZEROS
               GO  TO  12000-01-TRATA-DADOS-PROPON
           ELSE
           IF  INP-POSPROC-T700 EQUAL 1
               GO  TO  12000-02-RETORNO-RUEC7520
           ELSE
           IF  INP-POSPROC-T700 EQUAL 2
               GO  TO  12000-03-VALIDA-FICHA-CAD
           ELSE
           IF  INP-POSPROC-T700 EQUAL 3
               GO  TO  12000-04-RETORNO-RUEC7550
           ELSE
           IF  INP-POSPROC-T700 EQUAL 4
               GO  TO  12000-05-TRATA-ORIGEM-RECURSO
           ELSE
PW9931     IF  INP-POSPROC-T700 EQUAL 4,2
PW9931         IF  WRK-TRANSACAO    EQUAL 'RUEC6970'
PW9931*           XGO  TO  16010-00-RETORNO-RUEC6970
PRI499             PERFORM 16010-00-RETORNO-RUEC6970
PW9931         END-IF
PW9931         IF  WRK-TRANSACAO    EQUAL 'RUEC6980'
PW9931*           XGO  TO  16020-00-RETORNO-RUEC6980
PW9931             PERFORM 16020-00-RETORNO-RUEC6980
PW9931         END-IF
PW9931         IF  WRK-TRANSACAO    EQUAL 'RUEC6990'
PW9931*           XGO  TO  16030-00-RETORNO-RUEC6990
PRI499             PERFORM 16030-00-RETORNO-RUEC6990
PW9931         END-IF
PW9931     ELSE
PW993*        IF  INP-POSPROC-T700 EQUAL 4,3
PW993**          XGO TO   16040-00-CONTINUA
PRI499            PERFORM 16040-00-CONTINUA
PW993*        ELSE
PW9931     IF  INP-POSPROC-T700 EQUAL 4,4
PW9931         IF  WRK-TRANSACAO    EQUAL 'RUEC6140'
PW9931*           XGO  TO  17010-00-RETORNO-RUEC6140
PRI499             PERFORM 17010-00-RETORNO-RUEC6140
PRI499             GO TO 12000-99-FIM
PW9931         END-IF
PW9931         IF  WRK-TRANSACAO    EQUAL 'RUEC6150'
PW9931*           XGO  TO  17020-00-RETORNO-RUEC6150
PRI499             PERFORM 17020-00-RETORNO-RUEC6150
PRI499             GO TO 12000-99-FIM
PW9931         END-IF
PW9931         IF  WRK-TRANSACAO    EQUAL 'RUEC6160'
PW9931*           XGO  TO  17030-00-RETORNO-RUEC6160
PRI499             PERFORM 17030-00-RETORNO-RUEC6160
PRI499             GO TO 12000-99-FIM
PW9931         END-IF
PW9931         IF  WRK-TRANSACAO    EQUAL 'RUEC6170'
PW9931*           XGO  TO  17040-00-RETORNO-RUEC6170
PW9931             PERFORM 17040-00-RETORNO-RUEC6170
PRI499             GO TO 12000-99-FIM
PW9931         END-IF
PW9931         IF  WRK-TRANSACAO    EQUAL 'RUEC6180'
RI4991*           XGO  TO  17050-00-RETORNO-RUEC6180
PRI499             PERFORM 17050-00-RETORNO-RUEC6180
PRI499             GO TO 12000-99-FIM
PW9931         END-IF
PW9931         IF  WRK-TRANSACAO    EQUAL 'RUEC6190'
PW9931*           XGO  TO  17060-00-RETORNO-RUEC6190
PRI499             PERFORM 17060-00-RETORNO-RUEC6190
PRI499             GO TO 12000-99-FIM
PW9931         END-IF
PRI499         IF  WRK-TRANSACAO       EQUAL 'RUEC6210'
PRI499*           XGO  TO  17065-00-RETORNO-RUEC6210
PRI499             PERFORM 1766-00-RETORNO-RUEC6210
PRI499             GO TO 12000-05-CONTINUA
PRI499         END-IF
LMC            IF  WRK-TRANSACAO    EQUAL 'RUEC7750'
LMC               PERFORM 2397-00-RETORNO-RUEC7750
LMC            END-IF
PW9931     ELSE
           IF  INP-POSPROC-T700 EQUAL 5
               GO  TO  12000-06-TRATA-QTDE-CONTRATOS
           ELSE
           IF  INP-POSPROC-T700 EQUAL 6
               GO  TO  12000-07-TRATA-TAXA-JUROS
           ELSE
EG1216
EG1216     PERFORM 2500-CONSISTIR-DAP
EG1216     IF  WRK-INCONS-DAP      EQUAL 'S'
EG1216         GO TO 12000-99-FIM
EG1216     END-IF
EG1216
           IF  INP-POSPROC-T700 EQUAL 7
               IF  INP-FASE-T700 EQUAL '2'
                   GO  TO  12000-08-RETORNO-RUEC7530
               ELSE
                   GO  TO  12000-08-TRATA-DENOM-IMOVEL
           ELSE
           IF  INP-POSPROC-T700 EQUAL 8
               GO  TO  12000-09-TRATA-LAVOURA
           ELSE
PW9931     IF  INP-POSPROC-T700 EQUAL 8,5
PW9931         GO  TO  16500-00-RETORNO-RUEC6230
PW9931     ELSE
           IF  INP-POSPROC-T700 EQUAL 9
               GO  TO  12000-10-TRATA-BLOQUEIOS
           ELSE
           IF  INP-POSPROC-T700 EQUAL 10
               GO  TO  12000-11-TRATA-TIPO-PRODUTOR
           ELSE
PW9931     IF  INP-POSPROC-T700 EQUAL 11
PW9931         GO  TO  20000-01-TRATA-RECEITA-BRUTA
PW9931     ELSE
           IF  INP-POSPROC-T700 EQUAL 4,5
               GO  TO  12000-05-CONTINUA.


101020     IF  INP-POSPROC-T700 EQUAL 1,2
101020         GO  TO  12000-03-VALIDA-FICHA-CAD
101020     END-IF.

      *******************************
      ***  OBTEM DADOS DO PROPONENTE
      *******************************
       12000-01-TRATA-DADOS-PROPON.

           MOVE '1'                        TO  OUT-FASE-T700.
           MOVE  ZEROS                     TO  OUT-POSPROC-T700.

           IF  INP-AGENC-T700   NOT NUMERIC OR
               INP-AGENC-T700   EQUAL   ZEROS
               MOVE TAB-ATRIB(1)           TO   OUT-AGENC-ATTR-T700
               MOVE    WRK-M4              TO   OUT-MENSAG-T700
               GO TO 12000-99-FIM.

           IF  WRK-COD-DEPTO NOT EQUAL 4130 AND 4253 AND 4510
HEXA           MOVE    WRK-COD-DEPTO   TO          WRK-MESUX0-AGENCIA
HEXA           PERFORM 9990-10-CHAMA-ROTINA-FX-AG
HEXA           IF  WRK-EH-FX-AG
                   IF  WRK-COD-DEPTO NOT EQUAL INP-AGENC-T700
                       PERFORM 12006-ACESSAR-MIDD9000
                       IF  WRK-PERM-OK EQUAL 'S'
                           NEXT SENTENCE
                       ELSE
                           MOVE LNK-MENSAGEM   TO OUT-MENSAG-T700
                           MOVE TAB-ATRIB(1)   TO OUT-AGENC-ATTR-T700
                           GO TO 12000-99-FIM
                       END-IF
                   END-IF
               END-IF
           END-IF.

           IF  WRK-COD-DEPTO NOT EQUAL 4130 AND 4253 AND 4510
HEXA           MOVE    WRK-COD-DEPTO   TO          WRK-MESUX0-AGENCIA
HEXA           PERFORM 9990-10-CHAMA-ROTINA-FX-AG
HEXA           IF  NOT  WRK-EH-FX-AG
                   IF  WRK-COD-DEPTO NOT EQUAL INP-AGENC-T700
                       MOVE TAB-ATRIB(1)       TO OUT-AGENC-ATTR-T700
                       MOVE  WRK-M20           TO OUT-MENSAG-T700
                       GO TO 12000-99-FIM
                   END-IF
               END-IF
           END-IF.

           IF  INP-CONTA-T700   NOT NUMERIC OR
               INP-CONTA-T700   EQUAL   ZEROS
               MOVE TAB-ATRIB(1)           TO   OUT-CONTA-ATTR-T700
               MOVE    WRK-M4              TO   OUT-MENSAG-T700
               GO TO 12000-99-FIM.

           IF  AGENCIA NOT NUMERIC AND
               CONTA   NOT NUMERIC
               INITIALIZE AREA-RUEC7000
               INITIALIZE DADOS-IMOVEIS.


           IF  INP-CGC-CPF-T700-X  NOT EQUAL SPACES
               PERFORM 12060-VALIDA-CGC
               IF WRK-INCONSIS EQUAL '*'
                  MOVE TAB-ATRIB(1)       TO   OUT-CGCNUM-ATTR-T700
                                               OUT-CGCFIL-ATTR-T700
                                               OUT-CTRL-ATTR-T700
                  MOVE    WRK-M24         TO   OUT-MENSAG-T700
                  GO TO 12000-99-FIM
               ELSE
                   IF  WRK-CONTROLE  NOT EQUAL INP-CTRL-T700-X
                       MOVE TAB-ATRIB(1)  TO   OUT-CGCNUM-ATTR-T700
                                               OUT-CGCFIL-ATTR-T700
                                               OUT-CTRL-ATTR-T700
                       MOVE    WRK-M24    TO   OUT-MENSAG-T700
                       GO TO 12000-99-FIM
                   ELSE
                   IF  INP-CGCFIL-T700-X NOT EQUAL   SPACES AND
                       INP-CGCFIL-T700-X NOT EQUAL   ZEROS
                       MOVE INP-CGCNUM-T700    TO  NUMCGC
                       MOVE INP-CGCFIL-T700    TO  FILIAL
                       MOVE INP-CTRL-T700      TO  CTRL-CGC
                       MOVE 'J'                TO  TIPO-PESSOA
                   ELSE
                       MOVE INP-CGCNUM-T700    TO  NUMCPF
                       MOVE INP-CTRL-T700      TO  CTRL-CPF
                       MOVE 'F'                TO  TIPO-PESSOA.

           IF  INP-CGC-CPF-T700-X      EQUAL SPACES OR LOW-VALUES
               MOVE ZEROS                  TO  NUMCGC
               MOVE ZEROS                  TO  FILIAL
               MOVE ZEROS                  TO  CTRL-CGC
               MOVE ' '                    TO  TIPO-PESSOA.

201504****-IF  WRK-COD-DEPTO NOT EQUAL 4130 AND 4253 AND 4510
201504********-PERFORM 1000-VERIFICA-AGE-MIGRADA
201504****-END-IF
201504
201504****-IF WRK-INCONSIS EQUAL '*'
201504***********-GO TO 12000-99-FIM
201504****-END-IF

           IF  INP-NOME-T700           EQUAL SPACES OR LOW-VALUES
               MOVE SPACES                 TO  NOME
           ELSE
               MOVE INP-NOME-T700          TO  NOME.

           MOVE    'RUEC7000'              TO  PROGRAMA-ANT
           MOVE    INP-AGENC-T700          TO  AGENCIA
           MOVE    INP-CONTA-T700          TO  CONTA
           MOVE    IO-LTERM                TO  CTERM OF RUECB042.

           PERFORM 12015-DELETE-RUECB042.

           PERFORM 11400-MOVE-TELA-OUT-IN.

           PERFORM 12040-INSERT-RUECB042.

POL097     INITIALIZE  R31-AREA-TOTAL.
POL097     MOVE    1                       TO  R31-CSEQ-REG
POL097     MOVE    INP-DADOS-T700(1:300)   TO  R31-SALVA-RUEC7000
POL097     PERFORM 12031-UPDATE-RUECB048

POL097     INITIALIZE  R31-AREA-TOTAL.
POL097     MOVE    2                       TO  R31-CSEQ-REG
POL097     MOVE    INP-DADOS-T700(301:300) TO  R31-SALVA-RUEC7000
POL097     PERFORM 12031-UPDATE-RUECB048.

PW9931     INITIALIZE  R31-AREA-TOTAL.
PW9931     MOVE    3                       TO  R31-CSEQ-REG
LMC        MOVE    INP-DADOS-T700(601:322) TO  R31-SALVA-RUEC7000
PW9931     PERFORM 12031-UPDATE-RUECB048.

           MOVE    IO-LTERM            TO  COMU-COD-TERM.
           MOVE    'RUEC7000'          TO  COMU-TRANCODE.
           MOVE    +147                TO  COMU-LL.
           MOVE    COMU-AREA           TO  WRK-MENSAGEM.
           MOVE    'RUEC7510'          TO  WRK-TELA
           MOVE    WRK-CHNG            TO  WRK-FUNCAO

           GO TO 12000-99-FIM.

      ***************************
       12000-01-RETORNO-RUEC7510.

           MOVE    AGENCIA                 TO  OUT-AAGENC-T700
                                               OUT-AGENC-T700.
           MOVE    CONTA                   TO  OUT-ACONTA-T700
                                               OUT-CONTA-T700.
           MOVE    NOME                    TO  OUT-ANOME-T700
                                               OUT-NOME-T700.

           IF  TIPO-PESSOA EQUAL 'J'
               MOVE NUMCGC             TO  OUT-ACGCNUM-T700
                                           OUT-CGCNUM-T700
               MOVE FILIAL             TO  OUT-ACGCFIL-T700
                                           OUT-CGCFIL-T700
               MOVE CTRL-CGC           TO  OUT-ACTRL-T700
                                           OUT-CTRL-T700
           ELSE
               MOVE NUMCPF             TO  OUT-ACGCNUM-T700
                                           OUT-CGCNUM-T700
               MOVE ZEROS              TO  OUT-ACGCFIL-T700-X
                                           OUT-CGCFIL-T700-X
               MOVE CTRL-CPF           TO  OUT-ACTRL-T700
                                           OUT-CTRL-T700.


      **********************************************
      ***  OBTEM SITUACAO DO CLIENTE PERANTE O IRES
      **********************************************
       12000-02-TRATA-SITUACAO-CLIE.

           MOVE  1                         TO  OUT-POSPROC-T700.
           MOVE '1'                        TO  OUT-FASE-T700.

           MOVE    'RUEC7000'              TO  PROGRAMA-ANT.

           PERFORM 11400-MOVE-TELA-OUT-IN.

           MOVE 0002                       TO   ERR-LOCAL.
           PERFORM 12030-UPDATE-RUECB042.

           MOVE    IO-LTERM            TO  COMU-COD-TERM.
           MOVE    'RUEC7000'          TO  COMU-TRANCODE.
           MOVE    +147                TO  COMU-LL.
           MOVE    COMU-AREA           TO  WRK-MENSAGEM.
           MOVE    'RUEC7520'          TO  WRK-TELA
           MOVE    WRK-CHNG            TO  WRK-FUNCAO

           GO TO 12000-99-FIM.

      ***************************
       12000-02-RETORNO-RUEC7520.

           MOVE    SITUACAO                TO  OUT-SITUAC-T700.

           MOVE    MENS-SITUACAO           TO  OUT-MENSIT-T700.

           IF  SITUACAO EQUAL 'IMPEDIDO   '
               IF COD-IMPEDIMENTO NOT EQUAL 2 AND 3
                  MOVE SPACES              TO  WRK-INCONSIS
                  MOVE ZEROS               TO  CONSULTA-BLOQUEIO
                  MOVE '  TITULAR  IMPEDIDO '
                                           TO  M25-CAMPO
                  MOVE 'O TITULAR ESTA IMPEDIDO'  TO  M26-CAMPO-A
                  MOVE TAB-ATRIB(2)        TO  OUT-AGENC-ATTR-T700
                  MOVE 1                   TO  OUT-AUXDCR-T700
                  PERFORM 12210-VERIF-BLOQUEIO
                  IF WRK-INCONSIS EQUAL 'S'
                     GO TO 12000-99-FIM
                  ELSE
                     NEXT SENTENCE
               ELSE
                  MOVE '  TITULAR  IMPEDIDO ' TO M25-CAMPO
                  MOVE WRK-M25                TO OUT-MENSAG-T700
                  MOVE '3'                    TO OUT-FASE-T700
                  GO TO 12000-99-FIM
           ELSE
               NEXT SENTENCE.

101020     PERFORM 1200-00-OBTEM-FORMA-PAGTO.
101020     GO TO 12000-99-FIM.

201504****XPERFORM 1100-VERIFICA-FICHA-CRUR
201504
201504****XIF WRK-INCONSIS EQUAL '*'
201504***********XGO TO 12000-99-FIM
201504****XEND-IF.

      ***************************
       12000-02-RETORNO-PF11.

      *******************************************
      ***  VALIDA FICHA CADASTRAL PERANTE O FICA
      *******************************************
       12000-03-VALIDA-FICHA-CAD.

           MOVE  2                         TO  OUT-POSPROC-T700.
           MOVE '1'                        TO  OUT-FASE-T700.

           MOVE    'RUEC7000'              TO  PROGRAMA-ANT.
ALDO  *---------------------------------------------------------------*
ALDO  *    MOVE    ZEROS                   TO  COD-APLICACAO OF       *
ALDO  *                                        AREA-RUEC7000.         *
ALDO  *---------------------------------------------------------------*

           MOVE 0003                       TO   ERR-LOCAL.

           PERFORM 12030-UPDATE-RUECB042.

           MOVE    IO-LTERM            TO  LK-CODIGO-TERMINAL.
           MOVE    SPACES              TO  LK-STATUS-RETORNO
                                           LK-MENSAGEM.

POL097     IF  (COD-APLICACAO OF AREA-RUEC7000 NOT EQUAL 1 AND 2)  OR
POL097         (COD-FINALIDADE  GREATER  24  AND  LESS  37)        OR
MF0112         (COD-FINALIDADE  EQUAL    57 OR 59)                 OR
RO0520         (COD-FINALIDADE  EQUAL    76 OR 77)
POL097         GO  TO  12000-03-CONTINUA
POL097     END-IF.

BR0718     PERFORM 7000-CHECAR-EXIGE-IMOVEL
BR0718     IF  WRK-EXIGE-IMOVEL        EQUAL 'N'
BR0718         GO  TO  12000-03-CONTINUA
BR0718     END-IF.
BR0718
POL097     INITIALIZE  R20-AREA-R7718
POL097     MOVE 'RUEC7000'             TO  R20-PGMCHMD-R7718
POL097     MOVE CCLUB-RUEC             TO  R20-CCLUB-R7718.

POL097     CALL    'POOL0081'      USING   WRK-RUEC7718
POL097                                     R20-AREA-R7718
POL097                                     IO-PCB
POL097                                     ALT-PCB

POL097     IF  R20-RET-CODE-R7718      EQUAL   99
POL097         MOVE R20-SQLCA-R7718    TO SQLCA
POL097         MOVE R20-ERRO-AREA-R7718 TO ERRO-AREA
POL097         MOVE 'DB2'               TO ERR-TIPO-ACESSO
POL097         MOVE SPACES              TO ERR-SEGM
POL097         MOVE ERR-PGM             TO ERR-MODULO
POL097         PERFORM  99999-ROTINA-ERRO
           ELSE
POL097     IF  R20-RET-CODE-R7718      EQUAL   01
POL097         MOVE WRK-M34A            TO OUT-MENSAG-T700
               GO TO 12000-99-FIM
POL097     ELSE
POL097     IF  R20-RET-CODE-R7718       EQUAL   06
POL097         MOVE WRK-M38             TO OUT-MENSAG-T700
POL097         GO TO 12000-99-FIM
POL097     ELSE
POL097     IF  R20-RET-CODE-R7718  NOT  EQUAL   05
POL097         MOVE WRK-M37             TO OUT-MENSAG-T700
POL097         GO TO 12000-99-FIM.

201504****XPERFORM 1100-VERIFICA-FICHA-CRUR
201504
201504****XIF WRK-INCONSIS EQUAL '*'
201504***********XGO TO 12000-99-FIM
201504*****END-IF

POL097     INITIALIZE  R22-AREA-R7722.
POL097     MOVE 'RUEC7000'             TO  R22-PGMCHMD-R7722
POL097     MOVE 'P'                    TO  R22-FUNCAO-R7722.
POL097     MOVE 11                     TO  R22-QPESQ-R7722.
POL097     MOVE ZEROS                  TO  R22-QDESPR-R7722.
POL097     MOVE CCLUB-RUEC             TO  R22-CCLUB-R7722.
POL097     MOVE TIPO-PESSOA            TO  R22-TPSSOA-ENV-R7722.
POL097     MOVE ZEROS                  TO  R22-SEQPATRM-ENV-R7722.

POL097     CALL    'POOL0081'      USING   WRK-RUEC7722
POL097                                     R22-AREA-R7722
POL097                                     IO-PCB
POL097                                     ALT-PCB

POL097     IF  R22-RET-CODE-R7722       EQUAL   99
POL097         MOVE R22-SQLCA-R7722     TO SQLCA
POL097         MOVE R22-ERRO-AREA-R7722 TO ERRO-AREA
POL097         MOVE 'DB2'               TO ERR-TIPO-ACESSO
POL097         MOVE SPACES              TO ERR-SEGM
POL097         MOVE ERR-PGM             TO ERR-MODULO
POL097         PERFORM  99999-ROTINA-ERRO
           ELSE
POL097     IF  R22-RET-CODE-R7722       EQUAL   01
POL097         MOVE WRK-M331            TO OUT-MENSAG-T700
               GO TO 12000-99-FIM
POL097     ELSE
POL097     IF  R22-RET-CODE-R7722       EQUAL   05
POL097         MOVE R22-MENS-R7722      TO OUT-MENSAG-T700
POL097         GO TO 12000-99-FIM.

201504****XPERFORM 1100-VERIFICA-FICHA-CRUR
201504
201504****XIF WRK-INCONSIS EQUAL '*'
201504***********XGO TO 12000-99-FIM
201504*****END-IF.


POL097 12000-03-CONTINUA.

           PERFORM 11200-SELECT-RUECB042.

      ****************************************************
      ***  OBTEM PRODUTO/FINALIDADE/UNID.FINANC/UNID.PROD
      ****************************************************
       12000-04-TRATA-PROD-FINALIDADE.

           MOVE '1'                        TO  OUT-FASE-T700.

           IF INP-PFK-T700   EQUAL  '9'
              NEXT SENTENCE
           ELSE
              IF PRODUTO EQUAL SPACES OR LOW-VALUES
                 MOVE  3                   TO  OUT-POSPROC-T700
              ELSE
                 IF PRODUTO NOT EQUAL OUT-ANPROD1-T700
                    MOVE  3                TO  OUT-POSPROC-T700
                 ELSE
                    IF PRODUTO EQUAL OUT-NPROD1-T700
                       GO TO 12000-04-RETORNO-RUEC7550.

           MOVE  INP-NPROD1-T700           TO  PRODUTO

           MOVE    'RUEC7000'              TO  PROGRAMA-ANT.

           PERFORM 11400-MOVE-TELA-OUT-IN.

           MOVE 0004                       TO   ERR-LOCAL.
           PERFORM 12030-UPDATE-RUECB042.

           MOVE    IO-LTERM            TO  COMU-COD-TERM.
           MOVE    'RUEC7000'          TO  COMU-TRANCODE.
           MOVE    +147                TO  COMU-LL.
           MOVE    COMU-AREA           TO  WRK-MENSAGEM.
           MOVE    'RUEC7550'          TO  WRK-TELA
           MOVE    WRK-CHNG            TO  WRK-FUNCAO

           GO TO 12000-99-FIM.


      ***************************
       12000-04-RETORNO-RUEC7550.
      ***************************

           IF  COMU-STA-RET        EQUAL   'BL'
               MOVE    'S'                 TO  FLAG-GRAVA-INCONS
               MOVE     6                  TO  INP-AUXDCR-T700
               PERFORM 12200-CARREGA-RESP-APROV.

           MOVE    PRODUTO                 TO  OUT-ANPROD1-T700
                                               OUT-NPROD1-T700.

           MOVE    '-'                     TO  OUT-HIFEN-T700.
           MOVE    FINALIDADE              TO  OUT-NPROD2-T700.


           IF FLAG-PROD-PROVAVEL EQUAL 0
              MOVE 'NAO HA PROD. ESPERADA' TO OUT-LITERAL-T700
201503        MOVE 'PROD.ESP. TOTAL(TON):' TO WRK-OUT-LPRDTTL-T700
              MOVE ZEROS   TO   OUT-CAMPO-T700
                                INP-CAMPO-T700.

           IF FLAG-PROD-PROVAVEL EQUAL 1
              IF (INP-CAMPO-T700 NOT NUMERIC) OR
                 (INP-CAMPO-T700 EQUAL ZEROS)
201503           MOVE 'PROD.ESPER. (TON/HA):' TO OUT-LITERAL-T700
201503           MOVE 'PROD.ESP. TOTAL(TON):' TO WRK-OUT-LPRDTTL-T700
                 MOVE TAB-ATRIB(9)            TO  OUT-CAMPO-ATTR-T700
                 MOVE '1'                     TO  OUT-ATTRCA-T700
              ELSE
                 MOVE INP-CAMPO-T700          TO  PROD-PROVAVEL
                                                  OUT-CAMPO-T700
                                                  OUT-ACAMPO-T700.

           IF FLAG-PROD-PROVAVEL EQUAL 2
              IF (INP-CAMPO-T700 NOT NUMERIC) OR
                 (INP-CAMPO-T700 EQUAL ZEROS)
                 MOVE 'PROD. ESPERADA TOTAL:' TO OUT-LITERAL-T700
201503           MOVE 'PROD.ESP. TOTAL(TON):' TO WRK-OUT-LPRDTTL-T700
                 MOVE TAB-ATRIB(9)            TO  OUT-CAMPO-ATTR-T700
                 MOVE '1'                     TO  OUT-ATTRCA-T700
              ELSE
                 MOVE INP-CAMPO-T700          TO  PROD-PROVAVEL
                                                  OUT-CAMPO-T700
                                                  OUT-ACAMPO-T700.

           MOVE 0006                      TO   ERR-LOCAL
           PERFORM 12030-UPDATE-RUECB042.


      *****************************
      ***  OBTEM ORIGEM DE RECURSO
      *****************************
       12000-05-TRATA-ORIGEM-RECURSO.

           MOVE '1'                        TO  OUT-FASE-T700.

           IF INP-PFK-T700   EQUAL  '9'
              MOVE ZEROS                   TO  COD-ORIG-REC
              GO TO 12000-05-PASSA-CONTROLE
           ELSE
              IF COD-ORIG-REC NOT EQUAL OUT-AORIGCD-T700
                 MOVE  4                   TO  OUT-POSPROC-T700.

           MOVE ZEROS                      TO  COD-ORIG-REC.

           IF INP-ORIGCD-T700-X NOT EQUAL SPACES
              IF (INP-ORIGCD-T700 NOT NUMERIC) OR
                 (INP-ORIGCD-T700 EQUAL ZEROS)
                 MOVE TAB-ATRIB(1)            TO  OUT-ORIGCD-ATTR-T700
                 MOVE WRK-M18                 TO  OUT-MENSAG-T700
                 GO TO 12000-99-FIM
              ELSE
                 MOVE  INP-ORIGCD-T700        TO  COD-ORIG-REC
                 PERFORM 12070-SELECT-RURCV000
                 IF SQLCODE EQUAL ZEROS
                    MOVE  CORIGE-REC OF RURCV000 TO  COD-ORIG-REC
                    MOVE  IORIGE-REC OF RURCV000 TO  ORIG-REC
                    IF CDISPN-ORIGE-REC NOT EQUAL 'S' AND 'B'
                       MOVE SPACES              TO  WRK-INCONSIS
                       MOVE 120                 TO  CONSULTA-BLOQUEIO
                       MOVE 'ORIGEM REC. INATIVA ' TO M25-CAMPO
                       MOVE 'ORIGEM RECURSO INATIVA '  TO  M26-CAMPO-A
                       MOVE TAB-ATRIB(2)        TO  OUT-ORIGCD-ATTR-T700
                       MOVE 7                   TO  OUT-AUXDCR-T700
                       PERFORM 12210-VERIF-BLOQUEIO
                       IF WRK-INCONSIS EQUAL 'S'
                          GO TO 12000-99-FIM.

           IF INP-ATIVCD-T700-X NOT EQUAL SPACES
              IF (INP-ATIVCD-T700 NOT NUMERIC) OR
                 (INP-ATIVCD-T700 EQUAL ZEROS)
                 MOVE TAB-ATRIB(1)            TO  OUT-ATIVCD-ATTR-T700
                 MOVE WRK-M18                 TO  OUT-MENSAG-T700
                 GO TO 12000-99-FIM.

           IF (INP-ORIGCD-T700-X NOT EQUAL SPACES) AND
              (INP-ATIVCD-T700-X NOT EQUAL SPACES)
               MOVE INP-ATIVCD-T700         TO  COD-ATIVIDADE
               PERFORM 12080-SELECT-RURCV040
               IF  SQLCODE EQUAL +100
                   MOVE TAB-ATRIB(1)        TO  OUT-ORIGCD-ATTR-T700
                   MOVE COD-FINALIDADE      TO  M7-CAMPO1
                   MOVE INP-ORIGCD-T700-X   TO  M7-CAMPO2
                   MOVE INP-ATIVCD-T700-X   TO  M7-CAMPO3
                   MOVE WRK-M7              TO  OUT-MENSAG-T700
                   GO TO 12000-99-FIM
               ELSE
                   IF  SQLCODE EQUAL ZEROS
DK0510                 GO TO 19000-REINICIA-EMPREENDIMENTO.
DK0510*                GO TO 12000-05-CONTINUA.

      * -811 - ACESSAR RUEC7560 PARA SELECAO DA ATIVIDADE/ORIGEM/FINAL.
       12000-05-PASSA-CONTROLE.

           MOVE    'RUEC7000'              TO  PROGRAMA-ANT.

           PERFORM 11400-MOVE-TELA-OUT-IN.

           MOVE 0007                      TO   ERR-LOCAL
           PERFORM 12030-UPDATE-RUECB042.

           MOVE    IO-LTERM            TO  COMU-COD-TERM.
           MOVE    'RUEC7000'          TO  COMU-TRANCODE.
           MOVE    +147                TO  COMU-LL.
           MOVE    COMU-AREA           TO  WRK-MENSAGEM.
           MOVE    'RUEC7560'          TO  WRK-TELA.
           MOVE    WRK-CHNG            TO  WRK-FUNCAO

           GO TO 12000-99-FIM.

      ***************************
       12000-05-RETORNO-RUEC7560.

           IF  COMU-STA-RET        EQUAL   'BL'
               MOVE    'S'                 TO  FLAG-GRAVA-INCONS
               MOVE     7                  TO  INP-AUXDCR-T700
               PERFORM 12200-CARREGA-RESP-APROV.

201504     MOVE SPACES                TO WRK-INCONSIS
201504
201504     IF  COD-ORIG-REC           GREATER 59 AND
201504         COD-ORIG-REC           LESS  81
201504         NEXT SENTENCE
201504     ELSE
201504         IF  WRK-COD-DEPTO NOT EQUAL 4130 AND 4253 AND 4510
201504             PERFORM 1000-VERIFICA-AGE-MIGRADA
201504         END-IF
201504     END-IF.
201504
201504     IF WRK-INCONSIS EQUAL '*'
201504        GO TO 12000-99-FIM
201504     END-IF.


           MOVE  COD-ORIG-REC         TO  OUT-ORIGCD-T700
                                          OUT-AORIGCD-T700.

           MOVE  ORIG-REC             TO  OUT-ORIGDS-T700.

           MOVE  COD-ATIVIDADE        TO  OUT-ATIVCD-T700
                                          OUT-AATIVCD-T700.

           PERFORM 12030-UPDATE-RUECB042.

PW9931     GO TO   16000-00-TRATA-EMPREENDIMENTO.

      *-----------------------------------------------------------------
       12000-05-CONTINUA.
      *-----------------------------------------------------------------

           MOVE  4,5                  TO  OUT-POSPROC-T700.

           PERFORM 12085-SELECT-RURCV004.

           IF SQLCODE EQUAL +100
              MOVE  COD-ATIVIDADE     TO  M28-CAMPO
              MOVE  WRK-M28           TO  OUT-MENSAG-T700
              GO TO 12000-99-FIM.

           MOVE  IATVDD-RURAL OF RURCV004 TO OUT-ATIVDS-T700
                                             ATIVIDADE.

PW9931     PERFORM 15020-00-TRATA-NOVO-RECOR THRU 15020-99-FIM.

PW9931     PERFORM 15030-00-VALIDA-RECOR THRU 15030-99-FIM.

PW9931     IF FLAG-AREA-QTDE EQUAL 0
PW9931        MOVE TAB-ATRIB(4)        TO  OUT-NPROD1-ATTR-T700
PW9931        MOVE WRK-M3              TO  OUT-MENSAG-T700
PW9931        GO TO 12000-99-FIM
PW9931     END-IF.

           IF FLAG-AREA-QTDE EQUAL 1
              IF INP-QTDEF-T700 NOT NUMERIC OR
                 INP-QTDEF-T700 EQUAL ZEROS
                 MOVE TAB-ATRIB(4)        TO  OUT-QTDEF-ATTR-T700
                 MOVE WRK-M17             TO  OUT-MENSAG-T700
                 GO TO 12000-99-FIM
              ELSE
BR1218           MOVE INP-QTDEF-T700      TO  QTDE-FINANC
BR1218           PERFORM 7100-CHECAR-EXIGE-AREA
BR1218           IF WRK-EXIGE-AREA        EQUAL 'S'
BR1218              IF (INP-AREAF-T700-X  NOT NUMERIC
BR1218              OR  INP-AREAF-T700-X  EQUAL ZEROS )
BR1218                 MOVE TAB-ATRIB(4)  TO OUT-AREAF-ATTR-T700
BR1218                 MOVE WRK-M17       TO OUT-MENSAG-T700
BR1218                 GO TO 12000-99-FIM
BR1218              ELSE
BR1218                 MOVE INP-AREAF-T700
BR1218                                    TO  AREA-FINANC
BR1218              END-IF
BR1218           ELSE
BR1218              MOVE ZEROS            TO  AREA-FINANC
BR1218              MOVE SPACES           TO  OUT-AAREAF-T700-X
BR1218                                        OUT-AREAF-T700-X
BR1218           END-IF
BR1218        END-IF
BR1218     END-IF.

           IF (FLAG-AREA-QTDE EQUAL 2)  OR
              ((FINALIDADE    EQUAL 12) AND
               (COD-PRODUTO   EQUAL 89 OR 92))
              IF INP-AREAF-T700-X NOT NUMERIC OR
                 INP-AREAF-T700-X EQUAL ZEROS
                 MOVE TAB-ATRIB(4)        TO  OUT-AREAF-ATTR-T700
                 MOVE WRK-M17             TO  OUT-MENSAG-T700
                 GO TO 12000-99-FIM
              ELSE
                 MOVE INP-AREAF-T700      TO  AREA-FINANC
                 MOVE ZEROS               TO  QTDE-FINANC
                 MOVE SPACES              TO  OUT-AQTDEF-T700-X
                                              OUT-QTDEF-T700-X.

           IF FLAG-AREA-QTDE EQUAL 3
                 MOVE ZEROS               TO  QTDE-FINANC
                                              AREA-FINANC
                 MOVE SPACES              TO  OUT-AAREAF-T700-X
                                              OUT-AREAF-T700-X
                                              OUT-AQTDEF-T700-X
                                              OUT-QTDEF-T700-X.

           PERFORM 12030-UPDATE-RUECB042.

PW0410*   XIF FLAG-PROD-PROVAVEL EQUAL 0
PW0410*      XMOVE TAB-ATRIB(4)        TO  OUT-CAMPO-ATTR-T700
PW0410*      XMOVE WRK-M3              TO  OUT-MENSAG-T700
PW0410*      XGO TO 12000-99-FIM.

           IF FLAG-PROD-PROVAVEL EQUAL 1
              IF (INP-CAMPO-T700-X NOT NUMERIC) OR
                 (INP-CAMPO-T700-X EQUAL ZEROS)
201503           MOVE 'PROD.ESPER. (TON/HA):' TO OUT-LITERAL-T700
201503           MOVE 'PROD.ESP. TOTAL(TON):' TO  WRK-OUT-LPRDTTL-T700
                 MOVE TAB-ATRIB(4)            TO  OUT-CAMPO-ATTR-T700
                 MOVE WRK-M17                 TO  OUT-MENSAG-T700
                 MOVE '1'                     TO  OUT-ATTRCA-T700
                 GO TO 12000-99-FIM
              ELSE
                 MOVE INP-CAMPO-T700          TO  PROD-PROVAVEL
                                                  OUT-CAMPO-T700
                                                  OUT-ACAMPO-T700.

           IF FLAG-PROD-PROVAVEL EQUAL 2
              IF (INP-CAMPO-T700-X NOT NUMERIC) OR
                 (INP-CAMPO-T700-X EQUAL ZEROS)
                 MOVE 'PROD. ESPERADA TOTAL:' TO  OUT-LITERAL-T700
201503           MOVE 'PROD.ESP. TOTAL(TON):' TO  WRK-OUT-LPRDTTL-T700
                 MOVE TAB-ATRIB(4)            TO  OUT-CAMPO-ATTR-T700
                 MOVE WRK-M17                 TO  OUT-MENSAG-T700
                 MOVE '1'                     TO  OUT-ATTRCA-T700
                 GO TO 12000-99-FIM
              ELSE
                 MOVE INP-CAMPO-T700          TO  PROD-PROVAVEL
                                                  OUT-CAMPO-T700
                                                  OUT-ACAMPO-T700.

PW9931     IF WRK-INP-TPREC-T700 EQUAL 1
PW9931        MOVE  TAB-ATRIB(8)           TO WRK-OUT-RECBR-ATTR-T700
PW9931     ELSE
PW9931        MOVE  TAB-ATRIB(3)           TO WRK-OUT-RECBR-ATTR-T700
PW9931     END-IF.

MI1808     IF (COD-FINALIDADE OF AREA-RUEC7000 > 24 AND < 37)
RO0520     OR (COD-FINALIDADE OF AREA-RUEC7000 EQUAL 76 OR 77)
RO0520        EVALUATE TRUE
MI1808          WHEN INP-GRUPO-T700 EQUAL SPACES
MI1808               MOVE TAB-ATRIB(9)        TO  OUT-PORTE-ATTR-T700
MI1808               MOVE TAB-ATRIB(7)        TO  OUT-GRUPO-ATTR-T700
MI1808               MOVE WRK-M30             TO  OUT-MENSAG-T700
MI1808               MOVE 'COOPER.: GRUPO:'   TO  OUT-LITER2-T700
MI1808               MOVE 'PORTE:'            TO  OUT-LITER3-T700
MI1808               GO TO 12000-99-FIM

MI1808          WHEN INP-GRUPO-T700 NOT EQUAL '1' AND '2'
MI1808               MOVE TAB-ATRIB(4)        TO  OUT-GRUPO-ATTR-T700
MI1808               MOVE TAB-ATRIB(4)        TO  OUT-PORTE-ATTR-T700
MI1808               MOVE WRK-M30             TO  OUT-MENSAG-T700
MI1808               MOVE 'COOPER.: GRUPO:'   TO  OUT-LITER2-T700
MI1808               MOVE 'PORTE:'            TO  OUT-LITER3-T700
MI1808               GO TO 12000-99-FIM

MI1808          WHEN INP-PORTE-T700 EQUAL SPACES OR
MI1808               INP-PORTE-T700 NOT EQUAL 'PEQUENO' AND 'GRANDE'
MI1808               MOVE TAB-ATRIB(9)        TO  OUT-GRUPO-ATTR-T700
MI1808               MOVE TAB-ATRIB(4)        TO  OUT-PORTE-ATTR-T700
MI1808               MOVE WRK-M31             TO  OUT-MENSAG-T700
MI1808               MOVE 'COOPER.: GRUPO:'   TO  OUT-LITER2-T700
MI1808               MOVE 'PORTE:'            TO  OUT-LITER3-T700
MI1808               GO TO 12000-99-FIM
MI1808          WHEN OTHER
MI1008               PERFORM  120031-CONSISTE-GRUPO-PORTE
MI1808               MOVE 'COOPER.: GRUPO:'     TO  OUT-LITER2-T700
MI1808               MOVE 'PORTE:'              TO  OUT-LITER3-T700
MI1008               MOVE  INP-PORTE-T700       TO  OUT-PORTE-T700
MI1008               MOVE  INP-GRUPO-T700       TO  OUT-GRUPO-T700
MI1008               IF SQLCODE                 EQUAL   +100
MI1808                  MOVE TAB-ATRIB(4)       TO OUT-GRUPO-ATTR-T700
MI1808                  MOVE TAB-ATRIB(4)       TO OUT-PORTE-ATTR-T700
MI1008                  MOVE INP-PORTE-T700     TO WRK-M32-PORTE
MI1008                  MOVE INP-GRUPO-T700     TO WRK-M32-GRUPO
MI1008                  MOVE WRK-M32            TO  OUT-MENSAG-T700
MI1808                  GO TO 12000-99-FIM
MI1008               ELSE
MI1008                  MOVE INP-PORTE-T700     TO  OUT-APORTE-T700
MI1008                  MOVE INP-GRUPO-T700     TO  OUT-AGRUPO-T700
MI1008                  MOVE CTPO-PROTR-RURAL   OF RURCV022
MI1008                                          TO     COD-TIPO-PRODUTOR
MI1008                  PERFORM 12030-UPDATE-RUECB042
                     END-IF
RO0520        END-EVALUATE
RO0520     END-IF

           IF FLAG-AREA-QTDE EQUAL 2
              IF INP-AREAF-T700 GREATER 1000
                 MOVE INP-AREAF-T700      TO  AREA-FINANC
                 MOVE ZEROS               TO  QTDE-FINANC
                 MOVE SPACES              TO  OUT-AQTDEF-T700-X
                                              OUT-QTDEF-T700-X
                 MOVE TAB-ATRIB(4)        TO  OUT-AREAF-ATTR-T700
                 MOVE WRK-M8              TO  OUT-MENSAG-T700
                 MOVE 0005                TO   ERR-LOCAL
                 PERFORM 12030-UPDATE-RUECB042
                 MOVE  5                  TO   OUT-POSPROC-T700
                 GO TO 12000-99-FIM.

      *******************************************************
      ***  OBTEM QTDE CONTR. POR ORIGEM RECURSO POR MUTUARIO
      *******************************************************
       12000-06-TRATA-QTDE-CONTRATOS.

           MOVE  5                         TO  OUT-POSPROC-T700.
           MOVE '1'                        TO  OUT-FASE-T700.

           MOVE    'RUEC7000'              TO  PROGRAMA-ANT.

           MOVE 0008                      TO   ERR-LOCAL
           PERFORM 12030-UPDATE-RUECB042.

           MOVE    IO-LTERM            TO  LK-CODIGO-TERMINAL.
           MOVE    SPACES              TO  LK-STATUS-RETORNO
                                           LK-MENSAGEM.

           CALL    'POOL0081'      USING   WRK-RUEC7704
                                           IO-PCB
                                           ALT-PCB
                                           AREA-NAVEGA.


           IF  LK-STATUS-RETORNO   EQUAL   'DB'
               GOBACK
           ELSE
           IF  LK-STATUS-RETORNO   EQUAL   'ER'
               MOVE TAB-ATRIB(2)      TO   OUT-ORIGCD-ATTR-T700
               MOVE LK-MENSAGEM       TO   OUT-MENSAG-T700
               GO TO 12000-99-FIM.

           PERFORM 11200-SELECT-RUECB042.

           IF INP-ORIGCD-T700-X EQUAL SPACES OR LOW-VALUES OR ZEROS
              GO   TO  12000-05-TRATA-ORIGEM-RECURSO.

           PERFORM 12070-SELECT-RURCV000.

           IF  QTDE-CONTR-ORIGEM GREATER  QCONTR-ORIGE-REC OF RURCV000
               MOVE SPACES              TO  WRK-INCONSIS
               MOVE ZEROS               TO  CONSULTA-BLOQUEIO
               MOVE 'QTDE. CONTR. EXCEDEU' TO M25-CAMPO
               MOVE 'QTDE. CONTRATO EXCEDIDA'  TO  M26-CAMPO-A
               MOVE TAB-ATRIB(2)        TO  OUT-ORIGCD-ATTR-T700
               MOVE 2                   TO  OUT-AUXDCR-T700
               PERFORM 12210-VERIF-BLOQUEIO
               IF WRK-INCONSIS EQUAL 'S'
                  GO TO 12000-99-FIM.

      ***********************
       12000-06-RETORNO-PF11.


      ***************************************
      ***  VERIFICA EXISTENCIA TAXA DE JUROS
      ***************************************
       12000-07-TRATA-TAXA-JUROS.

           MOVE  6                         TO  OUT-POSPROC-T700.
           MOVE '1'                        TO  OUT-FASE-T700.

BR0718     PERFORM 7000-CHECAR-EXIGE-IMOVEL
BR0718
BR0718     IF  WRK-EXIGE-IMOVEL        EQUAL 'N'
BR0718         GO TO 12000-08-NAO-EXIGE-IMOVEL
BR0718     END-IF.
BR0718
MAG        IF  (COD-APLICACAO OF AREA-RUEC7000 NOT EQUAL 1 AND 2)  OR
MAGA           (COD-FINALIDADE  GREATER  24  AND  LESS  37) OR
MF0112         (COD-FINALIDADE  EQUAL    57 OR 59 )
ALDO            NEXT SENTENCE
ALDO       ELSE
ALDO            PERFORM 12030-UPDATE-RUECB042
POL097          INITIALIZE R20-AREA-R7718
POL097          MOVE 'RUEC7000'             TO  R20-PGMCHMD-R7718
POL097          MOVE CCLUB-RUEC             TO  R20-CCLUB-R7718
POL097          CALL    'POOL0081'      USING   WRK-RUEC7718
POL097                                          R20-AREA-R7718
POL097                                          IO-PCB
POL097                                          ALT-PCB
POL097          IF  R20-RET-CODE-R7718      EQUAL   99
POL097              MOVE R20-SQLCA-R7718    TO SQLCA
POL097              MOVE R20-ERRO-AREA-R7718 TO ERRO-AREA
POL097              MOVE 'DB2'               TO ERR-TIPO-ACESSO
POL097              MOVE SPACES              TO ERR-SEGM
POL097              MOVE ERR-PGM             TO ERR-MODULO
POL097              PERFORM  99999-ROTINA-ERRO
POL097          ELSE
POL097          IF  R20-RET-CODE-R7718      EQUAL   01
POL097              MOVE WRK-M34A            TO OUT-MENSAG-T700
POL097              GO TO 12000-99-FIM
POL097          ELSE
POL097          IF  R20-RET-CODE-R7718       EQUAL   06
POL097              MOVE WRK-M38             TO OUT-MENSAG-T700
POL097              GO TO 12000-99-FIM
POL097          ELSE
POL097          IF  R20-RET-CODE-R7718  NOT  EQUAL   05
POL097              MOVE WRK-M37             TO OUT-MENSAG-T700
POL097              GO TO 12000-99-FIM
POL097          ELSE
POL097          INITIALIZE  R22-AREA-R7722
POL097          MOVE 'RUEC7000'             TO  R22-PGMCHMD-R7722
POL097          MOVE 'P'                    TO  R22-FUNCAO-R7722
POL097          MOVE 11                     TO  R22-QPESQ-R7722
POL097          MOVE ZEROS                  TO  R22-QDESPR-R7722
POL097          MOVE CCLUB-RUEC             TO  R22-CCLUB-R7722
POL097          MOVE TIPO-PESSOA            TO  R22-TPSSOA-ENV-R7722
POL097          MOVE ZEROS                  TO  R22-SEQPATRM-ENV-R7722
POL097          CALL    'POOL0081'      USING   WRK-RUEC7722
POL097                                          R22-AREA-R7722
POL097                                          IO-PCB
POL097                                          ALT-PCB
POL097          IF  R22-RET-CODE-R7722       EQUAL   99
POL097              MOVE R22-SQLCA-R7722     TO SQLCA
POL097              MOVE R22-ERRO-AREA-R7722 TO ERRO-AREA
POL097              MOVE 'DB2'               TO ERR-TIPO-ACESSO
POL097              MOVE SPACES              TO ERR-SEGM
POL097              MOVE ERR-PGM             TO ERR-MODULO
POL097              PERFORM  99999-ROTINA-ERRO
POL097          ELSE
POL097          IF  R22-RET-CODE-R7722       EQUAL   01
POL097              MOVE WRK-M332            TO OUT-MENSAG-T700
POL097              GO TO 12000-99-FIM
POL097          ELSE
POL097          IF  R22-RET-CODE-R7722       EQUAL   05
POL097              MOVE R22-MENS-R7722      TO OUT-MENSAG-T700
ALDO                   GO TO 12000-99-FIM
ALDO               ELSE
ALDO                   PERFORM 11200-SELECT-RUECB042.

RE1117***** PROCESSO INIBIDO DEVIDO AO PRONAMP
RE1117****-IF (COD-APLICACAO OF AREA-RUEC7000 NOT EQUAL 1 AND 2)  OR
RE1117****   -(FLAG-PROD-PROVAVEL                 EQUAL 0)
RE1117****   -IF COD-ATIVIDADE OF AREA-RUEC7000   EQUAL 1  OR 2
RE1117****      -MOVE   4                         TO COD-TIPO-PRODUTOR
RE1117****   -ELSE
RE1117****      -MOVE   8                         TO COD-TIPO-PRODUTOR.
RE1117
RE1117****-IF  COD-FINALIDADE  GREATER  24  AND  LESS  37
RE1117****-   XMOVE  7  TO  COD-TIPO-PRODUTOR.

RE1117****-IF  COD-FINALIDADE  EQUAL    57 OR 59
RE1117****    -MOVE  4  TO  COD-TIPO-PRODUTOR
RE1117****-END-IF.

PW9931* *** SE FINALIDADE 'COOPERATIVA REPASSE' MOVER 2 PARA
PW9931* *** TIPO DE CEDULA, SENAO MOVER 1-AVULSA

BR0718*-------------------------*
BR0718 12000-08-NAO-EXIGE-IMOVEL.
BR0718*-------------------------*
BR0718
PW9931     PERFORM 21000-SELECT-PRODUTO
MF0615     IF  CPRODT-BACEN OF RURCB006 EQUAL '07440' OR '08000'
PW9931         MOVE   2  TO  COD-TIPO-CEDULA
PW9931     ELSE
PW9931         MOVE   1  TO  COD-TIPO-CEDULA
PW9931     END-IF.

ALDO       PERFORM 12030-UPDATE-RUECB042.

           PERFORM 12090-SELECT-RURCB051.

           IF  SQLCODE EQUAL +100
               MOVE TAB-ATRIB(1)        TO  OUT-ORIGCD-ATTR-T700
               MOVE WRK-M5              TO  OUT-MENSAG-T700
               GO TO 12000-99-FIM.


      *************************************************
      ***  OBTEM DENOMINACAO DO IMOVEL NO SISTEMA FICA
      *************************************************
       12000-08-TRATA-DENOM-IMOVEL.

CAR        IF  WRK-INP-TPOBEN-T700-X   EQUAL SPACES OR LOW-VALUES
CAR            PERFORM 2399-TRATA-BENEFICIARIO
CAR            GO TO 12000-99-FIM
CAR        END-IF.
CAR
           MOVE  7                         TO  OUT-POSPROC-T700.
           MOVE '1'                        TO  OUT-FASE-T700.

BR0718     PERFORM 7000-CHECAR-EXIGE-IMOVEL
BR0718
BR0718     IF  WRK-EXIGE-IMOVEL        EQUAL 'N'
BR0718         GO TO 12000-09-TRATA-LAVOURA
BR0718     ELSE
           IF (COD-APLICACAO  OF AREA-RUEC7000 EQUAL 1 OR 2) AND
MAGA          (COD-FINALIDADE OF AREA-RUEC7000 < 25 OR > 36) AND
MF0112        (COD-FINALIDADE OF AREA-RUEC7000 NOT EQUAL 57 AND 59)
             IF  INP-TPEXP-T700 EQUAL SPACES
                 MOVE TAB-ATRIB(7)           TO    OUT-TPEXP-ATTR-T700
                 MOVE WRK-M17                TO    OUT-MENSAG-T700
                 GO                          TO    12000-99-FIM
             ELSE
               IF  INP-TPEXP-T700          EQUAL 'ARRENDAMENTO' OR
                                                 'PROPRIA' OR
                                                 'COMODATO' OR
                                                 'PARCERIA' OR
                                                 'MEEIRO' OR
POL097                                           'USUFRUTO' OR
                                                 'CONDOMINIO'
                   MOVE  INP-TPEXP-T700    TO    TIPO-EXPLOR-RURAL
               ELSE
                   MOVE TAB-ATRIB(7)       TO    OUT-TPEXP-ATTR-T700
                   MOVE WRK-M11            TO    OUT-MENSAG-T700
                   GO                      TO    12000-99-FIM
POL097         END-IF
POL097       END-IF
POL097     END-IF
BR0718     END-IF.
BR0718
      * PARA NAO ABRIR FAZENDA QUANDO FOR COOPERATIVA  - (025/036)
      * E INTEGRALIZACAO - 57
MAGA       IF  (COD-FINALIDADE   GREATER 24 AND LESS 37) OR
MF0112         (COD-FINALIDADE   EQUAL   57 OR 59)
                MOVE  8                    TO  OUT-POSPROC-T700
                IF  INP-PFK-T700 EQUAL '9'
                    MOVE 'CONSULTA NAO PERMITIDA PARA ESSA APLICACAO'
                                           TO    OUT-MENSAG-T700
                    GO                     TO    12000-99-FIM
                ELSE
                    GO TO 12000-09-TRATA-LAVOURA
           ELSE
           IF  (COD-APLICACAO      EQUAL 1) AND
               (FLAG-PROD-PROVAVEL EQUAL 0)
                NEXT SENTENCE
           ELSE
           IF  (COD-APLICACAO      EQUAL 2) AND
               (FLAG-PROD-PROVAVEL EQUAL 0)
                NEXT SENTENCE
           ELSE
           IF  (COD-APLICACAO NOT EQUAL 1 AND 2) OR
               (FLAG-PROD-PROVAVEL EQUAL 0)
                MOVE  8                    TO  OUT-POSPROC-T700
                IF  INP-PFK-T700 EQUAL '9'
                    MOVE 'CONSULTA NAO PERMITIDA PARA ESSA APLICACAO'
                                           TO    OUT-MENSAG-T700
                    GO                     TO    12000-99-FIM
                ELSE
                    GO TO 12000-09-TRATA-LAVOURA.

           IF  INP-PFK-T700 EQUAL '9'
               MOVE SPACES              TO  IIMOV-RURAL OF
                                                     DADOS-IMOVEIS (1)
           ELSE
           IF  INP-IMOVEL-T700 NOT EQUAL SPACES
                   MOVE INP-IMOVEL-T700     TO  IIMOV-RURAL OF
                                                     DADOS-IMOVEIS (1).
      *************************
       12000-08-PASSA-RUEC7530.

           MOVE    'RUEC7000'           TO  PROGRAMA-ANT.

           PERFORM 11400-MOVE-TELA-OUT-IN.

           MOVE 0009                      TO   ERR-LOCAL
           PERFORM 12030-UPDATE-RUECB042.

           MOVE    IO-LTERM            TO  COMU-COD-TERM.
           MOVE    'RUEC7000'          TO  COMU-TRANCODE.
           MOVE    +147                TO  COMU-LL.
           MOVE    COMU-AREA           TO  WRK-MENSAGEM.
           MOVE    'RUEC7530'          TO  WRK-TELA.
           MOVE    WRK-CHNG            TO  WRK-FUNCAO.

           GO TO 12000-99-FIM.


      ***************************
       12000-08-RETORNO-RUEC7530.

           MOVE ' '                 TO  WRK-INCONSIS.

           PERFORM 12100-VERIFICA-COD-MUNIC
                                  VARYING IND FROM 1 BY 1
                                    UNTIL IND GREATER 25
                                       OR WRK-INCONSIS EQUAL 'S'.


           IF WRK-INCONSIS EQUAL 'S'
               MOVE SPACES              TO  WRK-INCONSIS
               MOVE 112                 TO  CONSULTA-BLOQUEIO
               MOVE 'MUNICIPIOS DISTINTOS' TO M25-CAMPO
               MOVE 'MUNICIPIOS DIVERGENTES '  TO  M26-CAMPO-A
               MOVE TAB-ATRIB(2)        TO  OUT-IMOVEL-ATTR-T700
               MOVE 3                   TO  OUT-AUXDCR-T700
               PERFORM 12210-VERIF-BLOQUEIO
               IF WRK-INCONSIS EQUAL 'S'
                  GO TO 12000-99-FIM.

EG1216     PERFORM 2500-CONSISTIR-DAP
EG1216
EG1216     IF  WRK-INCONS-DAP          EQUAL 'S'
EG1216         GO TO 12000-99-FIM
EG1216     END-IF.
EG1216
PW9931     IF WRK-RECEITA-BRUTA         EQUAL 'N'
PW9931        PERFORM 20000-01-TRATA-RECEITA-BRUTA
PW9931     END-IF.
EG1216
      ***************************

       12000-08-RETORNO-PF11.

           MOVE  'S'                   TO  FLAG-GRAVA-IMOV.

      **************************************
      ***  VALIDA LAVOURA/REBANHO/AREA/QTDE
      **************************************
       12000-09-TRATA-LAVOURA.
EG1216
EG1216     PERFORM 2500-CONSISTIR-DAP
EG1216
EG1216     IF  WRK-INCONS-DAP          EQUAL 'S'
EG1216         GO TO 12000-99-FIM
EG1216     END-IF.
EG1216
PW9931     IF WRK-RECEITA-BRUTA         EQUAL 'N'
PW9931        PERFORM 20000-01-TRATA-RECEITA-BRUTA
PW9931     END-IF.

           MOVE  8                         TO  OUT-POSPROC-T700.
           MOVE '1'                        TO  OUT-FASE-T700.

           IF  (COD-APLICACAO     EQUAL 1   OR   2) AND
               (COD-PRODUTO       EQUAL 162 OR 163)
                NEXT SENTENCE
           ELSE
           IF  (COD-APLICACAO NOT EQUAL 1 AND 2) OR
               (FLAG-PROD-PROVAVEL EQUAL 0) OR
MF0112         (COD-FINALIDADE     EQUAL 57 OR 59)
                MOVE  9                    TO  OUT-POSPROC-T700
                GO TO 12000-10-TRATA-BLOQUEIOS.

BR0718     PERFORM 7000-CHECAR-EXIGE-IMOVEL
BR0718
BR0718     IF  WRK-EXIGE-IMOVEL        EQUAL 'N'
BR0718         GO TO 12000-10-TRATA-BLOQUEIOS
BR0718     END-IF.
BR0718
           MOVE    'RUEC7000'              TO  PROGRAMA-ANT.

           MOVE 0010                      TO   ERR-LOCAL
           PERFORM 12030-UPDATE-RUECB042.

      *---------------------------------------------*
      * CHAMADA AO FICA PARA VALIDACAO DO IMOVEL    *
      *---------------------------------------------*

POL097     IF  (COD-APLICACAO OF AREA-RUEC7000 NOT EQUAL 1 AND 2)  OR
POL097         (COD-FINALIDADE  GREATER  24  AND  LESS  37) OR
MF0112         (COD-FINALIDADE  EQUAL    57 OR 59)
                GO  TO  ACESSA-RUEC7708
POL097     END-IF

BR0718     PERFORM 7000-CHECAR-EXIGE-IMOVEL
BR0718
BR0718     IF  WRK-EXIGE-IMOVEL        EQUAL 'N'
BR0718         GO  TO  ACESSA-RUEC7708
BR0718     END-IF.
BR0718
POL097     INITIALIZE  R20-AREA-R7718.
POL097     MOVE 'RUEC7000'             TO  R20-PGMCHMD-R7718
POL097     MOVE CCLUB-RUEC             TO  R20-CCLUB-R7718.

POL097     CALL    'POOL0081'      USING   WRK-RUEC7718
POL097                                     R20-AREA-R7718
POL097                                     IO-PCB
POL097                                     ALT-PCB

POL097     IF  R20-RET-CODE-R7718      EQUAL   99
POL097         MOVE R20-SQLCA-R7718    TO SQLCA
POL097         MOVE R20-ERRO-AREA-R7718 TO ERRO-AREA
POL097         MOVE 'DB2'               TO ERR-TIPO-ACESSO
POL097         MOVE SPACES              TO ERR-SEGM
POL097         MOVE ERR-PGM             TO ERR-MODULO
POL097         PERFORM  99999-ROTINA-ERRO
POL097     ELSE
POL097     IF  R20-RET-CODE-R7718      EQUAL   01
POL097         MOVE WRK-M34A            TO OUT-MENSAG-T700
POL097         GO TO 12000-99-FIM
POL097     ELSE
POL097     IF  R20-RET-CODE-R7718       EQUAL   06
POL097         MOVE WRK-M38             TO OUT-MENSAG-T700
POL097         GO TO 12000-99-FIM
POL097     ELSE
POL097     IF  R20-RET-CODE-R7718  NOT  EQUAL   05
POL097         MOVE WRK-M37             TO OUT-MENSAG-T700
POL097         GO TO 12000-99-FIM.

POL097     INITIALIZE  R22-AREA-R7722.
POL097     MOVE 'RUEC7000'             TO  R22-PGMCHMD-R7722
POL097     MOVE 'P'                    TO  R22-FUNCAO-R7722.
POL097     MOVE 11                     TO  R22-QPESQ-R7722.
POL097     MOVE ZEROS                  TO  R22-QDESPR-R7722.
POL097     MOVE CCLUB-RUEC             TO  R22-CCLUB-R7722.
POL097     MOVE TIPO-PESSOA            TO  R22-TPSSOA-ENV-R7722
POL097     MOVE ZEROS                  TO  R22-SEQPATRM-ENV-R7722.

POL097     CALL    'POOL0081'      USING   WRK-RUEC7722
POL097                                     R22-AREA-R7722
POL097                                     IO-PCB
POL097                                     ALT-PCB

POL097     IF  R22-RET-CODE-R7722       EQUAL   99
POL097         MOVE R22-SQLCA-R7722     TO SQLCA
POL097         MOVE R22-ERRO-AREA-R7722 TO ERRO-AREA
POL097         MOVE 'DB2'               TO ERR-TIPO-ACESSO
POL097         MOVE SPACES              TO ERR-SEGM
POL097         MOVE ERR-PGM             TO ERR-MODULO
POL097         PERFORM  99999-ROTINA-ERRO
           ELSE
POL097     IF  R22-RET-CODE-R7722       EQUAL   01
POL097         MOVE WRK-M333            TO OUT-MENSAG-T700
               GO TO 12000-99-FIM
POL097     ELSE
POL097     IF  R22-RET-CODE-R7722       EQUAL   05
POL097         MOVE R22-MENS-R7722      TO OUT-MENSAG-T700
POL097         GO TO 12000-99-FIM.

      *---------------------------------------------*
      * FINAL - CHAMADA PARA FICA
      *---------------------------------------------*

       ACESSA-RUEC7708.

           MOVE    IO-LTERM            TO  LK-CODIGO-TERMINAL.
           MOVE    SPACES              TO  LK-STATUS-RETORNO
                                           LK-MENSAGEM.

           CALL    'POOL0081'      USING   WRK-RUEC7708
                                           IO-PCB
                                           ALT-PCB
                                           AREA-NAVEGA.

           IF  LK-STATUS-RETORNO   EQUAL   'DB'
               GOBACK
           ELSE
CCM01A     IF LK-STATUS-RETORNO EQUAL 'ER'
CCM01A     AND LK-MENSAGEM(1:7) NOT EQUAL SPACES
CCM01A     AND INP-ORIGCD-T700  NOT EQUAL 100
CCM01A         MOVE TAB-ATRIB(7)       TO    OUT-TPEXP-ATTR-T700
CCM01A     END-IF

           IF  LK-STATUS-RETORNO   EQUAL   'ER'
BR0718     AND WRK-CHECA-AREA-FICHA    EQUAL 'S'
               MOVE LK-MENSAGEM       TO   OUT-MENSAG-T700
               GO TO 12000-99-FIM
BR0718     END-IF
BR0718     END-IF.

           PERFORM 11200-SELECT-RUECB042.


PW9931     IF WRK-INP-CONSOR-T700  EQUAL 'S'
PW9931        GO TO 16500-00-TRATA-CONSORCIO
PW9931     END-IF.

      ******************************************
      ***  VERIFICA INCONSISTENCIAS (BLOQUEIOS)
      ******************************************
       12000-10-TRATA-BLOQUEIOS.

           MOVE  9                         TO  OUT-POSPROC-T700.
           MOVE '1'                        TO  OUT-FASE-T700.

           MOVE    'RUEC7000'              TO  PROGRAMA-ANT
                                               PROGRAMA-BLOQUEIO.

           MOVE    ZEROS                   TO  CONSULTA-BLOQUEIO.

           MOVE 0011                      TO   ERR-LOCAL

ALDO  * QUANDO FOR COOPERATIVA OU INTEGRALIZACAO CARREGA MUNICIPIO DA
ALDO  * DA AGENCIA E CARREGA CMUN_RURAL NA NAVEGACAO_RURAL.
ALDO       IF (COD-FINALIDADE   GREATER 24 AND LESS 37) OR
MF0112        (COD-FINALIDADE   EQUAL   57 OR 59)
ALDO          PERFORM 12600-CARREGA-MUNICIPIO-COOP
ALDO          MOVE CID-MUN OF MESUV050 TO CMUN-RURAL OF DADOS-IMOVEIS(1)
ALDO          MOVE AGENCIA             TO CMATR-IMOV OF DADOS-IMOVEIS(1)
ALDO          MOVE ZEROS       TO CINDCD-AREA-PRINC OF DADOS-IMOVEIS(1)
MF0112     END-IF.

BR0718     PERFORM 7000-CHECAR-EXIGE-IMOVEL
BR0718
BR0718     IF  WRK-EXIGE-IMOVEL        EQUAL 'N'
BR0718        PERFORM 12600-CARREGA-MUNICIPIO-COOP
BR0718        MOVE CID-MUN OF MESUV050 TO CMUN-RURAL OF DADOS-IMOVEIS(1)
BR0718        MOVE AGENCIA             TO WRK-ZD5
BR0718        MOVE WRK-CH10            TO CMATR-IMOV OF DADOS-IMOVEIS(1)
BR0718        MOVE ZEROS       TO CINDCD-AREA-PRINC OF DADOS-IMOVEIS(1)
BR0718     END-IF.
BR0718
           PERFORM 12030-UPDATE-RUECB042.

           MOVE    IO-LTERM            TO  LK-CODIGO-TERMINAL.
           MOVE    SPACES              TO  LK-STATUS-RETORNO
                                           LK-MENSAGEM.

           CALL    'POOL0081'      USING   WRK-RUEC7710
                                           IO-PCB
                                           ALT-PCB
                                           AREA-NAVEGA.

           IF  LK-STATUS-RETORNO   EQUAL   'DB'
               GOBACK
           ELSE
           IF  LK-STATUS-RETORNO   EQUAL   'ER'
               MOVE LK-MENSAGEM       TO   OUT-MENSAG-T700
               GO TO 12000-99-FIM.

           PERFORM 11200-SELECT-RUECB042.

           IF  CODIGO-BLOQUEIO EQUAL 3
               GOBACK.

           IF  CODIGO-BLOQUEIO EQUAL 0
               MOVE MSG-RETORNO-BLOQ    TO  M25-CAMPO
               MOVE WRK-M25             TO  OUT-MENSAG-T700
               MOVE '3'                 TO  OUT-FASE-T700
               GO TO 12000-99-FIM.

           IF  RESP-APROV-INCONS OF AREA-RUEC7000 EQUAL 1
               MOVE 'DO DEF     '       TO  M29-CAMPO-B
           ELSE
           IF  RESP-APROV-INCONS OF AREA-RUEC7000 EQUAL 2
               MOVE 'DO AGRONOMO'       TO  M29-CAMPO-B
           ELSE
               MOVE 'DA AGENCIA '       TO  M29-CAMPO-B.

           IF  CODIGO-BLOQUEIO EQUAL 1
               MOVE TAB-ATRIB(2)        TO  OUT-AGENC-ATTR-T700
               MOVE MSG-RETORNO-BLOQ    TO  M29-CAMPO-A
               MOVE WRK-M29             TO  OUT-MENSAG-T700
               MOVE 4                   TO  OUT-AUXDCR-T700
               MOVE '2'                 TO  OUT-FASE-T700
               GO TO 12000-99-FIM.

      ***************************
       12000-10-RETORNO-PF11.


      **********************************
      ***  VERIFICA TIPO PRODUTOR ATIVO
      **********************************
       12000-11-TRATA-TIPO-PRODUTOR.

           MOVE  10                        TO  OUT-POSPROC-T700.
           MOVE '1'                        TO  OUT-FASE-T700.

BR0718     PERFORM 7000-CHECAR-EXIGE-IMOVEL
BR0718
BR0718     IF  WRK-EXIGE-IMOVEL        EQUAL 'N'
BR0718         CONTINUE
BR0718     ELSE
           IF (COD-APLICACAO  OF AREA-RUEC7000 EQUAL 1 OR 2) AND
              (COD-FINALIDADE OF AREA-RUEC7000 < 25 OR > 36) AND
MF0112        (COD-FINALIDADE OF AREA-RUEC7000 NOT EQUAL 57 AND 59)
             IF  INP-TPEXP-T700 EQUAL SPACES
                 MOVE TAB-ATRIB(7)           TO    OUT-TPEXP-ATTR-T700
                 MOVE WRK-M17                TO    OUT-MENSAG-T700
                 GO                          TO    12000-99-FIM
             ELSE
               IF  INP-TPEXP-T700          EQUAL 'ARRENDAMENTO' OR
                                                 'PROPRIA' OR
                                                 'COMODATO' OR
                                                 'PARCERIA' OR
                                                 'MEEIRO' OR
POL097                                           'USUFRUTO' OR
                                                 'CONDOMINIO'
                   MOVE  INP-TPEXP-T700    TO    TIPO-EXPLOR-RURAL
               ELSE
                   MOVE TAB-ATRIB(7)       TO    OUT-TPEXP-ATTR-T700
                   MOVE WRK-M11            TO    OUT-MENSAG-T700
                   GO                      TO    12000-99-FIM
POL097         END-IF
POL097       END-IF
POL097     END-IF
BR0718     END-IF.

           PERFORM 12110-SELECT-RURCV022.

           IF  SQLCODE EQUAL +100
               MOVE COD-TIPO-PRODUTOR   TO  M9-CAMPO
               MOVE WRK-M9              TO  OUT-MENSAG-T700
               GO TO 12000-99-FIM.

           IF  CSTTUS-ATIVO-INATI OF RURCV022 NOT EQUAL 'A'
               MOVE SPACES              TO  WRK-INCONSIS
               MOVE 116                 TO  CONSULTA-BLOQUEIO
               MOVE 'TP. PRODUTOR INATIVO' TO M25-CAMPO
               MOVE 'TIPO PRODUTOR INATIVO  '  TO  M26-CAMPO-A
               MOVE TAB-ATRIB(2)        TO  OUT-AGENC-ATTR-T700
               MOVE 5                   TO  OUT-AUXDCR-T700
               PERFORM 12210-VERIF-BLOQUEIO
               IF WRK-INCONSIS EQUAL 'S'
                  GO TO 12000-99-FIM.

      ***************************
       12000-11-RETORNO-PF11.


      ****************************
     ***  OBTEM NUMERO DA CEDULA
      ****************************
       12000-12-TRATA-NUM-CEDULA.

           MOVE  11                     TO  OUT-POSPROC-T700.
           MOVE  INP-AGENC-T700         TO  CJUNC-DEPDC OF RUECV001.

           CALL    'POOL7600'          USING   WRK-DATA-HORA
           MOVE    WRK-DT-AAAAMMDD     TO  WRK-DATA

           MOVE    'S'                 TO  WRK-EXISTE.

           PERFORM 12120-SELECT-RUECV001.

           IF  WRK-EXISTE = 'N'
               MOVE  5001               TO  WRK-NUM-CEDULA
               MOVE  ANO OF WRK-DATA-R  TO  WRK-ANO-CEDULA
           ELSE
               MOVE  CCDULA OF RUECV001 TO  WRK-CEDULA-R
               IF    ANO OF WRK-DATA-R  EQUAL  WRK-ANO-CEDULA
                     ADD  1  TO  WRK-NUM-CEDULA
               ELSE
                     MOVE ANO OF WRK-DATA-R  TO  WRK-ANO-CEDULA
                     MOVE 5001  TO  WRK-NUM-CEDULA.

EG0817     PERFORM 2550-CHECAR-CEDULA-EXISTENTE.
EG0817
           MOVE  WRK-CEDULA-R   TO  CCDULA      OF RUECV001
                                    CCDULA      OF AREA-RUEC7000.
           MOVE  INP-AGENC-T700 TO  CJUNC-DEPDC OF RUECV001.

           IF  WRK-EXISTE = 'N'
               PERFORM 12130-INSERT-RUECV001
           ELSE
               PERFORM 12135-UPDATE-RUECV001.

           MOVE  12                   TO   OUT-POSPROC-T700.


      ****************************
      ***  GRAVA PESSOA DA CEDULA
      ****************************
       12000-13-GRAVA-PESSOA-CEDULA.

POL097     IF  (COD-APLICACAO OF AREA-RUEC7000 NOT EQUAL 1 AND 2)  OR
POL097         (COD-FINALIDADE  GREATER  24  AND  LESS  37) OR
MF0112         (COD-FINALIDADE  EQUAL    57 OR 59)
POL097         MOVE  99                TO  R21-RET-CODE-R7720
POL097         GO  TO  12000-13-CONTINUA
POL097     END-IF.

BR0718     PERFORM 7000-CHECAR-EXIGE-IMOVEL
BR0718
BR0718     IF  WRK-EXIGE-IMOVEL        EQUAL 'N'
BR0718         MOVE  99                TO  R21-RET-CODE-R7720
BR0718         GO  TO  12000-13-CONTINUA
BR0718     END-IF.
BR0718
POL097     INITIALIZE  R21-AREA-R7720.
POL097     MOVE 'RUEC7000'             TO  R21-PGMCHMD-R7720
POL097     MOVE 'P'                    TO  R21-FUNCAO-R7720.
POL097     MOVE CCLUB-RUEC             TO  R21-CCLUB-R7720.

POL097     EXEC SQL
POL097          SET :R21-DCEDULA-R7720 = CURRENT TIMESTAMP
POL097     END-EXEC.

POL097     CALL    'POOL0081'      USING   WRK-RUEC7720
POL097                                     R21-AREA-R7720
                                           IO-PCB
                                           ALT-PCB

POL097     IF  R21-RET-CODE-R7720       EQUAL   99
POL097         MOVE R21-SQLCA-R7720     TO SQLCA
POL097         MOVE R21-ERRO-AREA-R7720 TO ERRO-AREA
POL097         MOVE 'DB2'               TO ERR-TIPO-ACESSO
POL097         MOVE SPACES              TO ERR-SEGM
POL097         MOVE ERR-PGM             TO ERR-MODULO
POL097         PERFORM  99999-ROTINA-ERRO
POL097     ELSE
POL097     IF  R21-RET-CODE-R7720       EQUAL   02
POL097         MOVE WRK-M35             TO OUT-MENSAG-T700
POL097         GO TO 12000-99-FIM
POL097     ELSE
POL097     IF  R21-RET-CODE-R7720       EQUAL   03
POL097         MOVE WRK-M36             TO OUT-MENSAG-T700
POL097         GO TO 12000-99-FIM
POL097     ELSE
POL097     IF  R21-RET-CODE-R7720  NOT  EQUAL   ZEROS
POL097         MOVE WRK-M34             TO OUT-MENSAG-T700
POL097         GO TO 12000-99-FIM.

POL097 12000-13-CONTINUA.

           MOVE AGENCIA                 TO CJUNC-DEPDC      OF RUECV008.
           MOVE CCDULA OF AREA-RUEC7000 TO CCDULA           OF RUECV008.

           IF TIPO-PESSOA            EQUAL 'J'
              MOVE NUMCGC               TO CCGC-CPF         OF RUECV008
              MOVE FILIAL               TO CFLIAL-CGC       OF RUECV008
              MOVE CTRL-CGC             TO CCTRL-CPF-CGC    OF RUECV008
MAG           MOVE 2                    TO CTPO-PSSOA       OF RUECV008
           ELSE
              MOVE NUMCPF               TO CCGC-CPF         OF RUECV008
              MOVE ZEROS                TO CFLIAL-CGC       OF RUECV008
              MOVE CTRL-CPF             TO CCTRL-CPF-CGC    OF RUECV008
MAG           MOVE 1                    TO CTPO-PSSOA       OF RUECV008.

           MOVE 'T'                     TO CTPO-RESP-PSSOA  OF RUECV008.
           MOVE NOME                    TO IPSSOA           OF RUECV008.
           MOVE 1                       TO CSEQ-PSSOA       OF RUECV008.
           MOVE ZEROS                   TO VRESP-RURAL      OF RUECV008.
           MOVE ZEROS                   TO VRESP-GRAL       OF RUECV008.
           MOVE ZEROS                   TO VSDO-MED-TMES    OF RUECV008.
           MOVE ZEROS                   TO VSDO-MED-SEMTL   OF RUECV008.
           MOVE ZEROS                   TO VRENDA-AGROP-OUTRO
                                                            OF RUECV008.
           MOVE ZEROS                   TO VRENDA-NAO-AGROP OF RUECV008.
           MOVE ZEROS                   TO DANO-RENDM       OF RUECV008.

POL097     IF  R21-RET-CODE-R7720       NOT EQUAL ZEROS
POL097         MOVE '01.01.0001'          TO
POL097                                    DINCL OF NAVEGACAO-RURAL
POL097     ELSE
POL097         MOVE R21-DCAD-PSSOA-R7720  TO
POL097                                    DINCL OF NAVEGACAO-RURAL
POL097     END-IF.

           IF  COD-APLICACAO              EQUAL  6
               CALL   'POOL7600'          USING  WRK-DATA-HORA
               MOVE   WRK-DT-AAAAMMDD     TO     WRK-DATA
               MOVE   CORR WRK-DATA-R     TO     WRK-DATA-DB2
               MOVE   WRK-DATA-DB2        TO     DCAD OF RUECV008
           ELSE
           IF  COD-APLICACAO              EQUAL  3
               MOVE  '01.01.0001'         TO     DCAD OF RUECV008
           ELSE
               MOVE  DINCL OF NAVEGACAO-RURAL TO  DCAD OF RUECV008.

           PERFORM 12140-INSERT-RUECV008.

           MOVE  13                   TO   OUT-POSPROC-T700.

      *****************************
      ***  GRAVA CEDULA TEMPORARIA
      *****************************
       12000-14-GRAVA-CEDULA-TEMP.

           INITIALIZE RUECB007.


           MOVE AGENCIA                  TO CJUNC-DEPDC   OF RUECB007.
           MOVE CCDULA  OF AREA-RUEC7000 TO CCDULA        OF RUECB007.
           MOVE CONTA                    TO CCTA-CORR     OF RUECB007.
           MOVE COD-TIPO-PRODUTOR        TO CTPO-PROTR-RURAL
                                                          OF RUECB007.
           MOVE COD-ATIVIDADE            TO CATVDD-RURAL  OF RUECB007.
           MOVE COD-FINALIDADE           TO CFNALD-RURAL  OF RUECB007.
           MOVE COD-PRODUTO              TO CPRODT-RURAL  OF RUECB007.

PW9931     MOVE COD-TIPO-CEDULA     TO CTPO-CDULA-RECOR   OF RUECB007.
PW9931     MOVE COD-MODALIDADE      TO CMODLD-RURAL       OF RUECB007.
PW9931     MOVE COD-VARIEDADE       TO CVAR-PRODT-RURAL   OF RUECB007.
PW9931     MOVE COD-CESTA-SAFRA     TO CCESTA-SAFRA-RURAL OF RUECB007.
PW9931     MOVE COD-TIPO-AGROPEC    TO CTPO-AGROP-RURAL   OF RUECB007.
PW9931     MOVE COD-TIPO-INTEGR     TO CTPO-INTGC-RURAL   OF RUECB007.
PW9931     MOVE COD-TIPO-IRRIG      TO CTPO-IGCAO-RURAL   OF RUECB007.
PW9931     MOVE COD-TIPO-CULTIVO    TO CTPO-CTIVO-RURAL   OF RUECB007.
PW9931     MOVE COD-CICLO-PROD      TO CCICLO-PROD-RURAL  OF RUECB007.
PRI499     MOVE COD-CLIMA-PROD      TO CCLIMT-RURAL-BACEN OF RUECB007.
PW9931     MOVE COD-ZONEAMENTO      TO CINDCD-ZNMTO       OF RUECB007.
PW9931     MOVE COD-CONSORCIO       TO CINDCD-CSCIO       OF RUECB007.
PW9931     MOVE COD-GRAO-SEMTE      TO CINDCD-UND-PRDTV   OF RUECB007.
LMC        MOVE COD-TIPO-BENEFIC    TO CTPO-BNEFC-RURAL   OF RUECB007.
LMC        MOVE ZEROS               TO WRK-CTPO-BNEFC-RURAL-NULL.

WI1121     IF  COD-ZONEAMENTO      EQUAL '1'
WI1121         MOVE   RUEC00-CCICLO-CTVDA      TO  CCICLO-CTVDA
WI1121                                         OF  RUECB007
WI1121         MOVE   RUEC00-CTPO-SOLO-AGCLT   TO  CTPO-SOLO-AGCLT
WI1121                                         OF  RUECB007
WI1121         MOVE   RUEC00-CMANUT-SOLO       TO  CMANUT-SOLO
WI1121                                         OF  RUECB007
WI1121     ELSE
WI1121         MOVE   -1                   TO  WRK-CTPO-SOLO-AGCLT-NULL
WI1121                                         WRK-CCICLO-CTVDA-NULL
WI1121                                         WRK-CMANUT-SOLO-NULL
WI1121     END-IF

           IF FLAG-AREA-QTDE EQUAL 1
              MOVE QTDE-FINANC           TO QUND-FINCD-RURAL
                                                          OF RUECB007
           ELSE
           IF FLAG-AREA-QTDE EQUAL 2
              MOVE AREA-FINANC           TO QUND-FINCD-RURAL
                                                          OF RUECB007
           ELSE
              MOVE ZEROS                 TO QUND-FINCD-RURAL
                                                          OF RUECB007.
BR1218     IF  FLAG-AREA-QTDE            EQUAL 1
BR1218         PERFORM 7100-CHECAR-EXIGE-AREA
BR1218         IF  WRK-EXIGE-AREA        EQUAL 'S'
BR1218             MOVE AREA-FINANC      TO VEGF          OF RUECB007
BR1218             MOVE ZEROS            TO WRK-V07-VEGF-N
BR1218         ELSE
BR1218             MOVE ZEROS            TO VEGF          OF RUECB007
BR1218             MOVE -1               TO WRK-V07-VEGF-N
BR1218         END-IF
BR1218     END-IF.
BR1218
           MOVE COD-UNID-FINAN           TO CUND-MEDD-FINCD
                                                          OF RUECB007.
           MOVE PROD-PROVAVEL            TO QUND-PROD-PROVV
                                                          OF RUECB007.
           MOVE COD-UNID-PROD-PROV       TO CUND-MEDD-PROVV
                                                          OF RUECB007.
           PERFORM 12165-VERIFICA-RESP VARYING IND FROM 1 BY 1
                                         UNTIL IND GREATER 7.
           MOVE CINDCD-RESP-APROV        OF NAVEGACAO-RURAL
                                     TO CINDCD-RESP-APROV OF RUECB007.

BOOG       MOVE ANO OF WRK-DATA-R        TO DINIC-SAFRA   OF RUECB007.

BOOG       MOVE ZEROS                    TO DFIM-SAFRA    OF RUECB007
                                            VFINAN        OF RUECB007
                                            VREC-PPRIO    OF RUECB007
                                            CESPCE-TITLO-GARNT
                                                          OF RUECB007
                                            CTPO-GARNT    OF RUECB007
                                            CINCID-PROAGRO
                                                          OF RUECB007
                                            CGARTD        OF RUECB007
                                            CAUTRZ-OPER   OF RUECB007
                                            CTPO-CONDC-FSCAL
                                                          OF RUECB007
                                            CIMPRE-CDULA-TEMPR
                                                          OF RUECB007
           IF   COD-APLICACAO     EQUAL 3 OR 6
                MOVE SPACES              TO RESPCE-EXPLO-IMOV
                                                          OF RUECB007
           ELSE
                MOVE TIPO-EXPLOR-RURAL   TO RESPCE-EXPLO-IMOV
                                                          OF RUECB007.

PW9931     MOVE RECEITA-BRUTA            TO VRECTA-BRUTA-EMPTO
PW9931                                                    OF RUECB007.

POL097****XMOVE TIMESTAMP-FICA           TO CPSSOA-CADTR  OF RUECB007.
POL097     MOVE '0001-01-01-01.01.01.000001'
POL097                                   TO CPSSOA-CADTR  OF RUECB007.
           MOVE WRK-COD-USER             TO CFUNC-BDSCO   OF RUECB007.
           IF   TIPO-EXPLOR-RURAL        EQUAL     'PROPRIA'
                MOVE 1                   TO CESPCE-EXPLO-IMOV
                                                          OF RUECB007
           ELSE
           IF   TIPO-EXPLOR-RURAL        EQUAL     'ARRENDAMENTO'
                MOVE 2                   TO CESPCE-EXPLO-IMOV
                                                          OF RUECB007
           ELSE
                MOVE 3                   TO CESPCE-EXPLO-IMOV
                                                          OF RUECB007.
           MOVE COD-ORIG-REC             TO CORIGE-REC    OF RUECB007.
           MOVE 'RUEC7010'               TO CPROG         OF RUECB007.
           MOVE COD-APLICACAO            TO CAPLIC-RURAL  OF RUECB007.
           MOVE 0                        TO CSIT-CDULA-TEMPR
                                                          OF RUECB007.

           MOVE  SPACES                  TO  IPPRIE       OF RUECB007
           MOVE  ZEROS                   TO  MAREA-TOT    OF RUECB007
           MOVE  ZEROS                   TO  CMUN-RURAL   OF RUECB007
           MOVE  SPACES                  TO  CMATR-IMOV-RURAL
                                                          OF RUECB007.
POL097     MOVE  CCLUB-RUEC              TO  CCLI-UNIC-BDSCO
POL097                                                    OF RUECB007.

           PERFORM 12500-VARRE-DADOS-IMOVEIS
               VARYING IND FROM 1 BY 1
                 UNTIL IND GREATER 25.

      * QUANDO FOR COOPERATIVA OU INTEGRALIZACAO CARREGA MUNICIPIO DA
      * DA AGENCIA E CARREGA CPROJ_RURAL NA NAVEGACAO_RURAL.
           IF  (COD-FINALIDADE   GREATER 24 AND LESS 37) OR
ALDO           (COD-APLICACAO OF AREA-RUEC7000 EQUAL 3 OR 6) OR
MF0112         (COD-FINALIDADE   EQUAL   57 OR 59)
               PERFORM 12600-CARREGA-MUNICIPIO-COOP
               PERFORM 12075-SELECT-RURCV014
MF0112     END-IF.

BR0718     PERFORM 7000-CHECAR-EXIGE-IMOVEL
BR0718
BR0718     IF  WRK-EXIGE-IMOVEL        EQUAL 'N'
BR0718         PERFORM 12600-CARREGA-MUNICIPIO-COOP
BR0718         PERFORM 12075-SELECT-RURCV014
BR0718     END-IF.
BR0718
POL129     MOVE  '01.01.0001'            TO  DDSPDO-AG    OF RUECB007
ACT115     MOVE  '01.01.0001'            TO  DIMPRE-CONTR-RURAL
ACT115                                                    OF RUECB007
POL128     MOVE  ZEROS                   TO  CSEGDR-CREDT-RURAL
EG0817                                   OF  RUECB007
POL097     IF  R21-RET-CODE-R7720       NOT EQUAL ZEROS
POL097         MOVE SPACES              TO CINDCD-FONE-RESID
POL097                                             OF RUECB007
POL097                                     CINDCD-FONE-COML
POL097                                             OF RUECB007
POL097                                     CINDCD-ENDER-RESID
POL097                                             OF RUECB007
POL097                                     CINDCD-ENDER-COML
POL097                                             OF RUECB007
POL097                                     CINDCD-QUALF-PROFS
POL097                                             OF RUECB007
POL097         MOVE ZEROS               TO QBLOCO-PATRM-IMOV
POL097                                             OF RUECB007
POL097                                     QBLOCO-PATRM-VEIC
POL097                                             OF RUECB007
POL097                                     QBLOCO-LSNG-CLI
POL097                                             OF RUECB007
POL097                                     QBLOCO-SEGUR-CLI
POL097                                             OF RUECB007
POL097                                     QBLOCO-QUADR-SCIAL
POL097                                             OF RUECB007
POL097     ELSE
POL097         MOVE R21-CIND-TRES-RET-R7720  TO CINDCD-FONE-RESID
POL097                                             OF RUECB007
POL097         MOVE R21-CIND-TCOM-RET-R7720  TO CINDCD-FONE-COML
POL097                                             OF RUECB007
POL097         MOVE R21-CIND-ERES-RET-R7720  TO CINDCD-ENDER-RESID
POL097                                             OF RUECB007
POL097         MOVE R21-CIND-ECOM-RET-R7720  TO CINDCD-ENDER-COML
POL097                                             OF RUECB007
POL097         MOVE R21-CIND-QPFS-RET-R7720  TO CINDCD-QUALF-PROFS
POL097                                             OF RUECB007
POL097         MOVE R21-QPATR-IMOV-RET-R7720 TO QBLOCO-PATRM-IMOV
POL097                                             OF RUECB007
POL097         MOVE R21-QPATR-VEIC-RET-R7720 TO QBLOCO-PATRM-VEIC
POL097                                             OF RUECB007
POL097         MOVE R21-QLSNG-RET-R7720      TO QBLOCO-LSNG-CLI
POL097                                             OF RUECB007
POL097         MOVE R21-QSEGUR-RET-R7720     TO QBLOCO-SEGUR-CLI
POL097                                             OF RUECB007
POL097         MOVE R21-QQDR-SCIAL-RET-R7720 TO QBLOCO-QUADR-SCIAL
POL097                                             OF RUECB007
POL097     END-IF.

CAR   **  XIF  WRK-INP-TPOBEN-T700-X EQUAL SPACES OR
CAR   **       WRK-INP-TPOBEN-T700   EQUAL ZEROS
CAR   **      XPERFORM 2399-TRATA-BENEFICIARIO
CAR   **      XGO TO 12000-99-FIM
CAR   **   END-IF.
EG1216
EG1216     PERFORM 2500-CONSISTIR-DAP
EG1216
EG1216     IF  WRK-INCONS-DAP          EQUAL 'S'
EG1216         GO TO 12000-99-FIM
EG1216     END-IF.
EG1216
101020     MOVE  FORMA-LIQ-CONTR      TO   CFORMA-LIQDC
101020                                OF   RUECB007.
101020     MOVE  INDC-OPTE-CHEQ       TO   CINDCD-OPTE-CHEQ
101020                                OF   RUECB007.
101020     MOVE  INDC-OPTE-REENV      TO   CINDCD-OPTE-REENV
101020                                OF   RUECB007.

0524       PERFORM 1530-00-SELECT-MANEJO.
0524       MOVE CRURWG2Y-S-COUTRO-TRATO
                                    TO COUTRO-TRATO-SOLO  OF RUECB007

           PERFORM 12150-INSERT-RUECB007.

           MOVE  14                   TO   OUT-POSPROC-T700.

      **************************
      ***  GRAVA INCONSISTENCIAS
      **************************
       12000-15-GRAVA-INCONSIS.

           IF  FLAG-GRAVA-INCONS   NOT EQUAL SPACES
               PERFORM 12180-GRAVA-TAB-INCONSIS
                                       VARYING IND FROM 1 BY 1
                                         UNTIL IND GREATER 7.

           MOVE  15                   TO   OUT-POSPROC-T700.


      **********************************
      ***  GRAVA MATRICULAS DOS IMOVEIS
      **********************************
       12000-16-GRAVA-MATRIC-IMOV.

           IF  FLAG-GRAVA-IMOV NOT EQUAL SPACES
               PERFORM 12190-GRAVA-TAB-IMOVEL
                                       VARYING IND FROM 1 BY 1
                                         UNTIL IND GREATER 25.


           MOVE  16                   TO   OUT-POSPROC-T700.


PW9931**********************************
PW9931***  GRAVA PRODUTOS CONSORCIADOS
PW9931**********************************

PW9931     IF WRK-INP-CONSOR-T700  EQUAL 'S'
PW9931        PERFORM 18500-SELECT-PROD-CONS THRU 18500-99-FIM
PW9931
PW9931        IF  WRK-FLAG-PROD-CONS     EQUAL 'S'
PW9931            PERFORM 18000-GRAVA-TAB-PRODT
PW9931                       VARYING WRK-IND FROM 1 BY 1 UNTIL
PW9931                       R31-PROD-CONSORCIADO(WRK-IND) EQUAL ZEROS
PW9931            PERFORM 1801-00-DELETA-032
PW9931        END-IF
PW9931     END-IF.

      **************************************************
      ***  PASSA CONTROLE PROGRAMA DE INCLUSAO RUEC7010
      **************************************************
       12000-17-PASSA-RUEC7010.

           MOVE    'RUEC7000'           TO  PROGRAMA-ANT.

           PERFORM 11400-MOVE-TELA-OUT-IN.

           MOVE 0012                      TO   ERR-LOCAL
           PERFORM 12030-UPDATE-RUECB042.

           MOVE    IO-LTERM            TO  COMU-COD-TERM.
           MOVE    'RUEC7000'          TO  COMU-TRANCODE.
           MOVE    +147                TO  COMU-LL.
           MOVE    COMU-AREA           TO  WRK-MENSAGEM.
           MOVE    'RUEC7010'          TO  WRK-TELA.
           MOVE    WRK-CHNG            TO  WRK-FUNCAO.

      *---------------------------------------------------------------*
       12000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
POL126*12001-ACESSAR-RUECB045 SECTION.
POL126*---------------------------------------------------------------*

POL126*   XEXEC SQL
POL126*     XSELECT CCONTR,
POL126*             CLIN_OBS_DEVLC,
POL126*             RLIN_OBS_DEVLC
POL126*     XINTO   :RUECB045.CCONTR,
POL126*             :RUECB045.CLIN-OBS-DEVLC,
POL126*             :RUECB045.RLIN-OBS-DEVLC
POL126*     XFROM   DB2PRD.OBS_DEVLC_RURAL
POL126*     XWHERE  CCONTR = :RUECB045.CCONTR
POL126*   XEND-EXEC.

POL126*   XIF ( SQLCODE   NOT  =    ZEROS AND +100 AND -811 ) OR
POL126*       ( SQLWARN0       =    'W'   )
POL126*        XMOVE 'DB2'                    TO   ERR-TIPO-ACESSO
POL126*        XMOVE 'OBS_DEVLC_RURAL'        TO   ERR-DBD-TAB
POL126*        XMOVE 'SELECT  '               TO   ERR-FUN-COMANDO
POL126*        XMOVE SQLCODE                  TO   ERR-SQL-CODE
POL126*        XMOVE 0096                     TO   ERR-LOCAL
POL126*        XMOVE SPACES                   TO   ERR-SEGM
POL126*        XPERFORM 99999-ROTINA-ERRO.

POL126*---------------------------------------------------------------*
POL126*12001-99-FIM. EXIT.
POL126*---------------------------------------------------------------*

      *---------------------------------------------------------------*
MI1008 120031-CONSISTE-GRUPO-PORTE SECTION.
MI1008*---------------------------------------------------------------*
MI1008
MI1008
MI1008     IF  INP-PORTE-T700 EQUAL 'GRANDE'
MI1008         PERFORM  120032-SELECT-RUECV0022-G
MI1008     ELSE
MI1008         PERFORM  120033-SELECT-RUECV0022-P.
MI1008
MI1008
MI1008
MI1008*---------------------------------------------------------------*
MI1008 120031-CONSISTE-GRUPO-PORTE-F. EXIT.
MI1008*---------------------------------------------------------------*
MI1008*---------------------------------------------------------------*
MI1008 120032-SELECT-RUECV0022-G SECTION.
MI1008*---------------------------------------------------------------*
MI1008
MI1008     MOVE  INP-GRUPO-T700        TO        WRK-LIKE-GRUPO-G.
MI1008
MI1008         EXEC SQL
MI1008          SELECT  CTPO_PROTR_RURAL INTO :RURCV022.CTPO-PROTR-RURAL
MI1008            FROM  DB2PRD.V01TPO_PROTR_RURAL
MI1008           WHERE
MI1008                  RTPO_PROTR_RURAL LIKE :WRK-LIKE-G-R
MI1008        END-EXEC
MI1008
MI1008     IF   (( SQLCODE   NOT  =    ZEROS )  AND
MI1008           ( SQLCODE   NOT  =     +100 ))  OR
MI1008           ( SQLWARN0       =    'W'   )
MI1008             MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
MI1008             MOVE 'V01TPO_PROTR_RURAL'     TO   ERR-DBD-TAB
MI1008             MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
MI1008             MOVE SQLCODE                  TO   ERR-SQL-CODE
MI1008             MOVE 0033                     TO   ERR-LOCAL
MI1008             MOVE SPACES                   TO   ERR-SEGM
MI1008             PERFORM 99999-ROTINA-ERRO.
MI1008
MI1008*---------------------------------------------------------------*
MI1008 120032-RUECV0022-G-FIM. EXIT.
MI1008*---------------------------------------------------------------*
MI1008
MI1008*---------------------------------------------------------------*
MI1008 120033-SELECT-RUECV0022-P SECTION.
MI1008*---------------------------------------------------------------*
MI1008
MI1008     MOVE  INP-GRUPO-T700        TO        WRK-LIKE-GRUPO-P.
MI1008
MI1008         EXEC SQL
MI1008          SELECT  CTPO_PROTR_RURAL INTO :RURCV022.CTPO-PROTR-RURAL
MI1008            FROM  DB2PRD.V01TPO_PROTR_RURAL
MI1008           WHERE
MI1008                  RTPO_PROTR_RURAL LIKE :WRK-LIKE-P-R
MI1008        END-EXEC
MI1008
MI1008     IF   (( SQLCODE   NOT  =    ZEROS )  AND
MI1008           ( SQLCODE   NOT  =     +100 ))  OR
MI1008           ( SQLWARN0       =    'W'   )
MI1008             MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
MI1008             MOVE 'V01TPO_PROTR_RURAL'     TO   ERR-DBD-TAB
MI1008             MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
MI1008             MOVE SQLCODE                  TO   ERR-SQL-CODE
MI1008             MOVE 0034                     TO   ERR-LOCAL
MI1008             MOVE SPACES                   TO   ERR-SEGM
MI1008             PERFORM 99999-ROTINA-ERRO.
MI1008
MI1008*---------------------------------------------------------------*
MI1008 120033-SELECT-RUECV0022-P-FIM. EXIT.
MI1008*---------------------------------------------------------------*
      *---------------------------------------------------------------*
       12003-EDITA-CAMPOS-DECIMAIS SECTION.
      *---------------------------------------------------------------*

           PERFORM 12005-INSPECT-CAMPOS.

           IF INP-AREAF-T700-X  NOT EQUAL SPACES
              MOVE INP-AREAF-T700-X       TO  WRK-ENTRADA
                                              WRK-TABELA-CAMPO
              CALL 'POOL0496' USING WRK-ENTRADA WRK-SAIDA WRK-TAMANHO
              IF RETURN-CODE     EQUAL    08
                 MOVE 'CAMPO EXCEDEU TAMANHO MAXIMO'
                                          TO  OUT-MENSAG-T700
                 MOVE TAB-ATRIB(4)        TO  OUT-AREAF-ATTR-T700
                 MOVE 'S'                 TO  WRK-INCONS-AREAF
                 GO   TO  12003-99-FIM
              ELSE
              IF RETURN-CODE NOT EQUAL ZEROS AND NOT EQUAL 20
                 MOVE WRK-M18             TO  OUT-MENSAG-T700
                 MOVE TAB-ATRIB(4)        TO  OUT-AREAF-ATTR-T700
                 MOVE 'S'                 TO  WRK-INCONS-AREAF
                 GO   TO  12003-99-FIM
              ELSE
                 PERFORM 12300-VERIFICA-VIRGULA
                 IF WRK-INCONSIS-BYTE EQUAL 'S'
                    MOVE 'S'              TO  WRK-INCONS-AREAF
                    MOVE WRK-M27          TO  OUT-MENSAG-T700
                    MOVE TAB-ATRIB(4)     TO  OUT-AREAF-ATTR-T700
                    GO   TO  12003-99-FIM
                 ELSE
                    MOVE WRK-SAIDA        TO  INP-AREAF-T700-X.

           IF INP-QTDEF-T700-X  NOT EQUAL SPACES
              MOVE INP-QTDEF-T700-X       TO  WRK-ENTRADA
                                              WRK-TABELA-CAMPO
              CALL 'POOL0496' USING WRK-ENTRADA WRK-SAIDA WRK-TAMANHO
              IF RETURN-CODE     EQUAL    08
                 MOVE 'CAMPO EXCEDEU TAMANHO MAXIMO'
                                          TO  OUT-MENSAG-T700
                 MOVE TAB-ATRIB(4)        TO  OUT-QTDEF-ATTR-T700
                 MOVE 'S'                 TO  WRK-INCONS-QTDEF
                 GO   TO  12003-99-FIM
              ELSE
              IF RETURN-CODE NOT EQUAL ZEROS AND 20
                 MOVE WRK-M18             TO  OUT-MENSAG-T700
                 MOVE TAB-ATRIB(4)        TO  OUT-QTDEF-ATTR-T700
                 MOVE 'S'                 TO  WRK-INCONS-QTDEF
                 GO   TO  12003-99-FIM
              ELSE
                 PERFORM 12300-VERIFICA-VIRGULA
                 IF WRK-INCONSIS-BYTE EQUAL 'S'
                    MOVE 'S'              TO  WRK-INCONS-QTDEF
                    MOVE WRK-M27          TO  OUT-MENSAG-T700
                    MOVE TAB-ATRIB(4)     TO  OUT-QTDEF-ATTR-T700
                    GO   TO  12003-99-FIM
                 ELSE
                    MOVE WRK-SAIDA        TO  INP-QTDEF-T700-X.

STEFA2*   XIF  WRK-INCONS-QTDEF        EQUAL 'S'
.     *       XCONTINUE
.     *   XELSE
.              IF  INP-QTDEF-T700      GREATER ZEROS
.                  PERFORM 7200-ACESSAR-CRUR236G
.                  IF  WRK-UNIDADE-ANIMAL
.                                      EQUAL 'S'
STEFA2** COMENTARIO: INDEPENDENTE DO QUE ENCONTRAR SE A QUANTIDADE
STEFA2**             INDEPENDENTE DO QUE ENCONTRAR SE A QUANTIDADE
STEFA2                 IF  WRK-CONTINUACAO NOT EQUAL 'S'
STEFA2                     PERFORM 11200-SELECT-RUECB042
STEFA2                     IF  QUND-ANMAL  OF NAVEGACAO-RURAL
STEFA2                                 EQUAL ZEROS
.                              MOVE WRK-M62    TO OUT-MENSAG-T700
.                              MOVE 'A'        TO WRK-INCONS-QTDEF
.                              GO              TO 12003-99-FIM
STEFA2                     END-IF
STEFA2                 END-IF
STEFA2             END-IF
.              END-IF
STEFA2*   XEND-IF

GUIAQU     IF      INP-AQTDEF-T700     NOT NUMERIC
.                  MOVE    INP-AQTDEF-T700-X   TO  OUT-AQTDEF-T700-X
.          ELSE
.                  MOVE    INP-AQTDEF-T700     TO  OUT-AQTDEF-T700
GUIAQU     END-IF.

           IF INP-CAMPO-T700-X  NOT EQUAL SPACES
              MOVE INP-CAMPO-T700-X       TO  WRK-ENTRADA
                                              WRK-TABELA-CAMPO
              CALL 'POOL0496' USING WRK-ENTRADA WRK-SAIDA WRK-TAMANHO
              IF RETURN-CODE     EQUAL    08
                 MOVE 'CAMPO EXCEDEU TAMANHO MAXIMO'
                                          TO  OUT-MENSAG-T700
                 MOVE TAB-ATRIB(4)        TO  OUT-CAMPO-ATTR-T700
                 MOVE 'S'                 TO  WRK-INCONS-CAMPO
              ELSE
              IF RETURN-CODE NOT EQUAL ZEROS AND NOT EQUAL 20
                 MOVE WRK-M18             TO  OUT-MENSAG-T700
                 MOVE TAB-ATRIB(4)        TO  OUT-CAMPO-ATTR-T700
                 MOVE 'S'                 TO  WRK-INCONS-CAMPO
              ELSE
                 PERFORM 12300-VERIFICA-VIRGULA
                 IF WRK-INCONSIS-BYTE EQUAL 'S'
                    MOVE 'S'              TO  WRK-INCONS-CAMPO
                    MOVE WRK-M27          TO  OUT-MENSAG-T700
                    MOVE TAB-ATRIB(4)     TO  OUT-CAMPO-ATTR-T700
                 ELSE
                    MOVE WRK-SAIDA        TO  INP-CAMPO-T700-X.

201503     MOVE SPACES                 TO WRK-MANTEM-ATTR.
201503     IF  WRK-OUT-RECBR-ATTR-T700 EQUAL WRK-TAB-ATRIB(3)
201503         MOVE  'S'               TO WRK-MANTEM-ATTR
201503     END-IF.
201503
PW9931     IF WRK-INP-RECBR-T700-X  NOT EQUAL SPACES
PW9931        MOVE WRK-INP-RECBR-T700-X   TO  WRK-ENTRADA-2
PW9931                                        WRK-TABELA-CAMPO
PW9931        CALL 'POOL0496' USING
PW9931                           WRK-ENTRADA-2 WRK-SAIDA-2 WRK-TAMANHO-2
PW9931        IF RETURN-CODE     EQUAL    08
PW9931           MOVE 'CAMPO EXCEDEU TAMANHO MAXIMO'
PW9931                                    TO  OUT-MENSAG-T700
PW9931           MOVE TAB-ATRIB(4)        TO  WRK-OUT-RECBR-ATTR-T700
PW9931           MOVE 'S'                 TO  WRK-INCONS-CAMPO
PW9931        ELSE
PW9931        IF RETURN-CODE NOT EQUAL ZEROS AND NOT EQUAL 20
PW9931           MOVE WRK-M18             TO  OUT-MENSAG-T700
PW9931           MOVE TAB-ATRIB(4)        TO  WRK-OUT-RECBR-ATTR-T700
PW9931           MOVE 'S'                 TO  WRK-INCONS-CAMPO
PW9931        ELSE
PW9931           PERFORM 12300-VERIFICA-VIRGULA
PW9931           IF WRK-INCONSIS-BYTE EQUAL 'S'
PW9931              MOVE 'S'              TO  WRK-INCONS-CAMPO
PW9931              MOVE WRK-M27          TO  OUT-MENSAG-T700
PW9931              MOVE TAB-ATRIB(4)     TO  WRK-OUT-RECBR-ATTR-T700
PW9931           ELSE
PW9931              MOVE WRK-SAIDA-2      TO  WRK-INP-RECBR-T700-X.

201503     IF  WRK-MANTEM-ATTR         EQUAL 'S'
201503         MOVE TAB-ATRIB(3)       TO WRK-OUT-RECBR-ATTR-T700
201503     END-IF.
201503
      *---------------------------------------------------------------*
       12003-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12004-RETORNA-INSPECT SECTION.
      *---------------------------------------------------------------*

           IF  WRK-INCONS-AREAF     EQUAL 'S'
               INSPECT INP-AREAF-T700-X REPLACING LEADING '0' BY ' '
               MOVE    INP-AREAF-T700-X  TO  OUT-AREAF-T700-X
               INSPECT INP-QTDEF-T700-X REPLACING LEADING '0' BY ' '
               MOVE    INP-QTDEF-T700-X  TO  OUT-QTDEF-T700-X
               INSPECT INP-CAMPO-T700-X REPLACING LEADING '0' BY ' '
               MOVE    INP-CAMPO-T700-X  TO  OUT-CAMPO-T700-X.

           IF  WRK-INCONS-QTDEF     EQUAL 'S'
               INSPECT INP-QTDEF-T700-X REPLACING LEADING '0' BY ' '
               MOVE    INP-QTDEF-T700-X  TO  OUT-QTDEF-T700-X
               INSPECT INP-CAMPO-T700-X REPLACING LEADING '0' BY ' '
               MOVE    INP-CAMPO-T700-X  TO  OUT-CAMPO-T700-X.

           IF  WRK-INCONS-CAMPO     EQUAL 'S'
               INSPECT INP-CAMPO-T700-X REPLACING LEADING '0' BY ' '
               MOVE    INP-CAMPO-T700-X  TO  OUT-CAMPO-T700-X.

      *---------------------------------------------------------------*
       12004-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12005-INSPECT-CAMPOS SECTION.
      *---------------------------------------------------------------*

           IF  INP-AGENC-T700-X NOT EQUAL SPACES
               INSPECT INP-AGENC-T700-X REPLACING LEADING ' ' BY '0'.

           IF  INP-CONTA-T700-X NOT EQUAL SPACES
               INSPECT INP-CONTA-T700-X REPLACING LEADING ' ' BY '0'.

           IF  INP-CGCNUM-T700-X NOT EQUAL SPACES
               INSPECT INP-CGCNUM-T700-X REPLACING LEADING ' ' BY '0'.

           IF  INP-CGCFIL-T700-X NOT EQUAL SPACES
               INSPECT INP-CGCFIL-T700-X REPLACING LEADING ' ' BY '0'.

           IF  INP-CTRL-T700-X NOT EQUAL SPACES
               INSPECT INP-CTRL-T700-X REPLACING LEADING ' ' BY '0'.

           IF  INP-ORIGCD-T700-X NOT EQUAL SPACES
               INSPECT INP-ORIGCD-T700-X REPLACING LEADING ' ' BY '0'.

           IF  INP-ATIVCD-T700-X NOT EQUAL SPACES
               INSPECT INP-ATIVCD-T700-X REPLACING LEADING ' ' BY '0'.

PW9931     IF  WRK-INP-CODMOD-T700-X NOT EQUAL SPACES
PW9931         INSPECT WRK-INP-CODMOD-T700-X
PW9931                                   REPLACING LEADING ' ' BY '0'
PW9931     END-IF.
PW9931     IF  WRK-INP-CODVAR-T700-X NOT EQUAL SPACES
PW9931         INSPECT WRK-INP-CODVAR-T700-X
PW9931                                   REPLACING LEADING ' ' BY '0'
PW9931     END-IF.
PW9931     IF  WRK-INP-CODCUL-T700-X NOT EQUAL SPACES
PW9931         INSPECT WRK-INP-CODCUL-T700-X
PW9931                                   REPLACING LEADING ' ' BY '0'
PW9931     END-IF.
PW9931     IF  WRK-INP-CODCIC-T700-X NOT EQUAL SPACES
PW9931         INSPECT WRK-INP-CODCIC-T700-X
PW9931                                   REPLACING LEADING ' ' BY '0'
PW9931     END-IF.

PRI499     IF  WRK-INP-CODCLI-T700-X    NOT EQUAL SPACES
PRI499         INSPECT WRK-INP-CODCLI-T700-X
PRI499                                  REPLACING LEADING ' ' BY '0'
PRI499     END-IF.

           IF  INP-AREAF-T700-X         NOT EQUAL SPACES
               INSPECT INP-AREAF-T700-X REPLACING LEADING ' ' BY '0'

           IF  INP-QTDEF-T700-X         NOT EQUAL SPACES
               INSPECT INP-QTDEF-T700-X REPLACING LEADING ' ' BY '0'.

           IF  INP-CAMPO-T700-X         NOT EQUAL SPACES
               INSPECT INP-CAMPO-T700-X REPLACING LEADING ' ' BY '0'.

           INSPECT INP-POSPROC-T700-X   REPLACING LEADING ' ' BY '0'.

           INSPECT INP-AUXDCR-T700-X    REPLACING LEADING ' ' BY '0'.

PW9931     IF  WRK-INP-RECBR-T700-X     NOT EQUAL SPACES
PW9931        INSPECT WRK-INP-RECBR-T700-X REPLACING LEADING ' ' BY '0'
PW9931     END-IF.

      *---------------------------------------------------------------*
       12005-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12006-ACESSAR-MIDD9000   SECTION.
      *---------------------------------------------------------------*

           MOVE 237                    TO LNK-BANCO-FUNC.
           MOVE WRK-COD-DEPTO          TO LNK-DEPTO-FUNC.
           MOVE WRK-COD-USER           TO LNK-COD-FUNC.
           MOVE INP-AGENC-T700         TO LNK-AGENCIA-PESQUISA.
           MOVE ZEROS                  TO LNK-STATUS
                                          LNK-RETORNO.
           MOVE SPACES                 TO LNK-MENSAGEM.
PW6062     MOVE 'RUEC'                 TO LNK-CENTRO-CUSTO.
           MOVE 'MIDD9000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             IO-PCB
                                             ALT-PCB
                                             LNK-AREA-MIDD01.

           EVALUATE TRUE

               WHEN LNK-STATUS         EQUAL 99 AND
                    LNK-RETORNO        EQUAL 99
                    GOBACK
               WHEN LNK-STATUS         EQUAL ZEROS AND
                    LNK-RETORNO        EQUAL ZEROS
                    MOVE 'S'           TO WRK-PERM-OK
               WHEN LNK-STATUS         EQUAL 1 AND
                    LNK-RETORNO        EQUAL 4
                    MOVE 'N'           TO WRK-PERM-OK
PW6062              MOVE WRK-M39       TO LNK-MENSAGEM
               WHEN LNK-STATUS         EQUAL 2 AND
                    LNK-RETORNO        EQUAL 4
                    MOVE 'N'           TO WRK-PERM-OK

           END-EVALUATE.

      *---------------------------------------------------------------*
       12006-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       12015-DELETE-RUECB042 SECTION.
      *---------------------------------------------------------------*

           MOVE    IO-LTERM        TO  CTERM       OF RUECB042.

           EXEC SQL
             DELETE FROM    DB2PRD.NAVEGACAO_RURAL
             WHERE
                   CTERM              = :RUECB042.CTERM
           END-EXEC.

           IF   (( SQLCODE   NOT  =    ZEROS )  AND
                 ( SQLCODE   NOT  =    +100  ))  OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'NAVEGACAO_RURAL   '     TO   ERR-DBD-TAB
                   MOVE 'DELETE  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0013                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM 99999-ROTINA-ERRO.

      *---------------------------------------------------------------*
       12015-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       12030-UPDATE-RUECB042 SECTION.
      *---------------------------------------------------------------*

           MOVE    IO-LTERM            TO  CTERM OF RUECB042.
           MOVE    NAVEGACAO-RURAL     TO  WNAVG-CREDT-RURAL-TEXT.
           MOVE    4000                TO  WNAVG-CREDT-RURAL-LEN.

           EXEC SQL
             UPDATE   DB2PRD.NAVEGACAO_RURAL
               SET
                   WNAVG_CREDT_RURAL  = :RUECB042.WNAVG-CREDT-RURAL
             WHERE
                   CTERM              = :RUECB042.CTERM
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS )   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'NAVEGACAO_RURAL   '     TO   ERR-DBD-TAB
                   MOVE 'UPDATE  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0014                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM 99999-ROTINA-ERRO.

POL097     INITIALIZE  R31-AREA-TOTAL.
POL097     MOVE    1                       TO  R31-CSEQ-REG
POL097     MOVE    INP-DADOS-T700(1:300)   TO  R31-SALVA-RUEC7000
POL097     PERFORM 12031-UPDATE-RUECB048

POL097     INITIALIZE  R31-AREA-TOTAL.
POL097     MOVE    2                       TO  R31-CSEQ-REG
