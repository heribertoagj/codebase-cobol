      ******************************************************************
      * SISTEMA : DCOM - I#DCOMT5                                      *
      * TAMANHO : 0400 BYTES                                           *
      * ARQUIVO : HISTORICO OPERACAO COMPLEMENTAR                      *
      * INCLUDE DA TABELA DCOMB0L6 - DB2PRD.THIST_OPER_COMPL           *
      ******************************************************************
BSI001*                    ALTERACAO - BSI TECNOLOGIA                  *
BSI001*----------------------------------------------------------------*
BSI001*  ANALISTA BSI..:   LUCIANDRA SILVEIRA - BSI TECNOLOGIA         *
BSI001*  ANALISTA DDS..:   RICARDO JAMMAL     - BRADESCO - GRUPO 39    *
BSI001*  DATA..........:   24/09/2015                                  *
BSI001*----------------------------------------------------------------*
BSI001*  OBJETIVO......: - INCLUIR TRATAMENTO PARA OS NOVOS CAMPOS CES *
BSI001*                    TA DE SAFRAS E ZONEAMENTO.                  *
BSI001*                    BSI - 15/0083 - ADEQUAR EMPREENDIMENTO RE-  *
BSI001*                    GRAS BACEN.                                 *
BSI001*================================================================*
BSI002*                    ALTERACAO - BSI TECNOLOGIA                  *
BSI002*----------------------------------------------------------------*
BSI002*  ANALISTA BSI..:   FABIO FINK         - BSI TECNOLOGIA         *
BSI002*  ANALISTA DDS..:   MARCO NOGUEIRA     - BRADESCO / GP.39       *
BSI002*  DATA..........:   12/09/2016                                  *
BSI002*----------------------------------------------------------------*
BSI002*  OBJETIVO......: - INCLUIR CAMPO TIPO BENEFICIARIO EMITENTE.   *
BSI002*                    BSI - 16/0244 - SICOR 4.08.                 *
BSI002*================================================================*
BRQ001*          ALTERACAO - BRQ SOLUCOES EM INFORMATICA S.A.          *
BRQ001*----------------------------------------------------------------*
BRQ001*  ANALISTA BRQ..:   FABIO AUGUSTO FINK      -  BRQ              *
BRQ001*  ANALISTA DDS..:   MARCO AURELIO NOGUEIRA  -  BRADESCO / GP.39 *
BRQ001*  DATA..........:   23/11/2017                                  *
BRQ001*----------------------------------------------------------------*
BRQ001*  OBJETIVO......:   INCLUIR CAMPO CODIGO BASE LEGAL RENEGOCIACAO*
BRQ001*                    SICOR 4.10                                  *
BRQ001*================================================================*
BRQ002*          ALTERACAO - BRQ SOLUCOES EM INFORMATICA S.A.          *
BRQ002*----------------------------------------------------------------*
BRQ002*  ANALISTA BRQ..:   FABIO AUGUSTO FINK      -  BRQ              *
BRQ002*  ANALISTA DDS..:   MARCO AURELIO NOGUEIRA  -  BRADESCO / GP.39 *
BRQ002*  DATA..........:   05/12/2017                                  *
BRQ002*----------------------------------------------------------------*
BRQ002*  OBJETIVO......:   SICOR - INCLUIR CAMPO 'NUMERO CAR'.         *
BRQ002*================================================================*
BRQ003*          ALTERACAO - BRQ SOLUCOES EM INFORMATICA S.A.          *
BRQ003*----------------------------------------------------------------*
BRQ003*  ANALISTA BRQ..:   FERNANDO LUIZ DE SANTI                      *
BRQ003*  ANALISTA DS...:   RICARDO JAMMAL       -  GRUPO 39            *
BRQ003*  DATA..........:   30/07/2018                                  *
BRQ003*----------------------------------------------------------------*
BRQ003*  OBJETIVO......:   INCLUIR CAMPO ORIGEM DE CADASTRAMENTO DO    *
BRQ003*    PROGRAMADO - CORIGE-ANTEC-PGMD.                             *
BRQ003*    PROJETO PROGRAMADO CANAIS NETEMPRESA E MOBILE - FASE 3      *
BRQ003*================================================================*
WIP001*                      A L T E R A C A O                         *
WIP001*----------------------------------------------------------------*
WIP001*  ANALISTA......:   FERNANDO LUIZ DE SANTI - WIPRO              *
WIP001*  ANALISTA DS...:   PAULO ROBERTO          - GRUPO 39           *
WIP001*  DATA..........:   16/10/2020                                  *
WIP001*----------------------------------------------------------------*
WIP001*  OBJETIVO......:   INCLUIR NOVOS CAMPOS DE OPTIN.              *
WIP001*================================================================*

       01  T5-L6-REGISTRO.
 01 03     05 T5-L6-DANO-OPER-DESC     PIC S9(004)V COMP-3.
 04 05     05 T5-L6-NSEQ-OPER-DESC     PIC S9(009)V COMP-3.
 09 26     05 T5-L6-HULT-ATULZ         PIC  X(026).
 35 03     05 T5-L6-CVAR-PRODT-RURAL   PIC S9(005)V COMP-3.
 38 10     05 T5-L6-CNRO-REFT-BACEN    PIC S9(018)V COMP-3.
 48 03     05 T5-L6-CCESTA-SAFRA-RURAL PIC S9(005)V COMP-3.
 51 01     05 T5-L6-CINDCD-ZNMTO       PIC S9(001)V COMP-3.
 52 02     05 T5-L6-CTPO-BNEFC-RURAL   PIC S9(002)V COMP-3.
 54 03     05 T5-L6-NBASE-LEGAL-RENEG  PIC S9(005)V COMP-3.
 57 41     05 T5-L6-CREG-AMBTL-RURAL   PIC  X(041).
 98 01     05 T5-L6-CORIGE-ANTEC-PGMD  PIC S9(001)V COMP-3.
 99 05     05 T5-L6-CCONTR-LIM-DESC    PIC S9(009)V COMP-3.
104 02     05 T5-L6-CVRSAO-CONTR-LIM   PIC S9(003)V COMP-3.
106 01     05 T5-L6-CINDCD-FORMA-LIQDC PIC  X(001).
107 01     05 T5-L6-CINDCD-UTILZ-LIM   PIC  X(001).
108 01     05 T5-L6-CINDCD-DEB-PCIAL   PIC  X(001).
109 01     05 T5-L6-CINDCD-TENTV-DEB   PIC  X(001).
110291     05 FILLER                   PIC  X(291).
