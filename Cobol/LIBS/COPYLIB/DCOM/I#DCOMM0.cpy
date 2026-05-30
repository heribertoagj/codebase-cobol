      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0M0 - DB2PRD.TTRASI_OPER_COMPL                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7580 - SELECT         - DCOM7581 - FETCH             *
      *    - DCOM7582 - INSERT         - DCOM7583 - UPDATE            *
      *    - DCOM7584 - DELETE                                        *
      *===============================================================*
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
BSI001*===============================================================*
BSI002*                    ALTERACAO - BSI TECNOLOGIA                  *
BSI002*----------------------------------------------------------------*
BSI002*  ANALISTA BSI..:   LUCIANDRA SILVEIRA - BSI TECNOLOGIA         *
BSI002*  ANALISTA DDS..:   MARCO NOGUEIRA     - BRADESCO - GRUPO 39    *
BSI002*  DATA..........:   06/09/2016                                  *
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
BRQ003*  OBJETIVO......:   INCLUIR CAMPO ORIGEM CADASTRO PROGRAMADO.   *
BRQ003*    PROJETO PROGRAMADO CANAIS NETEMPRESA E MOBILE - FASE 3      *
BRQ003*================================================================*
WIP001*                      A L T E R A C A O                         *
WIP001*----------------------------------------------------------------*
WIP001*  ANALISTA......:   FABIO AUGUSTO FINK     - WIPRO              *
WIP001*  ANALISTA DS...:   PAULO ROBERTO SATORATO - BRADESCO / GP.39   *
WIP001*  DATA..........:   16/10/2020                                  *
WIP001*----------------------------------------------------------------*
WIP001*  OBJETIVO......:   INCLUIR NOVOS CAMPOS DE OPTIN.              *
WIP001*================================================================*

       01  M0-ARGUMENTOS-ENTRADA.
           05 M0-COLUNAS-DA-TABELA.
              10 M0-DANO-OPER-DESC     PIC S9(004)V COMP-3.
              10 M0-NSEQ-OPER-DESC     PIC S9(009)V COMP-3.
              10 M0-CVAR-PRODT-RURAL   PIC S9(005)V COMP-3.
              10 M0-CNRO-REFT-BACEN    PIC S9(018)V COMP-3.
BSI001        10 M0-CCESTA-SAFRA-RURAL PIC S9(005)V COMP-3.
BSI001        10 M0-CINDCD-ZNMTO       PIC S9(001)V COMP-3.
BSI002        10 M0-CTPO-BNEFC-RURAL   PIC S9(002)V COMP-3.
BRQ001        10 M0-NBASE-LEGAL-RENEG  PIC S9(005)V COMP-3.
BRQ002        10 M0-CREG-AMBTL-RURAL   PIC  X(041).
BRQ003        10 M0-CORIGE-ANTEC-PGMD  PIC S9(001)V COMP-3.
WIP001        10 M0-CCONTR-LIM-DESC    PIC S9(009)V COMP-3.
WIP001        10 M0-CVRSAO-CONTR-LIM   PIC S9(003)V COMP-3.
WIP001        10 M0-CINDCD-FORMA-LIQDC PIC  X(001).
WIP001        10 M0-CINDCD-UTILZ-LIM   PIC  X(001).
WIP001        10 M0-CINDCD-DEB-PCIAL   PIC  X(001).
WIP001        10 M0-CINDCD-TENTV-DEB   PIC  X(001).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
           05 M0-QTDE-A-DESPREZAR      PIC  9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
           05 M0-INSTRUCAO             PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************

       01  M0-RETORNO.
           05 M0-COD-RETORNO           PIC  9(004).
           05 M0-MENSAGEM              PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
           05 M0-PROXIMO-RESTART       PIC  9(005).
           05 M0-QTDE-RETORNADA        PIC  9(003).
           05 M0-QTDE-TOTAL            PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
           05 M0-OCORRENCIAS    OCCURS 070 TIMES INDEXED BY IND-TM0.
              10 M0RT-DANO-OPER-DESC   PIC S9(004)V COMP-3.
              10 M0RT-NSEQ-OPER-DESC   PIC S9(009)V COMP-3.
              10 M0RT-CVAR-PRODT-RURAL PIC S9(005)V COMP-3.
              10 M0RT-CNRO-REFT-BACEN  PIC S9(018)V COMP-3.
BSI001        10 M0RT-CCESTA-SAFRA-RURAL
BSI001                                 PIC S9(005)V COMP-3.
BSI001        10 M0RT-CINDCD-ZNMTO     PIC S9(001)V COMP-3.
BSI002        10 M0RT-CTPO-BNEFC-RURAL PIC S9(002)V COMP-3.
BRQ001        10 M0RT-NBASE-LEGAL-RENEG
BRQ001                                 PIC S9(005)V COMP-3.
BRQ002        10 M0RT-CREG-AMBTL-RURAL PIC  X(041).
BRQ003        10 M0RT-CORIGE-ANTEC-PGMD
BRQ003                                 PIC S9(001)V COMP-3.
WIP001        10 M0RT-CCONTR-LIM-DESC  PIC S9(009)V COMP-3.
WIP001        10 M0RT-CVRSAO-CONTR-LIM PIC S9(003)V COMP-3.
WIP001        10 M0RT-CINDCD-FORMA-LIQDC
WIP001                                 PIC  X(001).
WIP001        10 M0RT-CINDCD-UTILZ-LIM PIC  X(001).
WIP001        10 M0RT-CINDCD-DEB-PCIAL PIC  X(001).
WIP001        10 M0RT-CINDCD-TENTV-DEB PIC  X(001).
