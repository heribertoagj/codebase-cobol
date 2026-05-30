      *---------------------------------------------------------------*
      *              S O N D A      P R O C W O R K                   *
      *---------------------------------------------------------------*
      * INCLUDE.....: I#RUEC58 - PARA MODULO RUEC8800                 *
      * GERACAO.....: JUNHO/2012                                      *
      *---------------------------------------------------------------*
      * OBJETIVO....: AREA DE COMUNICACAO PARA ENQUADRAMENTO DE       *
      *               OPERACOES RURAIS DE ACORDO COM AS REGRAS DO     *
      *               BANCO CENTRAL - RECOR                           *
      *               CHAMADO POR RUEC7000, RUEC8000, MASSIVO         *
      *---------------------------------------------------------------*
      * PROJETO ....: REFORMULACAO MENSAGERIA BACEN - RECOR           *
      *---------------------------------------------------------------*
WIP001* ALTERACAO...........: ALTERACAO AMPO SICOR 5.05               *
-     *                       COD-SIST-PRODC                          *
WIP001* GERACAO.............: 10/2022                                 *
PRI499*================================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
STE001*================================================================*
.     *                ALTERACAO - STEFANINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ANALISTA REPONSAVEL: VICTORINO                                 *
.     * DATA...............: MAIO / 2024                               *
.     * OBJETIVOS..........: INCLUSAO DE OUTRAS PRATICAS DE MANEJO E   *
.     *                      CAMPOS DO CAR                             *
.     *                      SICOR 5.08                                *
.     * TAMANHO............: 949                                       *
STE001*================================================================*
      *****************************************************************
       01  RUEC58-AREA.
           05  RUEC58-ENVIO.
             10  RUEC58-PGM-CHAMADOR         PIC  X(008).
             10  RUEC58-AGENCIA              PIC  9(005).
             10  RUEC58-CEDULA               PIC  9(009).
             10  RUEC58-CONTRATO             PIC  9(009).
             10  RUEC58-FINALIDADE           PIC  9(003).
             10  RUEC58-ATIVIDADE            PIC  9(003).
             10  RUEC58-MODALIDADE           PIC  9(005).
             10  RUEC58-PRODUTO              PIC  9(003).
             10  RUEC58-VARIEDADE            PIC  9(005).
             10  RUEC58-CESTA-SAFRA          PIC  9(005).
             10  RUEC58-ZONEAMENTO           PIC  9(001).
             10  RUEC58-CONSORCIO            PIC  9(001).
             10  RUEC58-TIPO-AGROP           PIC  9(005).
             10  RUEC58-TIPO-INTEG           PIC  9(005).
             10  RUEC58-GRAO-SEMENTE         PIC  9(001).
             10  RUEC58-TIPO-IRRIG           PIC  9(005).
             10  RUEC58-TIPO-CULTIVO         PIC  9(005).
             10  RUEC58-CICLO-PROD           PIC  9(005).
PRI499       10  RUEC58-CLIMA-PROD           PIC  9(002).
             10  RUEC58-DCDULA               PIC  X(010).
STE001       10  RUEC58-COUTRO-TRATO-SOLO    PIC  9(005).
STE001       10  RUEC58-PREDC-JURO-AMBTL     PIC  9(003)V99.
STE001       10  RUEC58-CINDCD-CONFM-FLORS   PIC  X(001).
STE001       10  RUEC58-CINDCD-CONFM-IBAMA   PIC  X(001).
STE001       10  RUEC58-CINDCD-CONFM-CSVAO   PIC  X(001).
STE001       10  RUEC58-RREGLZ-IBAMA         PIC  X(300).
STEFAB       10  RUEC58-IND-RCONFDD-IM-RURAL PIC  X(001).
STEFAB       10  RUEC58-TP-CONFDD-OTR        PIC  9(005).
STEFAB       10  RUEC58-CNPJ-BASE-EXIGLDD    PIC  9(008).
STEFAB       10  FILLER                      PIC  X(016).
      **
      *************** AREA DE DADOS DE RETORNO ***************
      **
           05  RUEC58-RETORNO.
             10  RUEC58-COD-RET              PIC  9(002).
      **
      **=======  00 - ENQUADRAMENTO EFETUADO
      **=======  01 - DADOS ENVIADOS INCONSISTENTES
      **=======  02 - EMPREENDIMENTO INEXISTENTE
      **=======  03 - SISTEMA DE PRODUCAO INEXISTENTE
      **=======  04 - EMPREENDIMENTO FORA DA VIGENCIA
      **=======  05 - CENTRO DE CUSTO INCOMPATIVEL
      **=======  99 - PROBLEMAS DB2
      **
             10  RUEC58-CUND-MEDD-FINCD      PIC  9(003).
             10  RUEC58-SIGL-MEDD-FINCD      PIC  X(003).
             10  RUEC58-IUND-MEDD-FINCD      PIC  X(040).
             10  RUEC58-CUND-MEDD-PROVV      PIC  9(003).
             10  RUEC58-SIGL-MEDD-PROVV      PIC  X(003).
             10  RUEC58-IUND-MEDD-PROVV      PIC  X(040).
             10  RUEC58-DINIC-VGCIA          PIC  X(010).
             10  RUEC58-DFIM-VGCIA           PIC  X(010).
             10  RUEC58-EMPREEND             PIC  X(014).
STE001       10  RUEC58-SIST-PROD            PIC  X(013).
             10  RUEC58-MENSAGEM             PIC  X(079).
STEFAB       10  RUEC58-TP-CONFDD-OTR-N2     PIC  9(002).
WIP001       10  FILLER                      PIC  X(046).
             10  RUEC58-SQLCA                PIC  X(136).
             10  RUEC58-ERRO-AREA            PIC  X(107).
