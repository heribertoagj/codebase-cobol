      *---------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *---------------------------------------------------------------*
       PROGRAM-ID.    RUEC7200.
      *REMARKS.
      *       ***********************  RUEC7200  ***********************
      *       *                                                        *
      *       * PROGRAMADOR         :    CASSIO LUIS                   *
      *       * ANALISTA            :    CASSIO LUIS                   *
      *       * DATA                :    FEVEREIRO\03                  *
      *       *                                                        *
      *       * SISTEMA             :    RUEC                          *
      *       *                                                        *
      *       * OBJETIVO            :    GRAVAR DADOS TABELA RESPONSA- *
      *       *                          BILIDADE RURAL.               *
      *       *                                                        *
      *       * ARQUIVOS UTILIZADOS :                                  *
      *       *                                                        *
      *       *      RUECV007  - DB2PRD.V01CEDULA_TEMPR                *
      *       *      RUECB046  - DB2PRD.RESP_CONTR_RURAL               *
ACT137*       *                                                        *
ACT137*       * POOLS               :                                  *
ACT137*       *                                                        *
ACT137*       *      POOL0081  - ALOC./DEL. DINAMICAMENTE OUTRO MOD    *
ACT137*       *      POOL0492  - CONSISTE VALORES EDITADOS (IMS)       *
ACT137*       *      POOL0530  - FORNECE DADOS SOBRE BANCO             *
ACT137*       *      POOL0660  - COMPRIME AREA DE I/O                  *
ACT137*       *      POOL5000  - MODULO ON-LINE                        *
ACT137*       *      POOL7100  - TRATAMENTO DE ERROS DE APLICACAO      *
ACT137*       *      POOL7600  - DATA E HORA DO SISTEMA                *
      *       *                                                        *
      *       *                                                        *
      *       *                                                        *
      *       * TELAS               :                                  *
      *       *                                                        *
      *       *      RUECT720  -
      *       *                                                        *
      *       * PROGRAMA  CHAMADOR  :                                  *
      *       *                                                        *
      *       *      RUEC7190  -
      *       *                                                        *
      *       *                                                        *
      *       *      RUEC7020  -
ACT137*       *                                                        *
ACT137*       *========================================================*
ACT137*       * ALTERACAO.......: ACT 07/0137-02    2007/0828/5-00     *
ACT137*       * PROGRAMADOR.....: RYUJI      ALTRAN                    *
ACT137*       * ANALISTA........: MARINA     ALTRAN                    *
ACT137*       * DATA............: 03/09/2007                           *
ACT137*       * PROMOVER ALTERACOES NOS DOCUMENTOS, CEDULAS E PROPOSTA *
ACT137*       *          DE ORCAMENTO                                  *
ACT137*       * COMPLEM: NAVEGAR PARA BANCOS NO PF9 EM VEZ DE FINALI-  *
ACT137*       *          DADE (RUEC6900 EM VEZ DE RUEC7550)            *
ACT137*       *          CEDULA, FINALIDADE E DATAS COM QUALQUER       *
ACT137*       *          CONTEUDO E CHECAR DUPLICIDADE DE LINHAS       *
      *       **********************************************************
HE0410*       * ALTERACAO.......: PWT 09/2678       2009/0855/5-01     *
HE0410*       * ANALISTA........: HEYDI MARIGO - SONDA PROCWORK        *
HE0410*       * DATA............: ABRIL/2010                           *
HE0410*       * OBJETIVO........: INCLUSAO DE TRATAMENTO PARA          *
HE0410*       *                   SIMULTANEIDADE.                      *
      *       **********************************************************
BR1217*       * ANALISTA........: RENATA AMORIM LEGARIO - BRQ          *
BR1217*       * DATA............: DEZEMBRO/2017                        *
BR1217*       * OBJETIVO........: OBTER RESPONSABILIDADE NO RCOR       *
BR1217*       *                                                        *
      *       **********************************************************
BRQ001*----------------------------------------------------------------*
BRQ001*                            ALTERACAO                           *
BRQ001*----------------------------------------------------------------*
BRQ001*                                                                *
BRQ001*      PROGRAMADOR  : GABRIEL SALIM         - BRQ                *
BRQ001*      ANALISTA     : RODOLFO VIEIRA        - BRQ                *
BRQ001*      DATA         : JUNHO/2021                                 *
BRQ001*                                                                *
BRQ001*      OBJETIVO     : INCLUIR NOVOS CAMPOS DO CATALOGO SICOR     *
BRQ001*                     V5.02 NA MENSAGEM COR0006R1                *
BRQ001*                                                                *
BRQ001*      REFERENCIA   : BRQ001                                     *
BRQ001*                                                                *
BRQ001*================================================================*
7C0625*================================================================*
.     * ANALISTA REPONSAVEL: EDSON/NIKSON                              *
.     * OBJETIVOS..........: FLASH - AGRO7-441 / AGRO7-442 / AGRO7-435 *
.     *                      ALTERAR PARA ENVIAR A SOLICITACAO DAS     *
.     *                      RESPONSABILIDADES DOS COOPERADOS (ATUAL)  *
.     *                      E TAMBEM DE TODOS OS CLIENTES             *
7C0625*================================================================*

      /---------------------------------------------------------------*
       ENVIRONMENT    DIVISION.
      *---------------------------------------------------------------*

       CONFIGURATION           SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      /---------------------------------------------------------------*
       DATA           DIVISION.
      *---------------------------------------------------------------*

      /---------------------------------------------------------------*
       WORKING-STORAGE         SECTION.
      *---------------------------------------------------------------*
ACT137 77  WRK-VFINAN-TELA             PIC S9(13)V99 COMP-3 VALUE ZEROS.
ACT137 77  WRK-VFINAN-BCO              PIC S9(13)V99 COMP-3 VALUE ZEROS.
ACT137 77  WRK-VFINAN-BRD              PIC S9(13)V99 COMP-3 VALUE ZEROS.
ACT137 77  WRK-VFINAN-TOT              PIC S9(13)V99 COMP-3 VALUE ZEROS.
HE0410 77  WRK-INCONS                  PIC  X(01)      VALUE 'N'.
       77  WRK-CHNG                    PIC  X(04)      VALUE 'CHNG'.
       77  WRK-ERRO                    PIC  X(79)      VALUE SPACES.
       77  WRK-IND                     PIC  9(02) COMP-3 VALUE ZEROS.
       77  IND-1                       PIC S9(05) COMP-3 VALUE ZEROS.
       77  IND-2                       PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-PAG-HEX                 PIC  9(03) COMP-3 VALUE ZEROS.
       77  WRK-PAG                     PIC  9(03) COMP-3 VALUE ZEROS.
       77  WRK-CONT                    PIC  9(03)      VALUE ZEROS.
       77  WRK-CONTA-INCON             PIC  9(02)      VALUE ZEROS.
       77  IND-HEX                     PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-ACUM-OCORR              PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-COUNT                   PIC S9(05) COMP   VALUE ZEROS.
       77  WRK-FIM-CSR-RUECB046        PIC  X(01)        VALUE SPACES.
BRQ001 77  WRK-OCOR                    PIC 9(02)         VALUE 03.
RLOS   01  WRK-REF-BACEN-X             PIC X(011)        VALUE SPACES.
.      01  FILLER                      REDEFINES  WRK-REF-BACEN-X.

BRQ001 01  WRK-CNPJERO-BASE-CS         PIC +9(09)      VALUE ZEROS.
BRQ001 01  WRK-CNPJERO-BASE-TELA       REDEFINES WRK-CNPJERO-BASE-CS.
BRQ001     03  FILLER                  PIC  X(01).
BRQ001     03  FILLER                  PIC  9(01).
BRQ001     03  WRK-CNPJERO-BASE-TELA1  PIC  X(02).
BRQ001     03  WRK-CNPJERO-BASE-TELA2  PIC  X(03).
BRQ001     03  WRK-CNPJERO-BASE-TELA3  PIC  X(03).
BRQ001
BRQ001 01  WRK-CPFERO-BASE-CS          PIC +9(09)      VALUE ZEROS.
BRQ001 01  WRK-CPFERO-BASE-TELA        REDEFINES WRK-CPFERO-BASE-CS.
BRQ001     03  FILLER                  PIC  X(01).
BRQ001     03  WRK-CPFERO-BASE-TELA1   PIC  X(03).
BRQ001     03  WRK-CPFERO-BASE-TELA2   PIC  X(03).
BRQ001     03  WRK-CPFERO-BASE-TELA3   PIC  X(03).
BRQ001
BRQ001 01  WRK-CNPJERO-FORM.
BRQ001     03  WRK-CNPJERO-BASE1       PIC  X(02)      VALUE SPACES.
BRQ001     03  WRK-CNPJERO-PT1         PIC  X(01)      VALUE '.'.
BRQ001     03  WRK-CNPJERO-BASE2       PIC  X(03)      VALUE SPACES.
BRQ001     03  WRK-CNPJERO-PT2         PIC  X(01)      VALUE '.'.
BRQ001     03  WRK-CNPJERO-PT3         PIC  X(01)      VALUE '/'.
BRQ001     03  WRK-CNPJERO-FILI        PIC  X(04)      VALUE SPACES.
BRQ001     03  WRK-CNPJERO-FILI-N REDEFINES WRK-CNPJERO-FILI
BRQ001                                 PIC  9(04).
BRQ001     03  WRK-CNPJERO-PT4         PIC  X(01)      VALUE '-'.
BRQ001     03  WRK-CNPJERO-CTRL        PIC  X(02)      VALUE SPACES.
BRQ001     03  WRK-CNPJERO-CTRL-N REDEFINES WRK-CNPJERO-CTRL
BRQ001                                 PIC  9(02).
BRQ001
BRQ001 01  WRK-CPFERO-FORM.
BRQ001     03  WRK-CPFERO-FILLER1      PIC  X(04)      VALUE SPACES.
BRQ001     03  WRK-CPFERO-BASE1        PIC  X(03)      VALUE SPACES.
BRQ001     03  WRK-CPFERO-PT1          PIC  X(01)      VALUE '.'.
BRQ001     03  WRK-CPFERO-BASE2        PIC  X(03)      VALUE SPACES.
BRQ001     03  WRK-CPFERO-PT2          PIC  X(01)      VALUE '.'.
BRQ001     03  WRK-CPFERO-BASE3        PIC  X(03)      VALUE SPACES.
BRQ001     03  WRK-CPFERO-PT4          PIC  X(01)      VALUE '-'.
BRQ001     03  WRK-CPFERO-CTRL         PIC  X(02)      VALUE SPACES.
BRQ001     03  WRK-CPFERO-CTRL-N REDEFINES WRK-CPFERO-CTRL
BRQ001                                 PIC  9(02).

BR1811 01  WRK-TIMESTAMP-1S            PIC X(26)         VALUE SPACES.

BR1811 01  WRK-AREA-POOL7600.
BR1811     03  WRK-PO-JULIANA          PIC 9(05) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-AAMMDD           PIC 9(07) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-AAAAMMDD         PIC 9(09) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-HHMMSS           PIC 9(07) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-HHMMSSMMMMMM     PIC 9(13) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-TIMESTAMP        PIC X(20)         VALUE SPACES.
BR1811
      *--- AREA POOL7600 ----------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC 9(05) COMP-3.
           03  WRK-DT-AAMMDD           PIC 9(07) COMP-3.
           03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3.
           03  WRK-TI-HHMMSS           PIC 9(07) COMP-3.
           03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3.
           03  WRK-TIMESTAMP           PIC X(20).


           05  WRK-VALO-ENT          PIC  X(14).
           05  WRK-VALO-SAI          PIC  X(11).
           05  WRK-VALO-SAI-N  REDEFINES  WRK-VALO-SAI
                                     PIC  9(11).
           05  WRK-VALO-SAI-R  REDEFINES  WRK-VALO-SAI
                                     PIC  9(09)V99.
           05  WRK-VALO-TAM          PIC  9(08)  VALUE 14110902.
           05  WRK-EDIT-VALO         PIC  ZZZ.ZZZ.ZZ9,99.

      ******************************************************
      * AREAS AUXILIARES PARA SEREM UTILIZADAS NO PROGRAMA *
      ******************************************************

       01  WRK-AREAS-AUXILIARES.
         05  WRK-EMPREEND-RECOR      PIC X(014)      VALUE SPACES.
         05  FILLER  REDEFINES  WRK-EMPREEND-RECOR.
             07  WRK-ATIV-RECOR      PIC 9(001).
             07  WRK-FINAL-RECOR     PIC 9(001).
             07  WRK-MODALID-RECOR   PIC X(002).
             07  WRK-PRODUTO-RECOR   PIC X(004).
             07  WRK-VARIED-RECOR    PIC X(003).
             07  WRK-CESTA-RECOR     PIC X(001).
             07  WRK-ZONEAM-RECOR    PIC X(001).

      *--------------------------------*
      * DEFINICAO DE AREAS DA POOL1000 *
      *--------------------------------*

         05  WRK-FUNCAO               PIC X(004)   VALUE SPACES.
         05  WRK-MODNAME              PIC X(008)   VALUE SPACES.
         05  WRK-NOVA-OPCAO           PIC 9(002)   VALUE ZEROS.
         05  WRK-COD-USER             PIC 9(007)   VALUE ZEROS.
         05  WRK-OPCAO-ATUAL          PIC 9(002)   VALUE ZEROS.
BR1217   05  WRK-CMSGEM-RECOR         PIC X(009)   VALUE SPACES.
BR1217   05  WRK-CMSGEM-RECOR-E       PIC X(009)   VALUE SPACES.
BR1217   05  WRK-CMSGEM-RECOR-NULL    PIC S9(004) COMP VALUE ZEROS.

         05  WRK-MENSAGEM.
             07  WRK-LL               PIC X(02).
             07  WRK-ZZ               PIC X(02).
             07  WRK-TRANSACAO        PIC X(08).
             07  FILLER               PIC X(1988).
      /---------------------------------------------------------------*
      * DEFINICAO DA AREA PARA RET-CODE DIFERENTE DE ZEROS EM MODULOS *
      *---------------------------------------------------------------*

       01  WRK-ERRO-ROTINA.
           03 WRK-MENSAGEM-ERRO        PIC  X(45).
           03 FILLER                   PIC  X(14) VALUE
              ' RET.COD = '.
           03 WRK-RETURN-ERRO          PIC  9(02)BB.
           03 FILLER                   PIC  X(08) VALUE 'LOCAL = '.
           03 WRK-LOCAL                PIC  9(02) VALUE ZEROS.

      *---------------------------------------------------------------*
      *  AREA RECEBIDA                                                *
      *---------------------------------------------------------------*

       01  COMU-AREA.
           03 COMU-LL                  PIC S9(04) COMP VALUE +66.
           03 COMU-ZZ                  PIC S9(04) COMP VALUE ZEROS.
           03 COMU-TRANS               PIC  X(15)      VALUE 'RUEC7000'.
           03 COMU-SENHA               PIC  X(37).
           03 COMU-CTERM               PIC  X(08)      VALUE SPACES.

      *---------------------------------------------------------------*
      *  AREA NAVEGADA PARA OUTROS PROGRAMAS                          *
      *---------------------------------------------------------------*

       01  AREA-NAVEGA.
           03 AREA-LL             PIC S9(04) COMP VALUE +147.
           03 AREA-ZZ             PIC S9(04) COMP VALUE ZEROS.
           03 AREA-TRANSACAO      PIC  X(15) VALUE 'RUEC7200'.
           03 AREA-SENHA          PIC  X(37).
           03 AREA-OPCAO          PIC  9(02).
           03 AREA-CTERM          PIC  X(08).
           03 AREA-STATUS         PIC  X(02).
           03 AREA-MENSAGEM       PIC  X(79).


      *---------------------------------------------------------------*
      *          AREAS RESERVADAS PARA UTILIZACAO DAS POOLS          *
      *---------------------------------------------------------------*


-INC I#RUEC00

-INC I#POOLB1

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
              10 WRK-IDENTIF                 PIC  9(015) VALUE ZEROS.
              10 WRK-CGC                     REDEFINES WRK-IDENTIF.
                  15  WRK-NUMCGC             PIC  9(009).
                  15  WRK-CTRL-CGC           PIC  9(002).
              10  WRK-CPF                    REDEFINES WRK-IDENTIF.
                  15  WRK-NUMCPF             PIC  9(009).
                  15  WRK-CTRL-CPF           PIC  9(002).
                  15  WRK-FILLER             PIC  9(004).
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


      /---------------------------------------------------------------*
      *---------------------------------------------------------------*

       01  WRK-COD-USUARIO.
           02  WRK-COD-USUARION        PIC  9(07).

       01  WRK-COD-DEPTO-X.
           02  WRK-COD-DEPTO           PIC  9(06)     VALUE ZEROS.
           02  WRK-COD-DEPTO-R         REDEFINES      WRK-COD-DEPTO.
               03  WRK-COD-DEPTO-1     PIC  9(02).
               03  WRK-COD-DEPTO-2     PIC  9(04).

      /---------------------------------------------------------------*
      *               DEFINICAO DE WRK-ATRS                          *
      *---------------------------------------------------------------*

      *    WRK-ATR  NUM., PRE-MOD., POS.CURSOR, BRILHO, OVERRIDE.

       01  WRK-49369                   PIC S9(08) COMP VALUE +49369.

      *    WRK-ATR  ALFA, PRE-MOD., POS.CURSOR, BRILHO, OVERRIDE.


      *    WRK-ATR  NUME, PRE-MOD., POS.CURSOR.

       01  WRK-49361                   PIC S9(08) COMP VALUE +49361.

      *    WRK-ATR  NUME, PRE-MOD., PROTEGIDO, OVERRIDE.

       01  WRK-241                     PIC S9(08) COMP VALUE +241.

      *    WRK-ATR  ALFA, PRE-MOD., PROTEGIDO, OVERRIDE.

       01  WRK-161                     PIC S9(08) COMP VALUE +161.

      /---------------------------------------------------------------*
      * DEFINICAO DA AREA PARA RET-CODE DIFERENTE DE ZEROS (POOL0660) *
      *---------------------------------------------------------------*

       01  WRK-ERRO-POOL0660.
           03 FILLER                   PIC  X(48) VALUE
              '* RETURN-CODE DIFERENTE DE ZEROS NA POOL0660 *'.
           03 FILLER                   PIC  X(13) VALUE
           03 WRK-RETURN-CODE          PIC  9(02)BB.
           03 FILLER                   PIC  X(08) VALUE 'LOCAL = '.
           03 WRK-LOCAL-ERRO           PIC  9(02) VALUE ZEROS.

      *---------------------------------------------------------------*
      *  AREA RESERVADA PARA STARTAR OS PROGRAMAS                     *
      *---------------------------------------------------------------*

       01  START-AREA.
           03 START-LL                      PIC S9(04) COMP VALUE +58.
           03 START-ZZ                      PIC S9(04) COMP VALUE ZEROS.
           03 START-TRANS                   PIC  X(15).
           03 START-SENHA                   PIC  X(37).
           03 START-OPCAO                   PIC  9(02).

      *---------------------------------------------------------------*
      *            TELA   RUECT720      -        INPUT                *
      *---------------------------------------------------------------*

       01  AREA-INPUT.
           03 FILLER                        PIC  X(18).
           03 IN-NAVEGA                     PIC  X(37).
           03 IN-OPCAO                      PIC  9(02).
           03 IN-CEDULA                     PIC  9(09).
           03 IN-FASE                       PIC  9(01).
           03 IN-CTERM                      PIC  X(08).
           03 WRK-IN-PAG                    PIC  9(03) VALUE ZEROS.
           03 IN-TIMES                      PIC  X(26).
           03 IN-AGENC                      PIC  9(05).
BRQ001     03 IN-LINHA   OCCURS   03 TIMES.
BR1217        05 IN-BANCO                   PIC  X(08).
BR1217        05 WRK-IN-RFBACEN             PIC  X(11).
BR1217        05 WRK-IN-FNALD               PIC  X(07).
BR1217        05 WRK-IN-ATIV                PIC  X(03).
BR1217        05 WRK-IN-DTEM                PIC  X(10).
BR1217        05 WRK-IN-DTVCTO              PIC  X(10).
BR1217        05 IN-VALOR-ALF.
BR1217           07 IN-VALOR                PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
BR1217        05 WRK-IN-FONTE               PIC  X(30) VALUE SPACES.
RLOS          05 WRK-IN-CNPJ-PRO-ALF.
.                07 WRK-IN-CNPJ-PRO         PIC  ZZ.ZZZ.ZZZ.
.             05 WRK-IN-RFBACEN-PRO         PIC  X(11).
RLOS             07 WRK-IN-VALOR-PRO        PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
BRQ001        05 WRK-IN-CNPJ-ENT-RESP-ORIG  PIC  X(18) VALUE SPACES.
BRQ001        05 WRK-IN-CNPJ-PARTC          PIC  X(18) VALUE SPACES.
BR1217     03 WRK-IN-CTRBAC                 PIC  X(20) VALUE SPACES.
ACT137     03 IN-TOTBA-ALF.
ACT137        05 IN-TOTBA                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137     03 IN-TOTBR-ALF.
ACT137        05 IN-TOTBR                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137     03 IN-TOTRE-ALF.
ACT137        05 IN-TOTRE                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
           03 IN-LITERAL                    PIC  X(38).
           03 IN-RESPTA                     PIC  X(01).
           03 IN-POSI.
               05  IN-LIN                   PIC  S9(04)  COMP.
               05  IN-COL                   PIC  S9(04)  COMP.

      *---------------------------------------------------------------*
      *            TELA   RUECT720      -        OUTPUT               *
      *---------------------------------------------------------------*

       01  AREA-OUTPUT.
BRQ001     03 OU-LL                         PIC S9(04) COMP VALUE +0833.
           03 OU-ZZ                         PIC  9(04) COMP VALUE 0.
           03 OU-NAVEGA                     PIC  X(37).
           03 OU-OPCAO                      PIC  9(02).
           03 OU-DATA.
              05 OU-DIA-T720                PIC  9(02).
              05 OU-BARRA1-T720             PIC  X(01).
              05 OU-MES-T720                PIC  9(02).
              05 OU-BARRA2-T720             PIC  X(01).
              05 OU-ANO-T720                PIC  9(04).
           03 OU-CEDULA                     PIC  9(09).
           03 OU-FASE                       PIC  9(01).
           03 OU-CTERM                      PIC  X(08).
           03 WRK-OU-PAG                    PIC  9(03) VALUE ZEROS.
           03 OU-TIMES                      PIC  X(26).
           03 OU-AGENC                      PIC  9(05).
BRQ001     03 OU-LINHA   OCCURS   03 TIMES.
              05 OU-BANCO                   PIC  X(08).
RE1117        05 WRK-OU-RFBACEN             PIC  X(11).
RE1117        05 WRK-OU-FNALD               PIC  X(07).
RE1117        05 WRK-OU-ATIV                PIC  X(03).
RE1117        05 WRK-OU-DTVCTO              PIC  X(10).
              05 OU-VALOR-ALF.
                 07 OU-VALOR                PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
              05 WRK-OU-FONTE               PIC  X(30) VALUE SPACES.
RLOS          05 WRK-OU-CNPJ-PRO-ALF.
.                07 WRK-OU-CNPJ-PRO         PIC  ZZ.ZZZ.ZZZ.
.             05 WRK-OU-RFBACEN-PRO         PIC  X(11).
.             05 WRK-OU-VALOR-PRO-ALF.
RLOS             07 WRK-OU-VALOR-PRO        PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
BRQ001        05 WRK-OU-CNPJ-ENT-RESP-ORIG  PIC  X(18) VALUE SPACES.
BRQ001        05 WRK-OU-CNPJ-PARTC          PIC  X(18) VALUE SPACES.
           03 WRK-OU-CTRBAC                 PIC  X(20) VALUE SPACES.
ACT137     03 OU-TOTBA-ALF.
ACT137        05 OU-TOTBA                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137     03 OU-TOTBR-ALF.
ACT137        05 OU-TOTBR                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137     03 OU-TOTRE-ALF.
ACT137        05 OU-TOTRE                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
           03 OU-LITERAL                    PIC  X(38).
           03 OU-RESPTA-ATR                 PIC S9(04) COMP.
           03 OU-RESPTA                     PIC  X(01).

      /---------------------------------------------------------------*
      *  AREA DA POOL0660 COM MASCARA DA TELA RUECT720 - SEGMENTO 00  *
      *---------------------------------------------------------------*
       01  660-RUECT720-SEG00.
BRQ001* ***05  660-LL-AREA-T720        PIC  9(04)  COMP  VALUE 0146.
BRQ001* ***05  660-LL-MENSAGEM-T720    PIC  9(04)  COMP  VALUE 1005.
BRQ001     05  660-LL-AREA-T720        PIC  9(04)  COMP  VALUE 0114.
BRQ001     05  660-LL-MENSAGEM-T720    PIC  9(04)  COMP  VALUE 0833.
           05  660-SENHA-T720          PIC  9(04)  COMP  VALUE 0037.
           05  660-OPCAO-T720          PIC  9(04)  COMP  VALUE 0002.
           05  660-DATA-T720           PIC  9(04)  COMP  VALUE 0010.
           05  660-CEDULA-T720         PIC  9(04)  COMP  VALUE 0009.
           05  660-FASE-T720           PIC  9(04)  COMP  VALUE 0001.
           05  660-CTERM-T720          PIC  9(04)  COMP  VALUE 0008.
           05  660-PAGA-T720           PIC  9(04)  COMP  VALUE 0003.
           05  660-TIMES-T720          PIC  9(04)  COMP  VALUE 0026.
           05  660-AGENCIA-T720        PIC  9(04)  COMP  VALUE 0005.
BRQ001     05  660-TABELA-T720         OCCURS      03    TIMES.
BR1217         07  660-BANCO-T720      PIC  9(04)  COMP  VALUE 0008.
BR1217         07  660-REFBACEN-720    PIC  9(04)  COMP  VALUE 0011.
BR1217         07  660-ATIV-T720       PIC  9(04)  COMP  VALUE 0003.
BR1217         07  660-DTEMI-T720      PIC  9(04)  COMP  VALUE 0010.
BR1217         07  660-DTVCT-T720      PIC  9(04)  COMP  VALUE 0010.
BR1217         07  660-VALOR-T720      PIC  9(04)  COMP  VALUE 0020.
BR1217         07  660-FONTE-T720      PIC  9(04)  COMP  VALUE 0030.
RLOS           07  660-CNPJ-PRO-T820   PIC  9(04)  COMP  VALUE 0010.
RLOS           07  660-REFBACEN-PRO-T820
                                       PIC  9(04)  COMP  VALUE 0011.
RLOS           07  660-VLR-PRO-T820    PIC  9(04)  COMP  VALUE 0020.
BRQ001         07  660-CNPJERO-T720    PIC  9(04)  COMP  VALUE 0018.
BRQ001         07  660-CNPJPAR-T720    PIC  9(04)  COMP  VALUE 0018.
BR1217     05  660-CTRBAC-T720         PIC  9(04)  COMP  VALUE 0020.
ACT137     05  660-TOTBA-T820          PIC  9(04)  COMP  VALUE 0020.
ACT137     05  660-TOTBR-T820          PIC  9(04)  COMP  VALUE 0020.
ACT137     05  660-TOTRE-T820          PIC  9(04)  COMP  VALUE 0020.
           05  660-LITERAL-T720        PIC  9(04)  COMP  VALUE 0038.
           05  660-RESPTA-T720         PIC  9(04)  COMP  VALUE 2003.
           05  660-MENS-T720           PIC  9(04)  COMP  VALUE 0079.

      /-----------------------------------------------------*
      * MENSAGENS DE ERRO PARA SEREM UTILIZADAS NO PROGRAMA *

       01  WRK-MSG00.
           05  FILLER              PIC X(79)        VALUE
           'DADOS DE  E.G.F. OBRIGATORIOS'.
       01  WRK-MSG01.
           05  FILLER              PIC X(79)        VALUE
           'CAMPO(S) INCONSISTENTE(S)'.
       01  WRK-MSG02.
           05  FILLER              PIC X(79)        VALUE
           'DIGITE CAMPO(S) ANTERIOR(ES)'.
       01  WRK-MSG03.
           05  FILLER              PIC X(32)        VALUE
           'PESQUISA NAO AUTORIZADA'.
       01  WRK-MSG04.
           05  FILLER              PIC X(79)        VALUE
           'PF INVALIDA'.
       01  WRK-MSG05.
           05  FILLER              PIC X(79)        VALUE
           'ATIVIDADE INEXISTENTE'.
       01  WRK-MSG06.
           05  FILLER              PIC X(79)        VALUE
       01  WRK-MSG07.
           05  FILLER              PIC X(79)        VALUE
           'FINALIDADE NAO COMPATIVEL COM A ATIVIDADE'.
       01  WRK-MSG08.
           05  FILLER              PIC X(79)        VALUE
           'PRODUTO INEXISTENTE'.
       01  WRK-MSG09.
           05  FILLER              PIC X(79)        VALUE
           'ORIGEM DE RECURSOS INEXISTENTE'.
       01  WRK-MSG10.
           05  FILLER              PIC X(79)        VALUE
           'TECLE "PF11" PARA CONFIRMAR OU ENTER'.
       01  WRK-MSG11.
           05  FILLER              PIC X(79)        VALUE
           'COMPLETE OS DADOS E TECLE ENTER'.
       01  WRK-MSG12.
           05  FILLER              PIC X(79)        VALUE
           'PRODUTO BLOQUEADO - TECLE ENTER OU PF11 P/ APROVACAO DO DCR
      -    ' '.
       01  WRK-MSG13.
           05  FILLER              PIC X(79)        VALUE
      -    'O'.
       01  WRK-MSG14.
           05  FILLER              PIC X(79)        VALUE
           'UNIDADE DE MEDIDA INEXISTENTE'.
       01  WRK-MSG15.
           05  FILLER              PIC X(79)        VALUE
           'UNIDADE DE MEDIDA INVALIDA'.
       01  WRK-MSG16.
           05  FILLER              PIC X(79)        VALUE
           'AREA FINANCIADA MAIOR QUE AREA DO IMOVEL'.
       01  WRK-MSG17.
           05  FILLER              PIC X(79)        VALUE
           'FINALIDADE, ATIVIDADE E ORIGEM INCOMPATIVEIS'.
       01  WRK-MSG18.
           05  FILLER              PIC X(79)        VALUE
           'ORIGEM INDISPONIVEL - TECLE ENTER OU PF11 P/ APROVACAO DO DC
      -    'R'.
       01  WRK-MSG19.
           05  FILLER              PIC X(79)        VALUE
           'CEDULA BLOQUEADA - TECLE ENTER OU PF11 P/ APROVACAO DO DCR'.
       01  WRK-MSG20.
           'CEDULA BLOQUEADA-ALTERE OS DADOS OU ENTRE EM CONTATO COM O D
      -    'CR'.
       01  WRK-MSG21.
           05  FILLER              PIC X(79)        VALUE
           'FINALIDADE NAO E DE E.G.F.'.
       01  WRK-MSG22.
           05  FILLER              PIC X(79)        VALUE
           'MUNICIPIO INEXISTENTE'.
       01  WRK-MSG23.
           05  FILLER              PIC X(79)        VALUE
           'COMPLETE OS DADOS DO FINANCIAMENTO'.
       01  WRK-MSG24.
           05  FILLER              PIC X(79)        VALUE
           'ALTERE OS DADOS DO FINANCIAMENTO'.
       01  WRK-MSG25.
           05  FILLER              PIC X(79)        VALUE
           'CODIGO DE EMBALAGEM INEXISTENTE'.
       01  WRK-MSG26.
           05  FILLER              PIC X(79)        VALUE
           'CODIGO DE SEGURADORA INEXISTENTE'.
       01  WRK-MSG27.
           'CODIGO TECLADO NAO E SEGURADORA'.
       01  WRK-MSG28.
           05  FILLER              PIC X(79)        VALUE
           'CLASSIFICACAO DO PRODUTO INEXISTENTE OU INCOMPATIVEL COM O P
      -    'RODUTO'.
       01  WRK-MSG29.
           05  FILLER              PIC X(79)        VALUE
           'CODIGO CATEGORIA INEXISTENTE'.
       01  WRK-MSG30.
           05  FILLER              PIC X(79)        VALUE
           'TIPO DE PRODUTOR INEXISTENTE'.
       01  WRK-MSG31.
           05  FILLER              PIC X(79)        VALUE
           'CLASSIFICACAO NAO COMPATIVEL COM O PRODUTO'.
       01  WRK-MSG32.
           05  FILLER              PIC X(79)        VALUE
           'TIPO DE PRODUTOR INCONSISTENTE'.
       01  WRK-MSG33.
           05  FILLER              PIC X(79)        VALUE
           'QUANTIDADE DE EMBALAGEM NAO DIGITADO'.
       01  WRK-MSG34.
           'CODGIGO DE EMBALAGEM NAO DIGITADO'.
       01  WRK-MSG35.
           05  FILLER              PIC X(79)        VALUE
           'CODIGO DO TERMINAL IGUAL A BRANCOS'.
       01  WRK-MSG36.
           05  FILLER              PIC X(79)        VALUE
           'TECLE "PF9" PARA CONSULTA DE LISTA'.
       01  WRK-MSG37.
           05  FILLER              PIC X(79)        VALUE
           'PREENCHA/ALTERE OS DADOS OU TECLE "PF11" PARA PROXIMA TELA'.
       01  WRK-MSG38.
           05  FILLER              PIC X(79)        VALUE
           'DIGITACAO DE QTDE. EMBALAGEM OBRIGATORIA'.
       01  WRK-MSG39.
           05  FILLER              PIC X(79)        VALUE
           'DIGITACAO DE EMBALAGEM OBRIGATORIA'.
       01  WRK-MSG40.
           05  FILLER              PIC X(79)        VALUE
           'NOME DA SEGURADORA PROPRIO OBRIGATORIO'.
       01  WRK-MSG41.
           05  FILLER              PIC X(79)        VALUE
       01  WRK-MSG42.
           05  FILLER              PIC X(79)        VALUE
           'SEGURADORA PROPRIA: PREENCHIMENTO OBRIGATORIO DO NOME E NUME
      -    'RO DA APOLICE'.
       01  WRK-MSG43.
           05  FILLER              PIC X(79)        VALUE
           'CODIGO UNIDADE DE MEDIDA INEXISTENTE'.
       01  WRK-MSG44.
           05  FILLER              PIC X(79)        VALUE
           'ORIGEM (COOPERATIVA): DIGITE APENAS 1, 2 OU 3'.
ACT137 01  WRK-MSG45.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'CODIGO DO BANCO DEVE SER DE OUTROS BANCOS    '.
ACT137 01  WRK-MSG46.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'BANCO INEXISTENTE'.
ACT137 01  WRK-MSG47.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'DATA DA CEDULA MAIOR QUE DATA CORRENTE'.
ACT137 01  WRK-MSG48.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137 01  WRK-MSG49.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'DATA DA CEDULA IGUAL A DATA DO VENCIMENTO'.
ACT137 01  WRK-MSG50.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'REGISTRO COM DUPLICIDADE (BANCO, CEDULA E VALOR) NA TELA'.
ACT137 01  WRK-MSG51.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'REGISTRO COM DUPLICIDADE (BANCO, CEDULA E VALOR) NA TABELA'.
ACT137 01  WRK-MSG53.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'POSICIONE CURSOR NA LINHA DO BANCO E TECLE <PF9>'.
HE0710 01  WRK-MSG54.
HE0710     05  FILLER              PIC X(79)        VALUE
HE0710     'ATUALIZACAO NAO EFETUADA. A CEDULA ESTA SENDO ATUALIZADA POR
HE0710-    ' OUTRO USUARIO'.

BR1217/---------------------------------------------------------------*
BR1217*             LAY-OUT TABELA                                    *
BR1217*---------------------------------------------------------------*
BR1217
BR1217
BR1217/---------------------------------------------------------------*
BR1217*             LAY-OUT TABELA                                    *
BR1217*---------------------------------------------------------------*
BR1217
BR1217     EXEC SQL    INCLUDE  CRURB002      END-EXEC.
BR1217
BR1217/---------------------------------------------------------------*
BR1217*             LAY-OUT TABELA                                    *
BR1217*---------------------------------------------------------------*
BR1217
BR1217     EXEC SQL    INCLUDE  RURCB028      END-EXEC.
BR1217
      /---------------------------------------------------------------*
      *             LAY-OUT TABELA                                    *
      *---------------------------------------------------------------*

           EXEC SQL    INCLUDE  RUECB046      END-EXEC.

      /---------------------------------------------------------------*
      *             LAY-OUT TABELA                                    *

BR1217     EXEC SQL    INCLUDE  RUECB09C      END-EXEC.

      /---------------------------------------------------------------*
      *             LAY-OUT TABELA                                    *
      *---------------------------------------------------------------*
           EXEC SQL
                INCLUDE RUECB042
           END-EXEC.

      /---------------------------------------------------------------*
      *             LAY-OUT TABELA                                    *
      *---------------------------------------------------------------*
           EXEC SQL
                INCLUDE RUECV007
           END-EXEC.

      /---------------------------------------------------------------*
      *                     SQLCA  DO PGM                             *
      *---------------------------------------------------------------*

BR1217
BR1217     EXEC SQL
BR1217       DECLARE CSR01-RUECB046 CURSOR FOR
BR1217       SELECT
BR1217         CBCO
BR1217       , VPCELA_COOP_RURAL
BR0918       , VFINAN
BR1217       , CPRODT_CREDT_RURAL
BR1217       , DCDULA_CREDT_RURAL
BR1217       , DVCTO_CREDT_RURAL
BR1217       , CEMPTO_BACEN
BR1217       , CREFT_RECOR
BR1217       , CTPO_FONTE_RECOR
RLOS         , VALUE (CCNPJ_BASE_MUTUA, 0)
RLOS         , VALUE (CFLIAL_BASE_MUTUA, 0)
RLOS         , VALUE (CCTRL_BASE_MUTUA, 0)
RLOS         , VALUE (NREFT_BACEN_INVES, 0)
RLOS         , VALUE (VPCELA_INVES_RURAL, 0)
BRQ001       , VALUE (CCNPJ_ENTID_RESP,   0)
BRQ001       , VALUE (CFLIAL_CNPJ_ENTID,  0)
BRQ001       , VALUE (CCTRL_CNPJ_ENTID,   0)
BRQ001       , VALUE (CFLIAL_CNPJ_PARTC,  0)
BRQ001       , VALUE (CCTRL_CNPJ_PARTC,   0)
BR1217       FROM  DB2PRD.RESP_CONTR_RURAL
BR1217       WHERE CAG_BCRIA           = :RUECB046.CAG-BCRIA
BR1217       AND   CCDULA              = :RUECB046.CCDULA
BR1217       ORDER BY
BR1217         CSEQ_RESP_RURAL
BR1217     END-EXEC.

      /---------------*
       LINKAGE SECTION.
      *---------------*

       01  IO-PCB.
           03  IO-TERM                 PIC X(08).
           03  FILLER                  PIC X(02).
           03  IO-STATUS               PIC X(02).
           03  FILLER                  PIC X(12).
           03  IO-MODNAME              PIC X(08).

       01  ALT-PCB.
           03  FILLER                  PIC X(02).
           03  ALT-STATUS              PIC X(02).
           03  FILLER                  PIC X(12).
           03  ALT-MODNAME             PIC X(08).


      /---------------------------------------------------------------*
       PROCEDURE      DIVISION.
      *---------------------------------------------------------------*

           ENTRY      'DLITCBL'        USING     IO-PCB   ALT-PCB.

           MOVE       'GU'             TO        WRK-FUNCAO.

      *---------------------------------------------------------------*
       A100-PROCESSA-TRANSACAO SECTION.
      *---------------------------------------------------------------*

           CALL 'POOL5000' USING       WRK-FUNCAO
                                       IO-PCB          ALT-PCB
                                       WRK-MENSAGEM    WRK-MODNAME
                                       WRK-OPCAO-ATUAL WRK-COD-DEPTO.
           IF     RETURN-CODE = 04  OR
                  RETURN-CODE = 12
                  GOBACK.

           PERFORM M200-LIMPA-TELA
                   VARYING WRK-IND  FROM     1 BY 1
                   UNTIL   WRK-IND  GREATER  WRK-OCOR.

           MOVE   WRK-MENSAGEM     TO             AREA-INPUT.
           MOVE   IN-NAVEGA        TO             OU-NAVEGA.
           MOVE   'RUECT720'       TO             WRK-MODNAME.
ACT137     CALL   'POOL7600'       USING          WRK-DATA-HORA.

           IF  WRK-TRANSACAO        EQUAL  'RUEC7200'
               PERFORM              A300-ROTINA-PRINCIPAL
           ELSE
               PERFORM              0001-RECEBER-CONTROLE.

           IF  WRK-MODNAME             EQUAL  'RUECT720'
               PERFORM              Z100-POOL0660-RUECT720.

           GO          TO              A100-PROCESSA-TRANSACAO.


      /------------------------------------------------------------*
       0001-RECEBER-CONTROLE  SECTION.
      *------------------------------------------------------------*

           MOVE  WRK-MENSAGEM    TO   COMU-AREA.
           MOVE  COMU-SENHA      TO   OU-NAVEGA

           MOVE  SPACES          TO   AREA-MENSAGEM.

           IF  COMU-CTERM        =    SPACES
               MOVE  WRK-MSG35   TO   AREA-MENSAGEM
               PERFORM 1100-CONTROLE-RUEC7140
           ELSE
               MOVE  SPACES      TO   AREA-STATUS
               MOVE  COMU-CTERM  TO   OU-CTERM
                                      IN-CTERM
               MOVE 1            TO   WRK-OU-PAG

               PERFORM 2500-SELECT-RUECB042

               MOVE AGENCIA OF NAVEGACAO-RURAL  TO IN-AGENC
                                                   OU-AGENC
               MOVE CCDULA  OF NAVEGACAO-RURAL  TO IN-CEDULA
                                                   OU-CEDULA
               PERFORM 2600-SELECT-RUECV007

               MOVE  SPACES      TO   OU-LITERAL
               MOVE  SPACES      TO   OU-RESPTA
               MOVE  WRK-241     TO   OU-RESPTA-ATR

BR1217         PERFORM 1600-OBTER-RESP

           END-IF.

       0001-99-FIM. EXIT.

      /---------------------------------------------------------------*
       A300-ROTINA-PRINCIPAL   SECTION.

           PERFORM 2500-SELECT-RUECB042.

           EVALUATE  IN-PFK

BR1217        WHEN '*'
BR1217            MOVE 'TECLA DE PF INVALIDA' TO OU-MENSAGEM
BR1217        WHEN 'C'
BR1217           PERFORM D100-STARTA-RUEC7000
BR1217        WHEN '.'
BR1217           IF IN-FASE   EQUAL   1
BR1217              PERFORM 1700-TRATAR-RETORNO
BR1217           END-IF
BR1217        WHEN '7'
BR1217           IF IN-FASE  EQUAL  2
BR1217              PERFORM 1850-TRATAR-PFK7
BR1217           END-IF
BR1217        WHEN '8'
BR1217           IF IN-FASE  EQUAL  2
BR1217              PERFORM 1860-TRATAR-PFK8
BR1217           END-IF
BR1217           PERFORM 1870-TRATAR-PFK11
BR1217     END-EVALUATE.

           PERFORM M100-MANTEM-TELA.

      *---------------------------------------------------------------*
       A300-EXIT.              EXIT.
      *---------------------------------------------------------------*

      /---------------------------------------------------------------*
       D100-STARTA-RUEC7000    SECTION.
      *---------------------------------------------------------------*

           MOVE  WRK-CHNG             TO    WRK-FUNCAO.
           MOVE  'RUEC7000'           TO    WRK-MODNAME.
           MOVE  IN-NAVEGA            TO    AREA-SENHA.
           MOVE  IN-CTERM             TO    AREA-CTERM.
           MOVE  SPACES               TO    AREA-STATUS
                                            AREA-MENSAGEM.
           MOVE  AREA-NAVEGA          TO    WRK-MENSAGEM.



      /---------------------------------------------------------------*
       M100-MANTEM-TELA        SECTION.
      *---------------------------------------------------------------*

           MOVE  IN-CEDULA               TO OU-CEDULA.
           MOVE  IN-AGENC                TO OU-AGENC.
           MOVE  IN-FASE                 TO OU-FASE.
           MOVE  WRK-IN-PAG              TO WRK-OU-PAG.
           MOVE  WRK-IN-CTRBAC           TO WRK-OU-CTRBAC
           MOVE  IN-CTERM                TO OU-CTERM
           MOVE  IN-TIMES                TO OU-TIMES.
           MOVE  IN-LITERAL              TO OU-LITERAL.
           MOVE  IN-RESPTA               TO OU-RESPTA.

           PERFORM  2900-TOTAL-BANCOS
           PERFORM  2950-TOTAL-BRADESCO
           ADD WRK-VFINAN-TELA  TO  WRK-VFINAN-BCO
           COMPUTE WRK-VFINAN-TOT = WRK-VFINAN-BCO
                                  + WRK-VFINAN-BRD
           MOVE WRK-VFINAN-BRD  TO  IN-TOTBR  OU-TOTBR
           MOVE WRK-VFINAN-TOT  TO  IN-TOTRE  OU-TOTRE.

           PERFORM M150-MANTEM-OCOR
                   VARYING WRK-IND FROM    1 BY 1
                   UNTIL   WRK-IND GREATER WRK-OCOR.

      *---------------------------------------------------------------*
       M100-EXIT.              EXIT.
      *---------------------------------------------------------------*

      /---------------------------------------------------------------*
       M150-MANTEM-OCOR        SECTION.
      *---------------------------------------------------------------*

           MOVE  IN-BANCO       (WRK-IND) TO OU-BANCO       (WRK-IND).
           MOVE  WRK-IN-RFBACEN (WRK-IND) TO WRK-OU-RFBACEN (WRK-IND).
           MOVE  WRK-IN-FNALD   (WRK-IND) TO WRK-OU-FNALD   (WRK-IND).
           MOVE  WRK-IN-ATIV    (WRK-IND) TO WRK-OU-ATIV    (WRK-IND).
           MOVE  WRK-IN-DTEM    (WRK-IND) TO WRK-OU-DTEM    (WRK-IND).
           MOVE  WRK-IN-DTVCTO  (WRK-IND) TO WRK-OU-DTVCTO  (WRK-IND).
           MOVE  WRK-IN-FONTE   (WRK-IND) TO WRK-OU-FONTE   (WRK-IND).
RLOS       MOVE  WRK-IN-RFBACEN-PRO  (WRK-IND)
.                                         TO WRK-OU-RFBACEN-PRO
.                                                           (WRK-IND).
.          MOVE  WRK-IN-CNPJ-PRO-ALF  (WRK-IND)
.                                         TO WRK-OU-CNPJ-PRO-ALF
.                                                             (WRK-IND).
RLOS       MOVE  WRK-IN-VALOR-PRO-ALF  (WRK-IND)
                                          TO WRK-OU-VALOR-PRO-ALF
                                                              (WRK-IND).

BRQ001     MOVE  WRK-IN-CNPJ-ENT-RESP-ORIG(WRK-IND)
BRQ001                                    TO WRK-OU-CNPJ-ENT-RESP-ORIG
BRQ001                                                        (WRK-IND).
BRQ001     MOVE  WRK-IN-CNPJ-PARTC(WRK-IND)
BRQ001                                    TO WRK-OU-CNPJ-PARTC(WRK-IND).

      *---------------------------------------------------------------*
       M150-EXIT.              EXIT.
      *---------------------------------------------------------------*

       M200-LIMPA-TELA         SECTION.
      *---------------------------------------------------------------*

BR1217     MOVE SPACES     TO IN-BANCO(WRK-IND)
BR1217                        OU-BANCO(WRK-IND)
BR1217                        WRK-IN-RFBACEN(WRK-IND)
BR1217                        WRK-OU-RFBACEN(WRK-IND)
BR1217                        WRK-IN-FNALD(WRK-IND)
BR1217                        WRK-OU-FNALD(WRK-IND)
BR1217                        WRK-IN-ATIV(WRK-IND)
BR1217                        WRK-OU-ATIV(WRK-IND)
BR1217                        WRK-IN-DTEM(WRK-IND)
BR1217                        WRK-OU-DTEM(WRK-IND)
BR1217                        WRK-IN-DTVCTO(WRK-IND)
BR1217                        WRK-OU-DTVCTO(WRK-IND)
BR1217                        IN-VALOR-ALF(WRK-IND)
BR1217                        OU-VALOR-ALF(WRK-IND)
BR1217                        WRK-IN-FONTE(WRK-IND)
BR1217                        WRK-OU-FONTE(WRK-IND)
RLOS                          WRK-IN-RFBACEN-PRO(WRK-IND)
.                             WRK-OU-RFBACEN-PRO(WRK-IND)
.                             WRK-OU-CNPJ-PRO-ALF(WRK-IND)
.                             WRK-IN-VALOR-PRO-ALF(WRK-IND)
RLOS                          WRK-OU-VALOR-PRO-ALF(WRK-IND)
BRQ001                        WRK-IN-CNPJ-PARTC(WRK-IND)
BRQ001                        WRK-OU-CNPJ-PARTC(WRK-IND)
BRQ001                        WRK-IN-CNPJ-ENT-RESP-ORIG(WRK-IND)
BRQ001                        WRK-OU-CNPJ-ENT-RESP-ORIG(WRK-IND).

      *---------------------------------------------------------------*
       M200-EXIT.              EXIT.
      *---------------------------------------------------------------*

      /---------------------------------------------------------------*
       Z100-POOL0660-RUECT720  SECTION.
      *---------------------------------------------------------------*

           MOVE LENGTH OF AREA-OUTPUT  TO  OU-LL
                                           660-LL-MENSAGEM-T720.
           MOVE LENGTH OF 660-RUECT720-SEG00
                                       TO  660-LL-AREA-T720.


           COMPUTE OU-DIA-T720 = WRK-DT-AAAAMMDD.
           COMPUTE OU-MES-T720 = WRK-DT-AAAAMMDD / 100.
           COMPUTE OU-ANO-T720 = WRK-DT-AAAAMMDD / 10000.

           MOVE   '/'                  TO          OU-BARRA1-T720
                                                   OU-BARRA2-T720.

           CALL 'POOL0660'             USING     AREA-OUTPUT
                                                 660-RUECT720-SEG00.

           IF RETURN-CODE              NOT       EQUAL ZEROS
              MOVE 'APL'               TO        ERR-TIPO-ACESSO
              MOVE RETURN-CODE         TO        WRK-RETURN-CODE
              MOVE 16                  TO        WRK-LOCAL-ERRO
              MOVE WRK-ERRO-POOL0660   TO        ERR-TEXTO
              GO                       TO        9999-ROTINA-ERRO.

           MOVE 'RUECT720'             TO        WRK-MODNAME.
           MOVE 'ISRT'                 TO        WRK-FUNCAO.
           MOVE AREA-OUTPUT            TO        WRK-MENSAGEM.
      *---------------------------------------------------------------*
       Z100-EXIT.              EXIT.
      *---------------------------------------------------------------*

      /----------------------------------------------------------*
       0850-ACESSA-RUECB042    SECTION.
      *----------------------------------------------------------*

           MOVE 'RUEC7200'       TO  AREA-TRANSACAO
           MOVE IN-CTERM         TO  AREA-CTERM
           MOVE SPACES           TO  AREA-MENSAGEM
                                     AREA-STATUS.

           PERFORM 2700-UPDATE-RUECB042.

           IF SQLCODE       EQUAL    100
              PERFORM       2730-INSERT-RUECB042.

       0850-99-FIM. EXIT.

      /------------------------------------------------------------*
      *------------------------------------------------------------*

           MOVE  WRK-CHNG             TO    WRK-FUNCAO.
           MOVE  'RUEC7140'           TO    WRK-MODNAME.
           MOVE  IN-NAVEGA            TO    AREA-SENHA.
           MOVE  IN-CTERM             TO    AREA-CTERM.
           MOVE  AREA-NAVEGA          TO    WRK-MENSAGEM.

       1100-99-FIM. EXIT.

      /------------------------------------------------------------*
       1200-CONTROLE-RUEC7020   SECTION.
      *------------------------------------------------------------*

           PERFORM   2760-UPDATE-RUECV007

HE0710     IF  WRK-INCONS             EQUAL 'S'
HE0710         GO TO 1200-99-FIM
HE0710     END-IF.

           PERFORM   0850-ACESSA-RUECB042
           MOVE  WRK-CHNG             TO    WRK-FUNCAO.
ACT137* ***MOVE  'RUEC7020'           TO    WRK-MODNAME.
ACT137     MOVE  CPROG OF RUECV007    TO    WRK-MODNAME.
           MOVE  IN-NAVEGA            TO    AREA-SENHA.
           MOVE  IN-CTERM             TO    AREA-CTERM.
           MOVE  SPACES               TO    AREA-STATUS
                                            AREA-MENSAGEM.
           MOVE  AREA-NAVEGA          TO    WRK-MENSAGEM.

       1200-99-FIM. EXIT.

BR1217/------------------------------------------------------------*
BR1217 1600-OBTER-RESP               SECTION.
BR1217*------------------------------------------------------------*
BR1217
BR1217     INITIALIZE WRK-LKG-AREA
BR1217
BR1217     MOVE 'RUEC7200'           TO WRK-PGM-CHAMADOR
BR1217     MOVE IN-AGENC             TO WRK-AGENCIA
BR1217     MOVE IN-CEDULA            TO WRK-CEDULA
BR1217     MOVE TIPO-PESSOA          TO WRK-TIPO-PESSOA
BR1217     IF TIPO-PESSOA            EQUAL 'J'
BR1217        MOVE NUMCGC            TO WRK-NUMCGC
BR1217        MOVE FILIAL            TO WRK-FILIAL
BR1217        MOVE CTRL-CGC          TO WRK-CTRL-CGC
BR1217     ELSE
BR1217        MOVE NUMCPF            TO WRK-NUMCPF
BR1217        MOVE ZEROS             TO WRK-FILLER
BR1217        MOVE CTRL-CPF          TO WRK-CTRL-CPF
BR1217     END-IF.
BR1217
7C0625     MOVE        1             TO WRK-NUM-ENVIO

BR1217     CALL WRK-MODULO-RUEC8840  USING  WRK-LKG-AREA
BR1217                                      ERRO-AREA.
BR1217
BR1217     IF WRK-COD-RET      EQUAL ZEROS
7C0625        INITIALIZE WRK-LKG-AREA
.
.             MOVE 'RUEC7200'           TO WRK-PGM-CHAMADOR
.             MOVE IN-AGENC             TO WRK-AGENCIA
.             MOVE IN-CEDULA            TO WRK-CEDULA
.
.             IF TIPO-PESSOA            EQUAL 'J'
.               MOVE NUMCGC            TO WRK-NUMCGC
.               MOVE FILIAL            TO WRK-FILIAL
.               MOVE CTRL-CGC          TO WRK-CTRL-CGC
.             ELSE
.               MOVE NUMCPF            TO WRK-NUMCPF
.               MOVE ZEROS             TO WRK-FILLER
.               MOVE CTRL-CPF          TO WRK-CTRL-CPF
.             END-IF
.
.             MOVE        2            TO WRK-NUM-ENVIO
.
BR1811     EXEC SQL
BR1811          SET :WRK-TIMESTAMP-1S  = CURRENT_TIMESTAMP + 5 SECOND
BR1811     END-EXEC
BR1811
BR1811     PERFORM UNTIL WRK-TIMES-1S  GREATER WRK-TIMESTAMP-1S
BR1811         CALL 'POOL7600'         USING WRK-AREA-POOL7600
BR1811
BR1811         STRING WRK-PO-TIMESTAMP(01:04) '-'
BR1811                WRK-PO-TIMESTAMP(07:02) '-'
BR1811                WRK-PO-TIMESTAMP(09:02) '.'
BR1811                WRK-PO-TIMESTAMP(11:02) '.'
BR1811                WRK-PO-TIMESTAMP(13:02) '.'
BR1811                WRK-PO-TIMESTAMP(15:06)
BR1811           DELIMITED BY SIZE   INTO  WRK-TIMES-1S
CC1811     END-PERFORM
.
.             CALL WRK-MODULO-RUEC8840  USING  WRK-LKG-AREA
.                                           ERRO-AREA
.             IF WRK-COD-RET      EQUAL ZEROS
.               MOVE WRK-CONTROLE-BACEN
.                                     TO     WRK-IN-CTRBAC
.                                            WRK-OU-CTRBAC
.
.               MOVE 'TECLE <ENTER> PARA INCLUIR A RESPONSABILIDADE'
.                                     TO     OU-MENSAGEM
.               MOVE    1             TO     IN-FASE
.                                            OU-FASE
.             ELSE
.               MOVE WRK-MENSAGEM-R    TO OU-MENSAGEM
7C0625        END-IF
BR1217     ELSE
BR1217        MOVE WRK-MENSAGEM-R    TO OU-MENSAGEM
BR1217
BR1217     END-IF.
BR1217
BR1217 1600-99-FIM. EXIT.
BR1217
BR1217/------------------------------------------------------------*
BR1217 1700-TRATAR-RETORNO           SECTION.
BR1217*------------------------------------------------------------*
BR1217
BR1217     MOVE WRK-IN-CTRBAC        TO CCTRL-RQUIS-FINCR OF RUECB09C.
BR1217     MOVE 'COR0006R1'          TO WRK-CMSGEM-RECOR
BR1217     MOVE 'COR0006E'           TO WRK-CMSGEM-RECOR-E
BR1217     MOVE ZEROS                TO NMNTRC-MSGEM-RURAL
BR1217                                                    OF RUECB09C.
BR1217     EXEC SQL
BR1217       SELECT CMSGEM_RECOR
BR1217        INTO :RUECB09C.CMSGEM-RECOR
BR1217                 :WRK-CMSGEM-RECOR-NULL
BR1217        WHERE CCTRL_RQUIS_FINCR   = :RUECB09C.CCTRL-RQUIS-FINCR
BR1217        AND  (CMSGEM_RECOR        = :WRK-CMSGEM-RECOR
BR1217         OR   CMSGEM_RECOR        = :WRK-CMSGEM-RECOR-E)
BR1217        AND   NMNTRC_MSGEM_RURAL >= :RUECB09C.NMNTRC-MSGEM-RURAL
BR1217     END-EXEC.
BR1217
BR1217     IF  ( SQLCODE   NOT  EQUAL ZEROS AND +100) OR
BR1217         ( SQLWARN0       =    'W'   )
BR1217           MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
BR1217           MOVE 'TMNTRC_MSGEM_RURAL'     TO   ERR-DBD-TAB
BR1217           MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
BR1217           MOVE SQLCODE                  TO   ERR-SQL-CODE
BR1217           MOVE '1700'                   TO   ERR-LOCAL
BR1217           MOVE SPACES                   TO   ERR-SEGM
BR1217           PERFORM   9999-ROTINA-ERRO
BR1217     END-IF.
BR1217
BR1217     IF SQLCODE                          EQUAL +100
BR1217        MOVE 'AGUARDANDO RETORNO DO BACEN  - TECLE ENTER '
BR1217                                         TO OU-MENSAGEM
BR1217        MOVE  1                          TO IN-FASE
BR1217        PERFORM M200-LIMPA-TELA
BR1217                VARYING WRK-IND  FROM     1 BY 1
BR1217                UNTIL   WRK-IND  GREATER  WRK-OCOR
BR1217        GO TO 1700-99-FIM
BR1217     END-IF.
BR1217
BR1217     IF  WRK-CMSGEM-RECOR-NULL   EQUAL -1
BR1217         MOVE SPACES             TO  CMSGEM-RECOR OF RUECB09C
BR1217     END-IF.
BR1217
BR1217     IF CMSGEM-RECOR OF RUECB09C EQUAL 'COR0006E'
BR1217        MOVE 'INCONSISTENCIA NA MSG DE RETORNO RCOR'
BR1217                                      TO OU-MENSAGEM
BR1217        MOVE  2                       TO IN-FASE
BR1217                                         OU-FASE
BR1217        GO TO 1700-99-FIM
BR1217     END-IF.
BR1217
BR1217     PERFORM 1851-SELECT-COUNT-RUECB046
BR1217     IF WRK-COUNT             EQUAL ZEROS
BR1217        MOVE 'NAO HA RESPONSABILIDADE PARA DEMONSTRAR'
BR1217        MOVE  2                       TO IN-FASE
BR1217                                         OU-FASE
BR1217        GO TO 1700-99-FIM
BR1217     END-IF
BR1217
BR1217     PERFORM 1852-ABRIR-CSR01-RUECB046
BR1217     PERFORM 1853-LER-CSR01-RUECB046
BR1217     MOVE ZEROS                      TO IND-1
BR1217                                        IND-2
BR1217
BR1217     MOVE 1                          TO IND-HEX
BR1217     MOVE IND-HEX                    TO WRK-ACUM-OCORR
BR1217     PERFORM 1854-PROCES-CSR-RUECB046  VARYING
BR1217                 IND-HEX              FROM IND-HEX  BY 1 UNTIL
BR1217                 WRK-FIM-CSR-RUECB046 EQUAL 'S'   OR
BRQ001                 IND-1                GREATER 02  OR
BR1217                 IND-2                GREATER WRK-COUNT
BR1217
BR1217     PERFORM 1855-FECHAR-CSR01-RUECB046
BR1217     MOVE  2                          TO IN-FASE
BR1217                                         OU-FASE.
BR1217
BR1217/------------------------------------------------------------*
BR1217 1850-TRATAR-PFK7              SECTION.
BR1217*------------------------------------------------------------*
BR1217
BR1217
BR1217     IF  WRK-IN-PAG              EQUAL 01
BR1217         MOVE 'NAO HA MAIS PAGINAS PARA VOLTAR'
BR1217                                 TO OU-MENSAGEM
BR1217         GO TO 1850-99-FIM
BR1217     END-IF.
BR1217
BR1217     PERFORM M200-LIMPA-TELA
BR1217             VARYING WRK-IND  FROM     1 BY 1
BR1217             UNTIL   WRK-IND  GREATER  WRK-OCOR.
BR1217
BR1217     PERFORM 1851-SELECT-COUNT-RUECB046
BR1217
BR1217     PERFORM 1852-ABRIR-CSR01-RUECB046.
BR1217
BR1217     PERFORM 1853-LER-CSR01-RUECB046.
BR1217     MOVE WRK-IN-PAG             TO WRK-PAG
BR1217     SUBTRACT 1                  FROM WRK-PAG.
BR1217     MOVE WRK-PAG                TO WRK-IN-PAG
BR1217
BR1217     MOVE WRK-IN-PAG             TO WRK-PAG-HEX.
BRQ001     COMPUTE IND-HEX             = ((WRK-PAG-HEX - 1 ) * 03 ) + 1.
BR1217
BR1217     MOVE ZEROS                      TO IND-1
BR1217                                        IND-2
BR1217
BR1217     MOVE IND-HEX                    TO WRK-ACUM-OCORR
BR1217     PERFORM 1854-PROCES-CSR-RUECB046  VARYING
BR1217                 IND-HEX              FROM IND-HEX  BY 1 UNTIL
BR1217                 WRK-FIM-CSR-RUECB046 EQUAL 'S'   OR
BRQ001                 IND-1                GREATER 02  OR
BR1217                 IND-2                GREATER WRK-COUNT
BR1217
BR1217     PERFORM 1855-FECHAR-CSR01-RUECB046.
BR1217
BR1217
BR1217 1850-99-FIM. EXIT.
BR1217
BR1217*----------------------------------------------------------------*
BR1217 1851-SELECT-COUNT-RUECB046      SECTION.
BR1217*----------------------------------------------------------------*
BR1217
BR1217     MOVE ZEROS                  TO WRK-COUNT.
BR1217     MOVE IN-AGENC               TO CAG-BCRIA OF RUECB046
BR1217     MOVE IN-CEDULA              TO CCDULA    OF RUECB046.
BR1217
BR1217     EXEC SQL
BR1217       SELECT
BR1217         COUNT(*)
BR1217       INTO
BR1217         :WRK-COUNT
BR1217       FROM  DB2PRD.RESP_CONTR_RURAL
BR1217       WHERE CAG_BCRIA           = :RUECB046.CAG-BCRIA
BR1217       AND   CCDULA              = :RUECB046.CCDULA
BR1217     END-EXEC.
BR1217
BR1217     IF (SQLCODE                 NOT EQUAL ZEROS )
BR1217     OR (SQLWARN0                EQUAL 'W'       )
BR1217         MOVE 'RESP_CONTR_RURAL' TO   ERR-DBD-TAB
BR1217         MOVE 'SELECT  '         TO   ERR-FUN-COMANDO
BR1217         MOVE SQLCODE            TO   ERR-SQL-CODE
BR1217         MOVE '1851'             TO   ERR-LOCAL
BR1217         MOVE SPACES             TO   ERR-SEGM
BR1217         PERFORM 9999-ROTINA-ERRO
BR1217     END-IF.
BR1217
BR1217*----------------------------------------------------------------*
BR1217 1851-99-FIM.                    EXIT.
BR1217*----------------------------------------------------------------*
BR1217
BR1217*---------------------------------------------------------------*
BR1217 1852-ABRIR-CSR01-RUECB046        SECTION.
BR1217*---------------------------------------------------------------*
BR1217
BR1217     MOVE IN-AGENC               TO CAG-BCRIA OF RUECB046
BR1217     MOVE IN-CEDULA              TO CCDULA    OF RUECB046
BR1217
BR1217     EXEC SQL
BR1217           OPEN   CSR01-RUECB046
BR1217
BR1217     IF ( SQLCODE                NOT EQUAL ZEROS )   OR
BR1217        ( SQLWARN0               EQUAL 'W'   )
BR1217          MOVE 'DB2'                TO ERR-TIPO-ACESSO
BR1217          MOVE 'RESP_CONTR_RURAL'   TO ERR-DBD-TAB
BR1217          MOVE 'OPEN    '           TO ERR-FUN-COMANDO
BR1217          MOVE SQLCODE              TO ERR-SQL-CODE
BR1217          MOVE '1851'               TO ERR-LOCAL
BR1217          MOVE SPACES               TO ERR-SEGM
BR1217          PERFORM 9999-ROTINA-ERRO
BR1217     END-IF.
BR1217
BR1217     MOVE 'N'                    TO WRK-FIM-CSR-RUECB046.
BR1217
BR1217*---------------------------------------------------------------*
BR1217 1852-99-FIM.                   EXIT.
BR1217*---------------------------------------------------------------*
BR1217*---------------------------------------------------------------*
BR1217 1853-LER-CSR01-RUECB046          SECTION.
BR1217*---------------------------------------------------------------*
BR1217
BR1217       FETCH CSR01-RUECB046
BR1217       INTO
BR1217         :RUECB046.CBCO
BR1217       , :RUECB046.VPCELA-COOP-RURAL
BR1217       , :RUECB046.VFINAN
BR1217       , :RUECB046.CPRODT-CREDT-RURAL
BR1217       , :RUECB046.DCDULA-CREDT-RURAL
BR1217       , :RUECB046.DVCTO-CREDT-RURAL
BR1217       , :RUECB046.CEMPTO-BACEN
BR1217       , :RUECB046.CREFT-RECOR
BR1217       , :RUECB046.CTPO-FONTE-RECOR
RLOS         , :RUECB046.CCNPJ-BASE-MUTUA
RLOS         , :RUECB046.CFLIAL-BASE-MUTUA
RLOS         , :RUECB046.CCTRL-BASE-MUTUA
RLOS         , :RUECB046.NREFT-BACEN-INVES
RLOS         , :RUECB046.VPCELA-INVES-RURAL
7C0625       , :RUECB046.CCNPJ-ENTID-RESP
.            , :RUECB046.CFLIAL-CNPJ-ENTID
.            , :RUECB046.CCTRL-CNPJ-ENTID
.            , :RUECB046.CCNPJ-PARTC
.            , :RUECB046.CFLIAL-CNPJ-PARTC
BR1217     END-EXEC.
BR1217
BR1217     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100) OR
BR1217         ( SQLWARN0              EQUAL 'W' )
BR1217           MOVE 'DB2'                TO ERR-TIPO-ACESSO
BR1217           MOVE 'RESP_CONTR_RURAL'   TO ERR-DBD-TAB
BR1217           MOVE 'FETCH   '           TO ERR-FUN-COMANDO
BR1217           MOVE SQLCODE              TO ERR-SQL-CODE
BR1217           MOVE '1853'               TO ERR-LOCAL
BR1217           MOVE SPACES               TO ERR-SEGM
BR1217           PERFORM 9999-ROTINA-ERRO
BR1217     END-IF.
BR1217
BR1217     IF SQLCODE                  EQUAL +100
BR1217        MOVE 'S'                 TO WRK-FIM-CSR-RUECB046
BR1217     END-IF.
BR1217
BR1217*---------------------------------------------------------------*
BR1217 1853-99-FIM.                   EXIT.
BR1217*---------------------------------------------------------------*
BR1217*---------------------------------------------------------------*
BR1217*---------------------------------------------------------------*
BR1217
BR1217     MOVE CEMPTO-BACEN           OF RUECB046
BR1217                                 TO WRK-EMPREEND-RECOR.
BR1217
BR1217     ADD 1                      TO IND-2
BR1217
BR1217     IF IND-2                   LESS WRK-ACUM-OCORR
BR1217        PERFORM 1853-LER-CSR01-RUECB046
BR1217     ELSE
BR1217        ADD 1                   TO IND-1
BR1217        IF CBCO                 OF RUECB046
BR1217                                EQUAL 237
BR1217           MOVE 'BRADESCO'      TO IN-BANCO         (IND-1)
BR1217                                   OU-BANCO         (IND-1)
BR1217
BR1217        ELSE
BR1217           MOVE 'OUTROS'        TO IN-BANCO         (IND-1)
BR1217                                   OU-BANCO         (IND-1)
BR1217        END-IF
BR0918        IF  VPCELA-COOP-RURAL   OF RUECB046 NOT EQUAL ZEROS
BR0918                                TO IN-VALOR         (IND-1)
BR0918                                   OU-VALOR         (IND-1)
BR0918        ELSE
BR1217            MOVE VFINAN         OF RUECB046
BR1217                                TO IN-VALOR         (IND-1)
BR1217                                   OU-VALOR         (IND-1)
BR0918        END-IF

BR1217        MOVE DCDULA-CREDT-RURAL OF RUECB046
BR1217                                TO WRK-OU-DTEM      (IND-1)
BR1217                                   WRK-IN-DTEM      (IND-1)
BR1217        MOVE DVCTO-CREDT-RURAL  OF RUECB046
BR1217                                TO WRK-OU-DTVCTO    (IND-1)
BR1217                                   WRK-IN-DTVCTO    (IND-1)
BR1217        MOVE CREFT-RECOR        OF RUECB046
BR1217                                TO WRK-IN-RFBACEN   (IND-1)
BR1217                                   WRK-OU-RFBACEN   (IND-1)
BR1217
BR1217        PERFORM 1900-OBTER-ATIVIDADE
BR1217        MOVE IATVDD-RURAL       OF CRURB002(01:03)
BR1217                                TO WRK-IN-ATIV      (IND-1)
BR1217        PERFORM 1920-OBTER-FINALIDADE
BR1217        MOVE IFNALD-RURAL       OF CRURB016(01:07)
BR1217                                TO WRK-IN-FNALD     (IND-1)
BR1217                                   WRK-OU-FNALD     (IND-1)
BR1217        PERFORM 1950-OBTER-FONTE
BR1217        MOVE IPLANO-REFIN       OF RURCB028 (1:30)
BR1217                                TO WRK-IN-FONTE     (IND-1)
BR1217                                   WRK-OU-FONTE     (IND-1)

RLOS          MOVE CCNPJ-BASE-MUTUA   OF RUECB046
.                                     TO WRK-IN-CNPJ-PRO  (IND-1)
.                                        WRK-OU-CNPJ-PRO  (IND-1)
.
.             MOVE NREFT-BACEN-INVES  OF RUECB046
.                                     TO WRK-REF-BACEN-9
.             MOVE WRK-REF-BACEN-X    TO WRK-IN-RFBACEN-PRO (IND-1)
.                                        WRK-OU-RFBACEN-PRO (IND-1)
.
PROB**        MOVE VPCELA-INVES-RURAL OF RUECB046
PROB**                                TO WRK-IN-VALOR-PRO   (IND-1)
RLOS**                                   WRK-OU-VALOR-PRO   (IND-1)
BRQ001           MOVE CCNPJ-ENTID-RESP OF RUECB046
BRQ001                                TO WRK-CPFERO-BASE-CS
BRQ001           MOVE WRK-CPFERO-BASE-TELA1
BRQ001                                TO WRK-CPFERO-BASE1
BRQ001           MOVE WRK-CPFERO-BASE-TELA2
BRQ001                                TO WRK-CPFERO-BASE2
BRQ001           MOVE WRK-CPFERO-BASE-TELA3
BRQ001                                TO WRK-CPFERO-BASE3
BRQ001           MOVE CCTRL-CNPJ-ENTID OF RUECB046
BRQ001                                TO WRK-CPFERO-CTRL-N
BRQ001           MOVE WRK-CPFERO-FORM TO
BRQ001                           WRK-IN-CNPJ-ENT-RESP-ORIG(IND-1)
BRQ001                           WRK-OU-CNPJ-ENT-RESP-ORIG(IND-1)
BRQ001        ELSE
BRQ001           MOVE CCNPJ-ENTID-RESP OF RUECB046
BRQ001                                TO WRK-CNPJERO-BASE-CS
BRQ001           MOVE WRK-CNPJERO-BASE-TELA1
BRQ001                                TO WRK-CNPJERO-BASE1
BRQ001           MOVE WRK-CNPJERO-BASE-TELA2
BRQ001                                TO WRK-CNPJERO-BASE2
BRQ001           MOVE WRK-CNPJERO-BASE-TELA3
BRQ001           MOVE CFLIAL-CNPJ-ENTID OF RUECB046
BRQ001                                TO WRK-CNPJERO-FILI-N
BRQ001           MOVE CCTRL-CNPJ-ENTID OF RUECB046
BRQ001                                TO WRK-CNPJERO-CTRL-N
BRQ001           MOVE WRK-CNPJERO-FORM TO
BRQ001                           WRK-IN-CNPJ-ENT-RESP-ORIG(IND-1)
BRQ001                           WRK-OU-CNPJ-ENT-RESP-ORIG(IND-1)
BRQ001        END-IF
BRQ001
BRQ001        IF CFLIAL-CNPJ-PARTC OF RUECB046 EQUAL ZEROS
BRQ001           MOVE CCNPJ-PARTC OF RUECB046
BRQ001                                TO WRK-CPFERO-BASE-CS
BRQ001           MOVE WRK-CPFERO-BASE-TELA1
BRQ001                                TO WRK-CPFERO-BASE1
BRQ001           MOVE WRK-CPFERO-BASE-TELA2
BRQ001                                TO WRK-CPFERO-BASE2
BRQ001           MOVE WRK-CPFERO-BASE-TELA3
BRQ001                                TO WRK-CPFERO-BASE3
BRQ001           MOVE CCTRL-CNPJ-PARTC OF RUECB046
BRQ001                                TO WRK-CPFERO-CTRL-N
BRQ001           MOVE WRK-CPFERO-FORM TO WRK-IN-CNPJ-PARTC(IND-1)
BRQ001        ELSE
BRQ001           MOVE CCNPJ-PARTC OF RUECB046
BRQ001                                TO WRK-CNPJERO-BASE-CS
BRQ001           MOVE WRK-CNPJERO-BASE-TELA1
BRQ001                                TO WRK-CNPJERO-BASE1
BRQ001           MOVE WRK-CNPJERO-BASE-TELA2
BRQ001                                TO WRK-CNPJERO-BASE2
BRQ001           MOVE WRK-CNPJERO-BASE-TELA3
BRQ001                                TO WRK-CNPJERO-BASE3
BRQ001           MOVE CFLIAL-CNPJ-PARTC OF RUECB046
BRQ001                                TO WRK-CNPJERO-FILI-N
BRQ001           MOVE CCTRL-CNPJ-PARTC OF RUECB046
BRQ001                                TO WRK-CNPJERO-CTRL-N
BRQ001           MOVE WRK-CNPJERO-FORM
BRQ001                                TO WRK-IN-CNPJ-PARTC(IND-1)
BRQ001                                   WRK-OU-CNPJ-PARTC(IND-1)
BRQ001        END-IF
BR1217
BR1217        PERFORM 1853-LER-CSR01-RUECB046
BR1217     END-IF.
BR1217
BR1217 1854-99-FIM.                   EXIT.
BR1217*---------------------------------------------------------------*
BR1217*---------------------------------------------------------------*
BR1217 1855-FECHAR-CSR01-RUECB046       SECTION.
BR1217*---------------------------------------------------------------*
BR1217
BR1217     EXEC SQL
BR1217       CLOSE   CSR01-RUECB046
BR1217     END-EXEC.
BR1217
BR1217     IF ( SQLCODE                NOT EQUAL ZEROS )   OR
BR1217        ( SQLWARN0               EQUAL 'W' )
BR1217          MOVE 'DB2'                TO ERR-TIPO-ACESSO
BR1217          MOVE 'RESP_CONTR_RURAL'   TO ERR-DBD-TAB
BR1217          MOVE 'CLOSE   '           TO ERR-FUN-COMANDO
BR1217          MOVE SQLCODE              TO ERR-SQL-CODE
BR1217          MOVE '1855'               TO ERR-LOCAL
BR1217          MOVE SPACES               TO ERR-SEGM
BR1217          PERFORM 9999-ROTINA-ERRO
BR1217     END-IF.
BR1217
BR1217 1855-99-FIM.                   EXIT.
BR1217*---------------------------------------------------------------*
BR1217
BR1217/------------------------------------------------------------*
BR1217 1860-TRATAR-PFK8              SECTION.
BR1217*------------------------------------------------------------*
BR1217
BR1217     PERFORM 1851-SELECT-COUNT-RUECB046
BR1217
BR1217     PERFORM 1852-ABRIR-CSR01-RUECB046.
BR1217
BR1217     PERFORM 1853-LER-CSR01-RUECB046.
BR1217
BR1217     MOVE WRK-IN-PAG             TO WRK-PAG.
BR1217     ADD  1                      TO WRK-PAG.
BR1217     MOVE WRK-PAG                TO WRK-IN-PAG
BR1217                                    WRK-OU-PAG.
BR1217
BR1217     MOVE WRK-IN-PAG             TO WRK-PAG-HEX.
BRQ001     COMPUTE IND-HEX             = ((WRK-PAG-HEX - 1 ) * 03 ) + 1.
BR1217
BR1217                                        IND-2
BR1217
BR1217     MOVE IND-HEX                    TO WRK-ACUM-OCORR
BR1217
BR1217     PERFORM 1854-PROCES-CSR-RUECB046  VARYING
BR1217                 IND-HEX              FROM IND-HEX  BY 1 UNTIL
BR1217                 WRK-FIM-CSR-RUECB046 EQUAL 'S'   OR
BRQ001                 IND-1                GREATER 02  OR
BR1217                 IND-2                GREATER WRK-COUNT
BR1217
BR1217     PERFORM 1855-FECHAR-CSR01-RUECB046.
BR1217
BR1217     IF IND-1                         EQUAL ZEROS
BR1217
BR1217        MOVE 'NAO HA MAIS PAGINAS PARA AVANCAR'
BR1217                                TO OU-MENSAGEM
BR1217        MOVE WRK-IN-PAG         TO WRK-PAG
BR1217        SUBTRACT 1              FROM WRK-PAG
BR1217        MOVE WRK-PAG            TO WRK-IN-PAG
BR1217                                   WRK-OU-PAG
BR1217        GO TO 1860-99-FIM
BR1217
BRQ001     IF IND-1                    LESS  03
BR1217        ADD  1                   TO IND-1
BR1217        PERFORM VARYING  IND-1   FROM  IND-1 BY 1
BRQ001          UNTIL IND-1            GREATER 03
BR1217         MOVE SPACES     TO IN-BANCO(IND-1)
BR1217                            OU-BANCO(IND-1)
BR1217                            WRK-IN-RFBACEN(IND-1)
BR1217                            WRK-OU-RFBACEN(IND-1)
BR1217                            WRK-IN-FNALD(IND-1)
BR1217                            WRK-OU-FNALD(IND-1)
BR1217                            WRK-IN-ATIV(IND-1)
BR1217                            WRK-OU-ATIV(IND-1)
BR1217                            WRK-IN-DTEM(IND-1)
BR1217                            WRK-OU-DTEM(IND-1)
BR1217                            WRK-IN-DTVCTO(IND-1)
BR1217                            WRK-OU-DTVCTO(IND-1)
BR1217                            IN-VALOR-ALF(IND-1)
BR1217                            OU-VALOR-ALF(IND-1)
BR1217                            WRK-IN-FONTE(IND-1)
BR1217                            WRK-IN-FONTE(IND-1)
.                                 WRK-OU-RFBACEN-PRO(IND-1)
.                                 WRK-IN-CNPJ-PRO-ALF(IND-1)
.                                 WRK-OU-CNPJ-PRO-ALF(IND-1)
.                                 WRK-IN-VALOR-PRO-ALF(IND-1)
RLOS                              WRK-OU-VALOR-PRO-ALF(IND-1)
BRQ001                            WRK-IN-CNPJ-PARTC(IND-1)
BRQ001                            WRK-OU-CNPJ-PARTC(IND-1)
BRQ001                            WRK-IN-CNPJ-ENT-RESP-ORIG(IND-1)
BRQ001                            WRK-OU-CNPJ-ENT-RESP-ORIG(IND-1)
BR1217        END-PERFORM
BR1217     END-IF.
BR1217
BR1217
BR1217 1860-99-FIM. EXIT.
BR1217
      /------------------------------------------------------------*
       1870-TRATAR-PFK11             SECTION.
      *------------------------------------------------------------*

           PERFORM 1200-CONTROLE-RUEC7020.


BR1217*----------------------------------------------------------------*
.     *    ROTINA PARA CONSULTAR TABELA TATVDD_RURAL - OBTER DESCRICAO *
.     *    DA ATIVIDADE RURAL                                          *
.     *----------------------------------------------------------------*
.      1900-OBTER-ATIVIDADE            SECTION.
.     *----------------------------------------------------------------*
.          INITIALIZE                  CRURB002
.     *
.          MOVE WRK-ATIV-RECOR         TO CATVDD-RURAL
.                                      OF CRURB002
.     *
.          EXEC SQL
.              SELECT IATVDD_RURAL
.                INTO :CRURB002.IATVDD-RURAL
.                FROM DB2PRD.TATVDD_RURAL
.               WHERE CATVDD_RURAL = :CRURB002.CATVDD-RURAL
.          END-EXEC
.     *
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
.          OR (SQLWARN0                EQUAL 'W'       )
.              MOVE 'TATVDD_RURAL'       TO ERR-DBD-TAB
.              MOVE 'SELECT'             TO ERR-FUN-COMANDO
.              MOVE SQLCODE              TO ERR-SQL-CODE
.              MOVE '1900'               TO ERR-LOCAL
.              MOVE SPACES               TO ERR-SEGM
.              PERFORM 9999-ROTINA-ERRO
.          END-IF
.     *
.          IF (SQLCODE                 EQUAL +100)
.              MOVE SPACES TO IATVDD-RURAL OF CRURB002
.          END-IF.
.     *
.     *----------------------------------------------------------------*
.      1900-99-FIM.                    EXIT.
BR1217*----------------------------------------------------------------*
BR1217*================================================================*
.      1920-OBTER-FINALIDADE           SECTION.
.     *================================================================*
.
.          MOVE WRK-FINAL-RECOR    TO CFNALD-RURAL OF CRURB016
.
.               SELECT IFNALD_RURAL
.                 INTO :CRURB016.IFNALD-RURAL
.                 FROM DB2PRD.TFNALD_RURAL
.                WHERE CFNALD_RURAL = :CRURB016.CFNALD-RURAL
.          END-EXEC.
.
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
.          OR (SQLWARN0                EQUAL 'W'       )
.              MOVE 'DB2'                TO ERR-TIPO-ACESSO
.              MOVE 'TFNALD_RURAL'       TO ERR-DBD-TAB
.              MOVE 'SELECT'             TO ERR-FUN-COMANDO
.              MOVE SQLCODE              TO ERR-SQL-CODE
.              MOVE '1920'               TO ERR-LOCAL
.              MOVE SPACES               TO ERR-SEGM
.              PERFORM 9999-ROTINA-ERRO
.          END-IF.
.
.          IF  SQLCODE EQUAL  +100
.              MOVE SPACES TO IFNALD-RURAL OF CRURB016
.          END-IF.
.     *----------------------------------------------------------------*
BR1217*----------------------------------------------------------------*
BR1217*---------------------------------------------------------------*
.      1950-OBTER-FONTE          SECTION.
.     *---------------------------------------------------------------*
.          MOVE   ZEROS                          TO CPLANO-REFIN
.                                                OF RURCB028.
.          MOVE CTPO-FONTE-RECOR    OF RUECB046  TO CPLANO-REFIN-RECOR
.                                                OF RURCB028.
.
.          EXEC SQL
.            SELECT CPLANO_REFIN,
.                   IPLANO_REFIN
.            INTO   :RURCB028.CPLANO-REFIN,
.                   :RURCB028.IPLANO-REFIN
.            FROM  DB2PRD.PLANO_REFIN
.            WHERE CPLANO_REFIN_RECOR    = :RURCB028.CPLANO-REFIN-RECOR
.            ORDER BY CPLANO_REFIN ASC
.            FETCH FIRST 1 ROW ONLY
.          END-EXEC.
.
.          IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
.              MOVE 'DB2'                TO ERR-TIPO-ACESSO
.              MOVE 'PLANO_REFIN'        TO ERR-DBD-TAB
.              MOVE 'SELECT'             TO ERR-FUN-COMANDO
.              MOVE SQLCODE              TO ERR-SQL-CODE
.              MOVE '1950'               TO ERR-LOCAL
.              MOVE SPACES               TO ERR-SEGM
.              PERFORM 9999-ROTINA-ERRO
.          END-IF.
.
.          IF  SQLCODE                  EQUAL +100
.              MOVE ZEROS  TO  CPLANO-REFIN       OF RURCB028
.              MOVE SPACES TO  IPLANO-REFIN       OF RURCB028
.          END-IF.
.
.     *----------------------------------------------------------------*
.      1950-99-FIM.                    EXIT.
BR1217*----------------------------------------------------------------*
      *------------------------------------------------------------*
       2500-SELECT-RUECB042 SECTION.
      *------------------------------------------------------------*


           EXEC SQL
             SELECT
                   WNAVG_CREDT_RURAL
             INTO
                   :RUECB042.WNAVG-CREDT-RURAL
             FROM   DB2PRD.NAVEGACAO_RURAL
             WHERE
                   CTERM              = :RUECB042.CTERM

           END-EXEC.

           IF SQLCODE       EQUAL    ZEROS
              MOVE WNAVG-CREDT-RURAL-TEXT  TO  NAVEGACAO-RURAL
           ELSE
DDF*******    IF SQLCODE       EQUAL    +100
DDF*******       PERFORM       2730-INSERT-RUECB042
DDF*******    ELSE
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'NAVEGACAO_RURAL   '     TO   ERR-DBD-TAB
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
                   MOVE 0005                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.


       2500-99-FIM.  EXIT.

      /------------------------------------------------------------*
       2600-SELECT-RUECV007 SECTION.
      *------------------------------------------------------------*

           MOVE IN-AGENC        TO  CJUNC-DEPDC OF RUECV007.
           MOVE IN-CEDULA       TO  CCDULA      OF RUECV007.

           EXEC SQL
             SELECT
                   HULT_ATULZ
             INTO
                   :RUECV007.HULT-ATULZ
             FROM   DB2PRD.V01CEDULA_TEMPR
             WHERE
                   CCDULA             = :RUECV007.CCDULA
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS) OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'CEDULA_TEMPR     '     TO   ERR-DBD-TAB
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0006                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.

           MOVE    HULT-ATULZ OF RUECV007        TO   IN-TIMES
                                                      OU-TIMES.

       2600-99-FIM.  EXIT.

ACT137/------------------------------------------------------------*
ACT137 2610-SELECT-RUECV007 SECTION.
ACT137*------------------------------------------------------------*
ACT137     EXEC SQL
ACT137          SELECT
ACT137                 CAPLIC_RURAL
ACT137          INTO
ACT137                :RUECV007.CAPLIC-RURAL
ACT137          FROM
ACT137                 DB2PRD.V01CEDULA_TEMPR
ACT137          WHERE
ACT137                CJUNC_DEPDC = :RUECV007.CJUNC-DEPDC     AND
ACT137                CCDULA      = :RUECV007.CCDULA          AND
ACT137                HULT_ATULZ  = :RUECV007.HULT-ATULZ
ACT137     END-EXEC.
ACT137
ACT137     IF SQLCODE  NOT EQUAL ZEROS AND +100  OR
ACT137        SQLWARN0     EQUAL 'W'
ACT137        MOVE   'DB2'                 TO    ERR-TIPO-ACESSO
ACT137        MOVE   'V01CEDULA_TEMPR  '  TO    ERR-DBD-TAB
ACT137        MOVE   'SELECT  '            TO    ERR-FUN-COMANDO
ACT137        MOVE    SQLCODE              TO    ERR-SQL-CODE
ACT137        MOVE    0008                 TO    ERR-LOCAL
ACT137        MOVE    SPACES               TO    ERR-SEGM
ACT137
ACT137 2610-99-FIM.  EXIT.
ACT137
      /------------------------------------------------------------*
       2700-UPDATE-RUECB042  SECTION.
      *------------------------------------------------------------*

           MOVE  IN-CTERM        TO  CTERM OF RUECB042

           MOVE  'RUEC7200'      TO  PROGRAMA-ANT OF NAVEGACAO-RURAL
           MOVE  IN-AGENC        TO  AGENCIA      OF NAVEGACAO-RURAL
           MOVE  IN-CEDULA       TO  CCDULA       OF NAVEGACAO-RURAL

           MOVE    4000          TO  WNAVG-CREDT-RURAL-LEN.
           MOVE  NAVEGACAO-RURAL TO  WNAVG-CREDT-RURAL-TEXT.

           EXEC SQL
             UPDATE   DB2PRD.NAVEGACAO_RURAL
               SET
                   WNAVG_CREDT_RURAL = :RUECB042.WNAVG-CREDT-RURAL
             WHERE
           END-EXEC.

           IF    ( SQLCODE        =    ZEROS  OR 100)
                   NEXT SENTENCE
           ELSE
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'NAVEGACAO_RURAL   '     TO   ERR-DBD-TAB
                   MOVE 'UPDATE  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0007                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.

       2700-99-FIM.  EXIT.


      /------------------------------------------------------------*
       2730-INSERT-RUECB042  SECTION.
      *------------------------------------------------------------*

           MOVE  'RUEC7200'      TO  PROGRAMA-ANT OF NAVEGACAO-RURAL
           MOVE  IN-CEDULA       TO  CCDULA       OF NAVEGACAO-RURAL
           MOVE  4000            TO  WNAVG-CREDT-RURAL-LEN.
           MOVE  NAVEGACAO-RURAL TO  WNAVG-CREDT-RURAL-TEXT.

           MOVE  IN-CTERM        TO  CTERM  OF  RUECB042

           EXEC SQL
             INSERT INTO DB2PRD.NAVEGACAO_RURAL
                  (CTERM,
                   WNAVG_CREDT_RURAL)
               VALUES
                  (:RUECB042.CTERM,
                   :RUECB042.WNAVG-CREDT-RURAL)
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS)   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'NAVEGACAO_RURAL   '     TO   ERR-DBD-TAB
                   MOVE 'INSERT  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM  9999-ROTINA-ERRO.


       2730-99-FIM.  EXIT.

      /------------------------------------------------------------*
       2760-UPDATE-RUECV007 SECTION.
      *------------------------------------------------------------*

           MOVE  IN-AGENC        TO  CJUNC-DEPDC  OF RUECV007
           MOVE  IN-CEDULA       TO  CCDULA       OF RUECV007
           MOVE  IN-TIMES        TO  HULT-ATULZ   OF RUECV007
HE0710     MOVE  'N'             TO  WRK-INCONS.

ACT137     PERFORM   2610-SELECT-RUECV007
ACT137
HE0710     IF  SQLCODE   EQUAL  +100
HE0710         MOVE WRK-MSG54         TO    OU-MENSAGEM
HE0710         MOVE 'S'               TO    WRK-INCONS
HE0710         GO TO 2760-99-FIM

ACT137     IF CAPLIC-RURAL OF RUECV007 EQUAL 1 OR 2 OR 6
ACT137        MOVE   'RUEC7020'            TO    CPROG OF RUECV007
ACT137     ELSE
ACT137        MOVE   'RUEC7140'            TO    CPROG OF RUECV007
ACT137     END-IF
ACT137
           EXEC SQL
                UPDATE DB2PRD.V01CEDULA_TEMPR
ACT137* ***     SET  CPROG              = 'RUEC7020',
ACT137          SET  CPROG              = :RUECV007.CPROG,
                     HULT_ATULZ         =  CURRENT TIMESTAMP
                WHERE  CJUNC_DEPDC      = :RUECV007.CJUNC-DEPDC
                AND    CCDULA           = :RUECV007.CCDULA
                AND    HULT_ATULZ       = :RUECV007.HULT-ATULZ
           END-EXEC.

           IF    ( SQLCODE     =    ZEROS  OR  100)
                  NEXT SENTENCE
           ELSE
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0012                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM   9999-ROTINA-ERRO.

       2760-99-FIM.  EXIT.

ACT137*---------------------------------------------------------------*
ACT137 2900-TOTAL-BANCOS         SECTION.
ACT137*---------------------------------------------------------------*
ACT137
ACT137     MOVE ZEROS                       TO WRK-VFINAN-BCO
ACT137                                         WRK-VFINAN-TELA
ACT137     MOVE IN-AGENC                    TO CAG-BCRIA   OF RUECB046
ACT137     MOVE IN-CEDULA                   TO CCDULA      OF RUECB046
BR1217     MOVE 999                         TO CBCO        OF RUECB046
ACT137
BR1217     EXEC SQL
BR1217          SELECT
BR1217            VALUE(SUM(VPCELA_COOP_RURAL), 0)
BR1217          FROM    DB2PRD.RESP_CONTR_RURAL
BR1217          WHERE   CAG_BCRIA           = :RUECB046.CAG-BCRIA
BR1217          AND     CCDULA              = :RUECB046.CCDULA
BR1217          AND     CBCO                = :RUECB046.CBCO
BR1217     END-EXEC
ACT137
ACT137     IF  (SQLCODE              NOT EQUAL ZEROS AND +100)
ACT137     OR   SQLWARN0                EQUAL 'W'
ACT137          MOVE   'DB2'                TO ERR-TIPO-ACESSO
ACT137          MOVE   'RESP_CONTR_RURAL'   TO ERR-DBD-TAB
ACT137          MOVE   'SELECT'             TO ERR-FUN-COMANDO
ACT137          MOVE    SQLCODE             TO ERR-SQL-CODE
ACT137          MOVE    0027                TO ERR-LOCAL
ACT137          MOVE    SPACES              TO ERR-SEGM
ACT137          PERFORM 9999-ROTINA-ERRO
ACT137     END-IF.
BR1217
BR1217     IF SQLCODE          EQUAL +100
BR1217        MOVE ZEROS                       TO WRK-VFINAN-BCO
BR1217                                            WRK-VFINAN-TELA
BR1217     END-IF.
ACT137*---------------------------------------------------------------*
ACT137 2900-EXIT.              EXIT.
ACT137*---------------------------------------------------------------*
ACT137
ACT137*---------------------------------------------------------------*
ACT137 2950-TOTAL-BRADESCO       SECTION.
ACT137*---------------------------------------------------------------*
ACT137
ACT137     MOVE ZEROS                       TO WRK-VFINAN-BRD
BR1217     MOVE IN-AGENC                    TO CAG-BCRIA   OF RUECB046
BR1217     MOVE IN-CEDULA                   TO CCDULA      OF RUECB046
BR1217     MOVE 237                         TO CBCO        OF RUECB046
BR1217
BR1217     EXEC SQL
BR1217          SELECT
BR1217            VALUE(SUM(VFINAN), 0)
BR1217          INTO   :WRK-VFINAN-BRD
BR1217          FROM    DB2PRD.RESP_CONTR_RURAL
BR1217          WHERE   CAG_BCRIA           = :RUECB046.CAG-BCRIA
BR1217          AND     CCDULA              = :RUECB046.CCDULA
BR1217          AND     CBCO                = :RUECB046.CBCO
BR1217
ACT137     IF  (SQLCODE               NOT EQUAL ZEROS AND +100)
ACT137     OR  (SQLWARN0                  EQUAL 'W')
ACT137          MOVE 'DB2'                   TO  ERR-TIPO-ACESSO
ACT137          MOVE 'SELECT'                TO  ERR-FUN-COMANDO
ACT137          MOVE  SQLCODE                TO  ERR-SQL-CODE
ACT137          MOVE 'SEL TOT BRADESCO'      TO  ERR-DBD-TAB
ACT137          MOVE  0028                   TO  ERR-LOCAL
ACT137          MOVE  SPACES                 TO  ERR-SEGM
ACT137          GO                           TO  9999-ROTINA-ERRO
ACT137     END-IF.
ACT137
BR1217     IF SQLCODE                        EQUAL +100
BR1217        MOVE ZEROS                     TO WRK-VFINAN-BRD
BR1217     END-IF.
BR1217
ACT137*---------------------------------------------------------------*
ACT137 2950-EXIT.              EXIT.
ACT137*---------------------------------------------------------------*
ACT137
      /---------------------------------------------------------------*
      *---------------------------------------------------------------*

           MOVE 'RUEC7200'             TO        ERR-PGM.
           MOVE WRK-COD-USER           TO        ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO        ERR-COD-DEPTO.
           MOVE 'RUEC7200'             TO        ERR-MODULO.
           CALL 'POOL7100'             USING     IO-PCB ERRO-AREA
                                                 ALT-PCB SQLCA.
           GOBACK.

      *---------------------------------------------------------------*
       9999-EXIT.              EXIT.
