      *---------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *---------------------------------------------------------------*
       PROGRAM-ID.    RUEC8200.
      *REMARKS.
      *       ***********************  RUEC8200  ***********************
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
ACT137*       *      POOL0660  - COMPRIME AREA DE I/O                  *
ACT137*       *      POOL1080  - RET. IMP. SELECIONADA VIA SENHA       *
ACT137*       *      POOL1570  - VALIDAR DATA COM SECULO               *
ACT137*       *      POOL5000  - MODULO ON-LINE                        *
ACT137*       *      POOL7100  - TRATAMENTO DE ERROS DE APLICACAO      *
ACT137*       *      POOL7600  - DATA E HORA DO SISTEMA                *
ACT137*       *                                                        *
      *       *                                                        *
      *       *                                                        *
      *       *                                                        *
      *       * TELAS               :                                  *
      *       *                                                        *
      *       *      RUECT820  -
      *       *                                                        *
      *       * PROGRAMA  CHAMADOR  :                                  *
      *       *                                                        *
      *       *                                                        *
      *       * PROGRAMA  CHAMADO   :                                  *
      *       *                                                        *
      *       *      RUEC8020  -
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
PW1577*                      A L T E R A C A O                        *
PW1577*===============================================================*
PW1577*                                                               *
PW1577*     ANALISTA ......: MARINA MIKI FUJITA   - SONDA PROCWORK    *
PW1577*     DATA ..........: DEZEMBRO/2009                            *
PW1577*     OBJETIVO ......: INCLUIR MAIS UMA OPCAO NO MENU DE ALTER. *
PW1577*                      CEDULA - OPCAO 09                        *
PW1577*                                                               *
PW1577*     PROJETO .......: PWT 09/1577  ANPR 2008/0536/5-02         *
PW1577*                      REGULARIZAR CEDULAS RURAIS CARTORIOS     *
PW1577*                                                               *
PW1577*===============================================================*
BR1217*                      A L T E R A C A O                        *
BR1217*===============================================================*
BR1217*                                                               *
BR1217*     ANALISTA ......: RENATA AMORIM LEGARIO BRQ                *
BR1217*     DATA ..........: DEZEMBRO/2017                            *
BR1217*                                                               *
BR1217*     OBJETIVO ......: OBTER DADOS DA RESPONSABILIDADE NO RCOR. *
BR1217*                                                               *
BR1217*===============================================================*
BRQ001*----------------------------------------------------------------*
BRQ001*               U L T I M A  A L T E R A C A O                   *
BRQ001*----------------------------------------------------------------*
BRQ001*                                                                *
BRQ001*      ANALISTA     : RODOLFO VIEIRA        - BRQ                *
BRQ001*                                                                *
BRQ001*      OBJETIVOS    : INCLUIR NOVOS CAMPOS DO CATALOGO SICOR     *
BRQ001*                     V5.02 NA MENSAGEM COR0006R1                *
BRQ001*                                                                *
BRQ001*      REFERENCIA   : BRQ001                                     *
BRQ001*                                                                *
BRQ001*================================================================*
7C0625*================================================================*
.     * ANALISTA REPONSAVEL: EDSON/NIKSON                              *
.     * DATA...............: junho/2025                                *
.     * OBJETIVOS..........: FLASH - AGRO7-441 / AGRO7-442 / AGRO7-435 *
.     *                      ALTERAR PARA ENVIAR A SOLICITACAO DAS     *
.     *                      RESPONSABILIDADES DOS COOPERADOS (ATUAL)  *
.     *                      E TAMBEM DE TODOS OS CLIENTES             *
7C0625*================================================================*

      /---------------------------------------------------------------*
       ENVIRONMENT    DIVISION.
      *---------------------------------------------------------------*

       CONFIGURATION               SECTION.
           DECIMAL-POINT IS COMMA.

      /---------------------------------------------------------------*
       DATA           DIVISION.
      *---------------------------------------------------------------*

      /---------------------------------------------------------------*
       WORKING-STORAGE             SECTION.
      *---------------------------------------------------------------*

ACT137 77  WRK-SQLCODE                 PIC S9(09)    COMP   VALUE ZEROS.
ACT137 77  WRK-VFINAN-BCO              PIC S9(13)V99 COMP-3 VALUE ZEROS.
ACT137 77  WRK-VFINAN-BRD              PIC S9(13)V99 COMP-3 VALUE ZEROS.
ACT137 77  WRK-VFINAN-TOT              PIC S9(13)V99 COMP-3 VALUE ZEROS.
ACT137 77  WRK-VFINAN-TELA             PIC S9(13)V99 COMP-3 VALUE ZEROS.
ACT137 77  WRK-VFINAN-ANTER            PIC S9(13)V99 COMP-3 VALUE ZEROS.
       77  WRK-TELA-ALTERADA           PIC  X(01)        VALUE 'N'.
       77  WRK-INCON                   PIC  X(01)        VALUE 'N'.
       77  WRK-CHNG                    PIC  X(04)        VALUE 'CHNG'.
       77  WRK-COUNT                   PIC S9(05) COMP   VALUE ZEROS.
       77  WRK-FIM-CSR-RUECB046        PIC  X(01)        VALUE SPACES.
       77  IND-2                       PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND                     PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND1                    PIC  9(02)        VALUE ZEROS.
ACT137 77  WRK-IND2                    PIC  9(02)        VALUE ZEROS.
       77  WRK-IND3                    PIC  9(02)        VALUE ZEROS.
       77  WRK-CONTA-INCON             PIC  9(02)        VALUE ZEROS.
BRQ001 77  WRK-OCOR                    PIC S9(05)        VALUE 03.
       77  WRK-RESTO                   PIC 9(03)         VALUE ZEROS.
       77  WRK-DESPREZA                PIC 9(05)  COMP-3 VALUE ZEROS.
       77  WRK-ACUM-OCORR              PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-PAG                     PIC 9(03)  COMP-3 VALUE ZEROS.
       77  WRK-PAG-HEX                 PIC 9(03)  COMP-3 VALUE ZEROS.
       77  WRK-TOTAL-LINHAS            PIC S9(5)V COMP-3 VALUE ZEROS.
       77  WRK-RUEC8500                PIC  X(8)       VALUE 'RUEC8500'.
       77  WRK-CSEQ-RESP-RURAL-NULL    PIC S9(04) COMP VALUE ZEROS.
ACT137 77  WRK-BANCO-NUM               PIC S9(03)    COMP-3 VALUE ZEROS.
ACT137 77  WRK-CEDUL-ALF               PIC  X(17)          VALUE SPACES.
ACT137 77  WRK-VALOR-NUM               PIC S9(15)V99 COMP-3 VALUE ZEROS.
RLOS   01  WRK-REF-BACEN-X             PIC X(011)          VALUE SPACES.
.      01  FILLER                      REDEFINES  WRK-REF-BACEN-X.
RLOS       05 WRK-REF-BACEN-9          PIC 9(011).
ACT137     03  WRK-DTCED-NUM           PIC  9(08).
ACT137 01  WRK-DTVCT-ALF                               VALUE SPACES.
ACT137     03  WRK-DTVCT-NUM           PIC  9(08).

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
BRQ001     03  WRK-CNPJERO-BASE2       PIC  X(03)      VALUE SPACES.
BRQ001     03  WRK-CNPJERO-PT2         PIC  X(01)      VALUE '.'.
BRQ001     03  WRK-CNPJERO-BASE3       PIC  X(03)      VALUE SPACES.
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
BRQ001                                 PIC  9(02).

BR1811 01  WRK-TIMES-1S                PIC X(26)         VALUE SPACES.
BR1811 01  WRK-TIMESTAMP-1S            PIC X(26)         VALUE SPACES.
BR1811
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
           03  WRK-TIMESTAMP           PIC X(20).

ACT137*************CAMPOS USADOS PARA A POOL1570***********
ACT137
ACT137 01  P1570-VALIDA-DATAS.
ACT137     03  P1570-DATA              PIC  X(10) VALUE SPACES.
ACT137     03  P1570-FORMATO-DATA      PIC  9(02) VALUE ZEROS.
ACT137     03  P1570-MENSAGEM          PIC  X(80) VALUE ZEROS.
ACT137
      *************CAMPOS USADOS PARA A POOL0492***********


       01  WRK-VALO.
           05  WRK-VALO-ENT          PIC  X(14).
           05  WRK-VALO-SAI          PIC  X(11).
           05  WRK-VALO-SAI-N  REDEFINES  WRK-VALO-SAI
                                     PIC  9(11).
           05  WRK-VALO-SAI-R  REDEFINES  WRK-VALO-SAI
                                     PIC  9(09)V99.
           05  WRK-VALO-TAM          PIC  9(08)  VALUE 14110902.
           05  WRK-EDIT-VALO         PIC  ZZZ.ZZZ.ZZ9,99.
           05  WRK-EDIT-DIAS         PIC  ZZZ9.

      ******************************************************
      * AREAS AUXILIARES PARA SEREM UTILIZADAS NO PROGRAMA *
      ******************************************************

       01          WRK-AREAS-AUXILIARES.

         05  WRK-EMPREEND-RECOR      PIC X(014)      VALUE SPACES.
         05  FILLER  REDEFINES  WRK-EMPREEND-RECOR.
             07  WRK-ATIV-RECOR      PIC 9(001).
             07  WRK-FINAL-RECOR     PIC 9(001).
             07  WRK-MODALID-RECOR   PIC X(002).
             07  WRK-PRODUTO-RECOR   PIC X(004).
             07  WRK-VARIED-RECOR    PIC X(003).
             07  WRK-CONSORC-RECOR   PIC X(001).
             07  WRK-CESTA-RECOR     PIC X(001).
             07  WRK-ZONEAM-RECOR    PIC X(001).
      *--------------------------------*
      * DEFINICAO DE AREAS DA POOL5000 *
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
           03 COMU-TRANS               PIC  X(15)      VALUE SPACES.
           03 COMU-SENHA               PIC  X(37).
           03 COMU-OPCAO               PIC  9(02)      VALUE ZEROS.
           03 COMU-CTERM               PIC  X(08)      VALUE SPACES.

      *---------------------------------------------------------------*
      *  AREA NAVEGADA PARA OUTROS PROGRAMAS                          *
      *---------------------------------------------------------------*
       01  AREA-NAVEGA.
           03 AREA-LL             PIC S9(04) COMP VALUE +147.
           03 AREA-ZZ             PIC S9(04) COMP VALUE ZEROS.
           03 AREA-TRANSACAO      PIC  X(15) VALUE 'RUEC8200'.
           03 AREA-SENHA          PIC  X(37).
           03 AREA-OPCAO          PIC  9(02).
           03 AREA-CTERM          PIC  X(08).
           03 AREA-STATUS         PIC  X(02).
           03 AREA-MENSAGEM       PIC  X(79).

      *---------------------------------------------------------------*
      *          AREAS RESERVADAS PARA UTILIZACAO DAS POOLS          *
      *---------------------------------------------------------------*

ACT137 COPY POL7100C.
ACT137 COPY I#RUEC00.
ACT137 COPY I#RUEC08.
ACT137 COPY I#POOLB1.
0CT137 COPY I#RUEC19.
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
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
                  15  WRK-FILIAL             PIC  9(004).
                  15  WRK-CTRL-CGC           PIC  9(002).
              10  WRK-CPF                    REDEFINES WRK-IDENTIF.
                  15  WRK-NUMCPF             PIC  9(009).
                  15  WRK-CTRL-CPF           PIC  9(002).
                  15  WRK-FILLER             PIC  9(004).
7C0625        10 WRK-NUM-ENVIO               PIC  9(001) VALUE ZEROS.
              10 FILLER                      PIC  X(061) VALUE SPACES.
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
      * REDEFINICAO DE AREAS UTILISADAS NO MODULO    "POOL5000"       *
      *---------------------------------------------------------------*

       01  WRK-COD-USUARIO.
           02  WRK-COD-USUARION        PIC  9(07).

       01  WRK-COD-DEPTO-X.
           02  WRK-COD-DEPTO           PIC  9(06)     VALUE ZEROS.
           02  WRK-COD-DEPTO-R         REDEFINES      WRK-COD-DEPTO.
               03  WRK-COD-DEPTO-2     PIC  9(04).

      /---------------------------------------------------------------*
      *               DEFINICAO DE WRK-ATRS                          *
      *---------------------------------------------------------------*

      *    WRK-ATR  NUM., PRE-MOD., POS.CURSOR, BRILHO, OVERRIDE.

       01  WRK-49369                   PIC S9(08) COMP VALUE +49369.

      *    WRK-ATR  ALFA, PRE-MOD., POS.CURSOR, BRILHO, OVERRIDE.

       01  WRK-49353                   PIC S9(08) COMP VALUE +49353.

      *    WRK-ATR  NUME, PRE-MOD., POS.CURSOR.

       01  WRK-49361                   PIC S9(08) COMP VALUE +49361.

      *    EQUIVALE AO 241 COM POSICIONAMENTO DE CURSOR  *
       01  WRK-49393                   PIC S9(08) COMP VALUE +49393.


       01  WRK-241                     PIC S9(08) COMP VALUE +241.

      *    WRK-ATR  ALFA, PRE-MOD., PROTEGIDO, OVERRIDE.

       01  WRK-161                     PIC S9(08) COMP VALUE +161.

       01  WRK-225                     PIC 9(05)   VALUE 225   COMP.

       01  WRK-209                     PIC 9(05)   VALUE 209   COMP.
      /---------------------------------------------------------------*
      * DEFINICAO DA AREA PARA RET-CODE DIFERENTE DE ZEROS (POOL0660) *
      *---------------------------------------------------------------*

       01  WRK-ERRO-POOL0660.
           03 FILLER                   PIC  X(48) VALUE
              '* RETURN-CODE DIFERENTE DE ZEROS NA POOL0660 *'.
           03 FILLER                   PIC  X(13) VALUE
              'RET.COD = '.
           03 WRK-RETURN-CODE          PIC  9(02)BB.
           03 FILLER                   PIC  X(08) VALUE 'LOCAL = '.

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
      *            TELA   RUECT820      -        INPUT                *
      *---------------------------------------------------------------*

       01  AREA-INPUT.
           03 FILLER                        PIC  X(18).
           03 IN-PFK                        PIC  X(01).
           03 IN-NAVEGA                     PIC  X(37).
           03 IN-OPCAO                      PIC  9(02).
           03 IN-FASE                       PIC  9(01).
           03 IN-CTERM                      PIC  X(08).
           03 IN-PAGATU                     PIC  9(03).
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
.             05 WRK-IN-VALOR-PRO-ALF.
.                07 WRK-IN-VALOR-PRO        PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
RLOS
BRQ001        05 WRK-IN-CNPJ-PARTC          PIC  X(18) VALUE SPACES.
BR1217     03 WRK-IN-CTRBAC                 PIC  X(20) VALUE SPACES.
ACT137     03 IN-TOTBA-ALF.
ACT137        05 IN-TOTBA                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137     03 IN-TOTBR-ALF.
ACT137        05 IN-TOTBR                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137     03 IN-TOTRE-ALF.
ACT137        05 IN-TOTRE                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
           03 IN-LITERAL                    PIC  X(33).
           03 IN-RESPTA                     PIC  X(01).
           03 INP-POSI.
               05  IN-LIN                   PIC  S9(04)  COMP.
               05  IN-COL                   PIC  S9(04)  COMP.

      *---------------------------------------------------------------*
      *            TELA   RUECT820      -        OUTPUT               *
      *---------------------------------------------------------------*

       01  AREA-OUTPUT.
ACT137* ***03 OU-LL                         PIC S9(04) COMP VALUE +1092.
BRQ001* ***03 OU-LL                         PIC S9(04) COMP VALUE +1000.
           03 OU-ZZ                         PIC  9(04) COMP VALUE 0.
           03 OU-NAVEGA                     PIC  X(37).
           03 OU-OPCAO                      PIC  9(02).
           03 OU-DATA.
              05 OU-DIA-T820                PIC  9(02).
              05 OU-BARRA1-T820             PIC  X(01).
              05 OU-MES-T820                PIC  9(02).
              05 OU-BARRA2-T820             PIC  X(01).
              05 OU-ANO-T820                PIC  9(04).
           03 OU-CEDULA                     PIC  9(09).
           03 OU-FASE                       PIC  9(01).
           03 OU-CTERM                      PIC  X(08).
           03 OU-PAGATU                     PIC  9(03).
           03 OU-TIMES                      PIC  X(26).
           03 OU-AGENC                      PIC  9(05).
BRQ001     03 OU-LINHA   OCCURS   03 TIMES.
BR1217        05 OU-BANCO                   PIC  X(08).
BR1217        05 WRK-OU-RFBACEN             PIC  X(11).
BR1217        05 WRK-OU-FNALD               PIC  X(07).
BR1217        05 WRK-OU-ATIV                PIC  X(03).
BR1217        05 WRK-OU-DTEM                PIC  X(10).
BR1217        05 OU-VALOR-ALF.
BR1217           07 OU-VALOR                PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
BR1217        05 WRK-OU-FONTE               PIC  X(30) VALUE SPACES.
RLOS          05 WRK-OU-CNPJ-PRO-ALF.
.                07 WRK-OU-CNPJ-PRO         PIC  ZZ.ZZZ.ZZZ.
.             05 WRK-OU-RFBACEN-PRO         PIC  X(11).
.             05 WRK-OU-VALOR-PRO-ALF.
.                07 WRK-OU-VALOR-PRO        PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
RLOS
BRQ001        05 WRK-OU-CNPJ-ENT-RESP-ORIG  PIC  X(18) VALUE SPACES.
BRQ001        05 WRK-OU-CNPJ-PARTC          PIC  X(18) VALUE SPACES.
BR1217     03 WRK-OU-CTRBAC                 PIC  X(20) VALUE SPACES.
ACT137     03 OU-TOTBA-ALF.
ACT137        05 OU-TOTBA                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137     03 OU-TOTBR-ALF.
ACT137        05 OU-TOTBR                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
ACT137     03 OU-TOTRE-ALF.
ACT137        05 OU-TOTRE                   PIC  Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
           03 OU-LITERAL                    PIC  X(33).
           03 OU-RESPTA-ATR                 PIC S9(04) COMP.
           03 OU-RESPTA                     PIC  X(01).

      /---------------------------------------------------------------*
      *  AREA DA POOL0660 COM MASCARA DA TELA RUECT820 - SEGMENTO 00  *
      *---------------------------------------------------------------*
       01  660-RUECT820-SEG00.
ACT137* ***05  660-LL-AREA-T820        PIC  9(04)  COMP  VALUE 0162.
ACT137* ***05  660-LL-MENSAGEM-T820    PIC  9(04)  COMP  VALUE 1092.
BRQ001* ***05  660-LL-AREA-T820        PIC  9(04)  COMP  VALUE 0146.
BRQ001* ***05  660-LL-MENSAGEM-T820    PIC  9(04)  COMP  VALUE 1000.
BRQ001     05  660-LL-AREA-T820        PIC  9(04)  COMP  VALUE 0114.
BRQ001     05  660-LL-MENSAGEM-T820    PIC  9(04)  COMP  VALUE 0828.
           05  660-SENHA-T820          PIC  9(04)  COMP  VALUE 0037.
           05  660-OPCAO-T820          PIC  9(04)  COMP  VALUE 0002.
           05  660-DATA-T820           PIC  9(04)  COMP  VALUE 0010.
           05  660-CEDULA-T820         PIC  9(04)  COMP  VALUE 0009.
           05  660-FASE-T820           PIC  9(04)  COMP  VALUE 0001.
           05  660-CTERM-T820          PIC  9(04)  COMP  VALUE 0008.
           05  660-PAGATU-T820         PIC  9(04)  COMP  VALUE 0003.
           05  660-TIMES-T820          PIC  9(04)  COMP  VALUE 0026.
           05  660-AGENCIA-T820        PIC  9(04)  COMP  VALUE 0005.
BRQ001     05  660-TABELA-T820         OCCURS      03    TIMES.
BR1217         07  660-REFBACEN-820    PIC  9(04)  COMP  VALUE 0011.
BR1217         07  660-FNALD-T820      PIC  9(04)  COMP  VALUE 0007.
BR1217         07  660-ATIV-T820       PIC  9(04)  COMP  VALUE 0003.
BR1217         07  660-DTEMI-T820      PIC  9(04)  COMP  VALUE 0010.
BR1217         07  660-DTVCT-T820      PIC  9(04)  COMP  VALUE 0010.
BR1217         07  660-VALOR-T820      PIC  9(04)  COMP  VALUE 0020.
BR1217         07  660-FONTE-T820      PIC  9(04)  COMP  VALUE 0030.
RLOS           07  660-CNPJ-PRO-T820   PIC  9(04)  COMP  VALUE 0010.
RLOS           07  660-REFBACEN-PRO-T820
                                       PIC  9(04)  COMP  VALUE 0011.
RLOS           07  660-VLR-PRO-T820    PIC  9(04)  COMP  VALUE 0020.
BRQ001         07  660-CNPJERO-T820    PIC  9(04)  COMP  VALUE 0018.
BRQ001         07  660-CNPJPAR-T820    PIC  9(04)  COMP  VALUE 0018.
BR1217     05  660-CTRBAC-T820         PIC  9(04)  COMP  VALUE 0020.
ACT137     05  660-TOTBA-T820          PIC  9(04)  COMP  VALUE 0020.
ACT137     05  660-TOTBR-T820          PIC  9(04)  COMP  VALUE 0020.
ACT137     05  660-TOTRE-T820          PIC  9(04)  COMP  VALUE 0020.
           05  660-LITERAL             PIC  9(04)  COMP  VALUE 0033.
           05  660-RESPTA              PIC  9(04)  COMP  VALUE 2003.
           05  660-MENS-T820           PIC  9(04)  COMP  VALUE 0079.

      * MENSAGENS DE ERRO PARA SEREM UTILIZADAS NO PROGRAMA *
      *-----------------------------------------------------*

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
           'PRAZO INVALIDO'.
       01  WRK-MSG06.
           05  FILLER              PIC X(79)        VALUE
           'FINALIDADE NAO ENCONTRADA'.
           05  FILLER              PIC X(79)        VALUE
           'FINAL DE AMOSTRAGEM'.
       01  WRK-MSG08.
           05  FILLER              PIC X(79)        VALUE
           'VALOR INVALIDO, OU IGUAL A ZEROS'.
       01  WRK-MSG09.
           05  FILLER              PIC X(79)        VALUE
           'CODIGO DO BANCO INVALIDO'.
       01  WRK-MSG10.
           05  FILLER              PIC X(79)        VALUE
           'DIGITE <PF11> PARA CONFIRMAR OU <ENTER> PARA DESISTIR'.
       01  WRK-MSG11.
           05  FILLER              PIC X(79)        VALUE
           'DESCRICAO DA RESPONSABILIDADE INVALIDO'.
       01  WRK-MSG12.
           05  FILLER              PIC X(79)        VALUE
           'CODIGO DA RESPONSABILIDADE INVALIDO'.
       01  WRK-MSG13.
           05  FILLER              PIC X(79)        VALUE
           'CODIGO DO EMPRESTIMO INVALIDO'.
       01  WRK-MSG14.
           'TECLE PF9 PESQUISAR CODIGO E DESCRICAO EXISTENTES'.
       01  WRK-MSG15.
           05  FILLER              PIC X(79)        VALUE
           'ALTERACAO EFETUADA'.
       01  WRK-MSG16.
           05  FILLER              PIC X(79)        VALUE
           'ALTERACOES NAO EFETUADAS'.
       01  WRK-MSG17.
           05  FILLER              PIC X(79)        VALUE
           'POSICIONE CURSOR NO CODIGO OU DESCRICAO DA FINALIDADE E TECL
      -    'E <PF9>'.
       01  WRK-MSG18.
           05  FILLER              PIC X(79)        VALUE
           'NAO FORAM INFORMADAS RESPONSABILIDADES'.
       01  WRK-MSG19.
           05  FILLER              PIC X(79)        VALUE
           'ERRO NO DB DE FINALIDADE DESCR. INEXISTENTE'.
       01  WRK-MSG20.
           05  FILLER              PIC X(79)        VALUE
           'NAO HA PAGINA POSTERIOR'.
       01  WRK-MSG21.
           'DIGITE OS DADOS E TECLE <ENTER> PARA INCLUIR '.
       01  WRK-MSG22.
           05  FILLER              PIC X(79)        VALUE
           'ALTERE OS DADOS E TECLE <ENTER>'.
       01  WRK-MSG23.
           05  FILLER              PIC X(79)        VALUE
           'NAO HA PAGINA ANTERIOR'.
       01  WRK-MSG24.
           05  FILLER              PIC X(27)        VALUE
           'ERRO NO CONTROLE DE FASE - '.
           05  WRK-FASE-ERRO       PIC X(02).

       01  WRK-MSG25.
           05  FILLER              PIC X(79)        VALUE
           'INCLUA RESPONSABILIDADES E TECLE <ENTER>'.

       01  WRK-MSG26.
           05  FILLER              PIC X(79)        VALUE
           'ALTERE OS DADOS E TECLE <ENTER> PARA PROCESSAR OU <PF11> PAR
      -    'A FINALIZAR'.

           05  FILLER              PIC X(79)        VALUE
           'PARA INCLUSAO DE MAIS RESPONSABILIDADES, DIGITE "S" OU "N"'.

ACT137 01  WRK-MSG28.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'CODIGO DO PRODUTO INVALIDO'.
ACT137
ACT137 01  WRK-MSG29.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'DATA DA CEDULA INVALIDA'.
ACT137
ACT137 01  WRK-MSG30.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'DATA DO VENCIMENTO INVALIDA'.
ACT137
ACT137 01  WRK-MSG45.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'CODIGO DO BANCO DEVE SER DE OUTROS BANCOS    '.
ACT137
ACT137 01  WRK-MSG46.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137
ACT137 01  WRK-MSG47.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'DATA DA CEDULA MAIOR QUE DATA CORRENTE'.
ACT137
ACT137 01  WRK-MSG48.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'DATA DA VENCIMENTO MENOR QUE DATA CORRENTE'.
ACT137
ACT137 01  WRK-MSG49.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'DATA DA CEDULA IGUAL A DATA DO VENCIMENTO'.
ACT137
ACT137 01  WRK-MSG50.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'REGISTRO COM DUPLICIDADE (BANCO, CEDULA E VALOR) NA TELA'.
ACT137
ACT137 01  WRK-MSG51.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'REGISTRO COM DUPLICIDADE (BANCO, CEDULA E VALOR) NA TABELA'.
ACT137
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'CODIGO DO BANCO NAO PODE SER ALTERADO'.
ACT137
ACT137 01  WRK-MSG53.
ACT137     05  FILLER              PIC X(79)        VALUE
ACT137     'POSICIONE CURSOR NA LINHA DO BANCO E TECLE <PF9>'.
ACT137
BR1217/---------------------------------------------------------------*
BR1217*             LAY-OUT TABELA                                    *
BR1217*---------------------------------------------------------------*
BR1217
BR1217     EXEC SQL    INCLUDE  CRURB016      END-EXEC.
BR1217
BR1217/---------------------------------------------------------------*
BR1217*             LAY-OUT TABELA                                    *
BR1217*---------------------------------------------------------------*
BR1217
BR1217     EXEC SQL    INCLUDE  CRURB002      END-EXEC.
BR1217
BR1217/---------------------------------------------------------------*
BR1217*             LAY-OUT TABELA                                    *
BR1217
BR1217     EXEC SQL    INCLUDE  RURCB028      END-EXEC.
BR1217
      /---------------------------------------------------------------*
      *             LAY-OUT TABELA                                    *
      *---------------------------------------------------------------*

           EXEC SQL    INCLUDE  RUECV007      END-EXEC.

      /---------------------------------------------------------------*
      *             LAY-OUT TABELA                                    *
      *---------------------------------------------------------------*

           EXEC SQL    INCLUDE  RUECB046      END-EXEC.

      /---------------------------------------------------------------*
      *             LAY-OUT TABELA                                    *
      *---------------------------------------------------------------*

           EXEC SQL    INCLUDE  RUECB09C      END-EXEC.

      *             LAY-OUT TABELA                                    *
      *---------------------------------------------------------------*
           EXEC SQL    INCLUDE  RUECB042      END-EXEC.

      *---------------------------------------------------------------*
      *                     SQLCA  DO PGM                             *
      *---------------------------------------------------------------*

           EXEC SQL    INCLUDE   SQLCA        END-EXEC.

BR1217/---------------------------------------------------------------*
BR1217*      CURSOR PARA ACESSAR TABELA DE RESPONSABILIDADE           *
BR1217*---------------------------------------------------------------*
BR1217
BR1217     EXEC SQL
BR1217       DECLARE CSR01-RUECB046 CURSOR FOR
BR1217       SELECT
BR1217         CBCO
BR1217       , VPCELA_COOP_RURAL
BR0918       , VFINAN
BR1217       , CPRODT_CREDT_RURAL
BR1217       , DVCTO_CREDT_RURAL
BR1217       , CEMPTO_BACEN
BR1217       , CREFT_RECOR
BR1217       , CTPO_FONTE_RECOR
RLOS         , VALUE (CCNPJ_BASE_MUTUA, 0)
RLOS         , VALUE (CFLIAL_BASE_MUTUA, 0)
RLOS         , VALUE (CCTRL_BASE_MUTUA, 0)
RLOS         , VALUE (NREFT_BACEN_INVES, 0)
RLOS         , VALUE (VPCELA_INVES_RURAL, 0)
BRQ001       , VALUE (CCNPJ_ENTID_RESP,  0)
BRQ001       , VALUE (CFLIAL_CNPJ_ENTID, 0)
BRQ001       , VALUE (CCTRL_CNPJ_ENTID,  0)
BRQ001       , VALUE (CCNPJ_PARTC,       0)
BRQ001       , VALUE (CFLIAL_CNPJ_PARTC, 0)
BRQ001       , VALUE (CCTRL_CNPJ_PARTC,  0)
BR1217       FROM  DB2PRD.RESP_CONTR_RURAL
BR1217       WHERE CAG_BCRIA           = :RUECB046.CAG-BCRIA
BR1217       AND   CCDULA              = :RUECB046.CCDULA
BR1217       ORDER BY
BR1217         CSEQ_RESP_RURAL
BR1217     END-EXEC.
      /---------------*
       LINKAGE                     SECTION.
      *---------------*

       01  IO-PCB.
           03  IO-TERM                 PIC X(08).
           03  FILLER                  PIC X(02).
           03  IO-STATUS               PIC X(02).
           03  FILLER                  PIC X(12).
           03  IO-MODNAME              PIC X(08).

       01  ALT-PCB.
           03  ALT-TERM                PIC X(08).
           03  FILLER                  PIC X(02).
           03  ALT-STATUS              PIC X(02).
           03  FILLER                  PIC X(12).
           03  ALT-MODNAME             PIC X(08).


      /---------------------------------------------------------------*
       PROCEDURE      DIVISION.

           ENTRY      'DLITCBL'        USING     IO-PCB   ALT-PCB.

           MOVE       'GU'             TO        WRK-FUNCAO.

      *---------------------------------------------------------------*
       A100-PROCESSA-TRANSACAO     SECTION.
      *---------------------------------------------------------------*

           CALL 'POOL5000' USING       WRK-FUNCAO
                                       IO-PCB          ALT-PCB
                                       WRK-MENSAGEM    WRK-MODNAME
                                       WRK-NOVA-OPCAO  WRK-COD-USER
                                       WRK-OPCAO-ATUAL WRK-COD-DEPTO.
           IF     RETURN-CODE = 04  OR
                  RETURN-CODE = 12
                  GOBACK.

           PERFORM M200-LIMPA-TELA
                   VARYING WRK-IND  FROM     1 BY 1
                   UNTIL   WRK-IND  GREATER  WRK-OCOR.
           MOVE   WRK-MENSAGEM     TO             AREA-INPUT.
           MOVE   IN-NAVEGA        TO             OU-NAVEGA.
           MOVE   'RUECT820'       TO             WRK-MODNAME.
ACT137     CALL   'POOL7600'    USING             WRK-DATA-HORA

           IF  WRK-TRANSACAO        EQUAL  'RUEC8200'
               PERFORM              A300-ROTINA-PRINCIPAL
           ELSE
               PERFORM              A200-RECEBER-CONTROLE.

           IF  WRK-MODNAME          EQUAL  'RUECT820'
               PERFORM              Z100-POOL0660-RUECT820.

           GO          TO           A100-PROCESSA-TRANSACAO.

      /------------------------------------------------------------*
       A200-RECEBER-CONTROLE       SECTION.
      *------------------------------------------------------------*

      **-- RECEBE CONTROLE DOS PROGRAMA  RUEC8140

           MOVE  COMU-SENHA      TO   OU-NAVEGA.

           MOVE  IO-TERM         TO   OU-CTERM
                                      IN-CTERM.

           MOVE 1                TO   OU-PAGATU
                                      IN-PAGATU

           PERFORM 2500-SELECT-RUECB042.

           MOVE AGENCIA OF NAVEGACAO-RURAL  TO IN-AGENC
                                               OU-AGENC
           MOVE CCDULA  OF NAVEGACAO-RURAL  TO IN-CEDULA
                                               OU-CEDULA
           PERFORM 2600-SELECT-RUECV007

           MOVE  HULT-ATULZ OF RUECV007     TO IN-TIMES

BR1217     PERFORM 1600-OBTER-RESP.

       A200-99-FIM.            EXIT.
      /---------------------------------------------------------------*
       A300-ROTINA-PRINCIPAL       SECTION.
      *---------------------------------------------------------------*

           PERFORM 2500-SELECT-RUECB042

BR1217     EVALUATE  IN-PFK
BR1217        WHEN '*'
BR1217           MOVE WRK-MSG04 TO OU-MENSAGEM
BR1217        WHEN 'C'
BR1217         PERFORM  D100-PF3-MENU-ALTERACAO
BR1217        WHEN '.'
BR1217           IF IN-FASE =  1
BR1217              PERFORM 1700-TRATAR-RETORNO
BR1217           END-IF
BR1217        WHEN '7'
BR1217           IF IN-FASE =  8
BR1217              PERFORM E300-VOLTAR-PAGINA
BR1217           END-IF
BR1217        WHEN '8'
BR1217           IF IN-FASE =  2
BR1217           END-IF
BR1217        WHEN 'B'
BR1217           PERFORM 8888-ACESSA-RUEC8500
BR1217     END-EVALUATE.

           PERFORM M100-MANTEM-TELA.

       A300-EXIT.              EXIT.

      /---------------------------------------------------------------*
       D100-PF3-MENU-ALTERACAO     SECTION.
      *---------------------------------------------------------------*

           MOVE  WRK-CHNG             TO    WRK-FUNCAO.
           MOVE  +58                  TO    AREA-LL.
           MOVE  'RUEC8200'           TO    AREA-TRANSACAO.
           MOVE  IN-NAVEGA            TO    AREA-SENHA.
           MOVE  IN-CTERM             TO    AREA-CTERM.
           MOVE  'RUEC8710'           TO    WRK-MODNAME.
           MOVE  SPACES               TO    AREA-STATUS
                                            AREA-MENSAGEM.

       D100-99-FIM.            EXIT.

      *---------------------------------------------------------------*
       E300-VOLTAR-PAGINA          SECTION.
      *---------------------------------------------------------------*

BR1217     IF (IN-PAGATU          EQUAL 1         )
BR1217        MOVE  WRK-MSG23     TO   OU-MENSAGEM
BR1217     ELSE
BR1217        MOVE IN-PAGATU      TO WRK-PAG
BR1217        SUBTRACT 1          FROM WRK-PAG
BR1217        MOVE    WRK-PAG     TO IN-PAGATU
BR1217                               OU-PAGATU
BR1217        MOVE IN-PAGATU      TO  WRK-PAG-HEX
BR1217
BRQ001        COMPUTE WRK-DESPREZA = (( WRK-PAG-HEX - 1 ) * 03 ) + 1
BR1217        PERFORM E400-MONTAR-TELA
BR1217     END-IF.

       E300-EXIT.              EXIT.
      *---------------------------------------------------------------*
       E400-MONTAR-TELA            SECTION.
      *---------------------------------------------------------------*

           PERFORM M200-LIMPA-TELA
                   VARYING IND-1   FROM     1 BY 1
                   UNTIL   IND-1   GREATER  WRK-OCOR

BR1217     PERFORM  1851-SELECT-COUNT-RUECB046
BR1217
BR1217*==> ABRE CURSOR DE ACORDO COM A ESPECIE DIGITADA
BR1217     PERFORM  1852-ABRIR-CSR01-RUECB046
BR1217
BR1217*==> DESPREZA AS OCORRENCIAS JA APRESENTADAS
BR1217     PERFORM  1853-LER-CSR01-RUECB046
BR1217
BR1217     MOVE ZEROS                      TO IND-1
BR1217                                        IND-2
BR1217
BR1217     MOVE WRK-DESPREZA               TO WRK-ACUM-OCORR
BR1217     PERFORM 1854-PROCES-CSR-RUECB046  VARYING
BR1217                 WRK-FIM-CSR-RUECB046 EQUAL 'S'   OR
BRQ001                 IND-1                GREATER 02  OR
BR1217                 IND-2                GREATER WRK-COUNT
BR1217
BR1217
BR1217     PERFORM 1855-FECHAR-CSR01-RUECB046.
BR1217     PERFORM  2700-UPDATE-RUECB042.

       E400-EXIT.              EXIT.

      /---------------------------------------------------------------*
       M100-MANTEM-TELA            SECTION.
      *---------------------------------------------------------------*

           MOVE  IN-CEDULA               TO OU-CEDULA.
           MOVE  IN-AGENC                TO OU-AGENC.
           MOVE  IN-FASE                 TO OU-FASE.
           MOVE  IN-CTERM                TO OU-CTERM
           MOVE  IN-TIMES                TO OU-TIMES.
           MOVE  IN-PAGATU               TO OU-PAGATU.
           MOVE  WRK-IN-CTRBAC           TO WRK-OU-CTRBAC
           PERFORM  2900-TOTAL-BANCOS
           PERFORM  2950-TOTAL-BRADESCO
           ADD WRK-VFINAN-TELA  TO  WRK-VFINAN-BCO
           COMPUTE WRK-VFINAN-TOT = WRK-VFINAN-BCO
                                  + WRK-VFINAN-BRD
           MOVE WRK-VFINAN-BCO  TO  IN-TOTBA  OU-TOTBA
           MOVE WRK-VFINAN-BRD  TO  IN-TOTBR  OU-TOTBR
           MOVE WRK-VFINAN-TOT  TO  IN-TOTRE  OU-TOTRE.

           PERFORM M150-MANTEM-OCOR
                   VARYING WRK-IND FROM    1 BY 1
                   UNTIL   WRK-IND GREATER WRK-OCOR.
       M100-EXIT.              EXIT.

      /---------------------------------------------------------------*
       M150-MANTEM-OCOR        SECTION.
      *---------------------------------------------------------------*

           MOVE  IN-BANCO       (WRK-IND) TO OU-BANCO       (WRK-IND).
           MOVE  WRK-IN-RFBACEN (WRK-IND) TO WRK-OU-RFBACEN (WRK-IND).
           MOVE  WRK-IN-FNALD   (WRK-IND) TO WRK-OU-FNALD   (WRK-IND).
           MOVE  WRK-IN-DTEM    (WRK-IND) TO WRK-OU-DTEM    (WRK-IND).
           MOVE  WRK-IN-DTVCTO  (WRK-IND) TO WRK-OU-DTVCTO  (WRK-IND).
           MOVE  IN-VALOR-ALF   (WRK-IND) TO OU-VALOR-ALF   (WRK-IND).
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
      *---------------------------------------------------------------*

      /---------------------------------------------------------------*
       M200-LIMPA-TELA             SECTION.
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
RLOS                          WRK-IN-RFBACEN-PRO(WRK-IND)
.                             WRK-OU-RFBACEN-PRO(WRK-IND)
.                             WRK-IN-CNPJ-PRO-ALF(WRK-IND)
.                             WRK-OU-CNPJ-PRO-ALF(WRK-IND)
.                             WRK-IN-VALOR-PRO-ALF(WRK-IND)
RLOS                          WRK-OU-VALOR-PRO-ALF(WRK-IND)
BRQ001                        WRK-IN-CNPJ-PARTC(WRK-IND)
BRQ001                        WRK-OU-CNPJ-PARTC(WRK-IND)
BRQ001                   WRK-IN-CNPJ-ENT-RESP-ORIG(WRK-IND)
BRQ001                   WRK-OU-CNPJ-ENT-RESP-ORIG(WRK-IND).

       M200-EXIT.              EXIT.

      /---------------------------------------------------------------*
       Z100-POOL0660-RUECT820      SECTION.
      *---------------------------------------------------------------*

           MOVE LENGTH OF AREA-OUTPUT  TO  OU-LL
                                           660-LL-MENSAGEM-T820.
           MOVE LENGTH OF 660-RUECT820-SEG00
                                       TO  660-LL-AREA-T820.
           CALL   'POOL7600'           USING       WRK-DATA-HORA.

           COMPUTE OU-DIA-T820 = WRK-DT-AAAAMMDD.
           COMPUTE OU-MES-T820 = WRK-DT-AAAAMMDD / 100.
           COMPUTE OU-ANO-T820 = WRK-DT-AAAAMMDD / 10000.

           MOVE   '/'                  TO          OU-BARRA1-T820
                                                   OU-BARRA2-T820.

           CALL 'POOL0660'             USING     AREA-OUTPUT
                                                 660-RUECT820-SEG00.

           IF RETURN-CODE              NOT       EQUAL ZEROS
              MOVE 'APL'               TO        ERR-TIPO-ACESSO
              MOVE RETURN-CODE         TO        WRK-RETURN-CODE
              MOVE 16                  TO        WRK-LOCAL-ERRO
              MOVE WRK-ERRO-POOL0660   TO        ERR-TEXTO
              GO                       TO        9999-ROTINA-ERRO.

           MOVE 'RUECT820'             TO        WRK-MODNAME.
           MOVE 'ISRT'                 TO        WRK-FUNCAO.

       Z100-EXIT.              EXIT.

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
BR1217
BR1217     IF TIPO-PESSOA            EQUAL 'J'
BR1217        MOVE NUMCGC            TO WRK-NUMCGC
BR1217        MOVE FILIAL            TO WRK-FILIAL
BR1217        MOVE CTRL-CGC          TO WRK-CTRL-CGC
BR1217     ELSE
BR1217        MOVE NUMCPF            TO WRK-NUMCPF
BR1217        MOVE ZEROS             TO WRK-FILLER
BR1217     END-IF.
BR1217
           MOVE            1         TO WRK-NUM-ENVIO

BR1217     CALL WRK-MODULO-RUEC8840  USING  WRK-LKG-AREA
BR1217                                      ERRO-AREA.
BR1217
BR1217     IF WRK-COD-RET      EQUAL ZEROS
7C0625       INITIALIZE WRK-LKG-AREA
.
.            MOVE 'RUEC7200'           TO WRK-PGM-CHAMADOR
.            MOVE IN-AGENC             TO WRK-AGENCIA
.            MOVE IN-CEDULA            TO WRK-CEDULA
.            MOVE TIPO-PESSOA          TO WRK-TIPO-PESSOA
.
.            IF TIPO-PESSOA            EQUAL 'J'
.               MOVE NUMCGC            TO WRK-NUMCGC
.               MOVE FILIAL            TO WRK-FILIAL
.               MOVE CTRL-CGC          TO WRK-CTRL-CGC
.            ELSE
.               MOVE NUMCPF            TO WRK-NUMCPF
.               MOVE CTRL-CPF          TO WRK-CTRL-CPF
.            END-IF
.
.            MOVE            2         TO WRK-NUM-ENVIO
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
BR1811           DELIMITED BY SIZE   INTO  WRK-TIMES-1S
BR1811     END-PERFORM
.            CALL WRK-MODULO-RUEC8840  USING  WRK-LKG-AREA
.
.            IF WRK-COD-RET      EQUAL ZEROS
.               MOVE WRK-CONTROLE-BACEN
.                                      TO     WRK-IN-CTRBAC
.                                             WRK-OU-CTRBAC
.               MOVE 'TECLE <ENTER> PARA INCLUIR A RESPONSABILIDADE'
.                                     TO     OU-MENSAGEM
.               MOVE    1             TO     IN-FASE
.                                            OU-FASE
.            ELSE
.               MOVE WRK-MENSAGEM-R    TO OU-MENSAGEM
7C0625       END-IF
           ELSE
              MOVE WRK-MENSAGEM-R    TO OU-MENSAGEM
BR1217
BR1217     END-IF.
BR1217
BR1217 1600-99-FIM. EXIT.
BR1217
BR1217/------------------------------------------------------------*
BR1217 1700-TRATAR-RETORNO           SECTION.
BR1217
BR1217     MOVE WRK-IN-CTRBAC        TO CCTRL-RQUIS-FINCR OF RUECB09C.
BR1217     MOVE 'COR0006R1'          TO WRK-CMSGEM-RECOR
BR1217     MOVE 'COR0006E'           TO WRK-CMSGEM-RECOR-E
BR1217     MOVE ZEROS                TO NMNTRC-MSGEM-RURAL
BR1217                                                    OF RUECB09C.
BR1217     EXEC SQL
BR1217       SELECT NMNTRC_MSGEM_RURAL
BR1217        INTO :RUECB09C.NMNTRC-MSGEM-RURAL
BR1217       FROM  DB2PRD.TMNTRC_MSGEM_RURAL
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
BR1217           MOVE '1700'                   TO   ERR-LOCAL
BR1217           MOVE SPACES                   TO   ERR-SEGM
BR1217           PERFORM   9999-ROTINA-ERRO
BR1217     END-IF.
BR1217
BR1217     IF SQLCODE                          EQUAL +100
BR1217        MOVE 'AGUARDANDO RETORNO DO BACEN  - TECLE ENTER '
BR1217                                         TO OU-MENSAGEM
BR1217        MOVE  1                          TO IN-FASE
BR1217                                            OU-FASE
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
BR1217                                      TO OU-MENSAGEM
BR1217        MOVE  2                       TO IN-FASE
BR1217                                         OU-FASE
BR1217        GO TO 1700-99-FIM
BR1217     END-IF.
BR1217
BR1217     PERFORM 1851-SELECT-COUNT-RUECB046
BR1217     IF WRK-COUNT             EQUAL ZEROS
BR1217        MOVE 'NAO HA RESPONSABILIDADE PARA DEMONSTRAR'
BR1217                                      TO OU-MENSAGEM
BR1217        MOVE  2                       TO IN-FASE
BR1217                                         OU-FASE
BR1217        GO TO 1700-99-FIM
BR1217     END-IF.
BR1217     PERFORM 1852-ABRIR-CSR01-RUECB046
BR1217     PERFORM 1853-LER-CSR01-RUECB046
BR1217     MOVE ZEROS                  TO IND-1
BR1217                                    IND-2
BR1217
BR1217     MOVE 1                      TO WRK-DESPREZA
BR1217     MOVE WRK-DESPREZA           TO WRK-ACUM-OCORR
BR1217     PERFORM 1854-PROCES-CSR-RUECB046  VARYING
BR1217               WRK-DESPREZA      FROM WRK-DESPREZA BY 1 UNTIL
BR1217               WRK-FIM-CSR-RUECB046 EQUAL 'S'   OR
BRQ001               IND-1                GREATER 02  OR
BR1217                  IND-2                GREATER WRK-COUNT
BR1217
BR1217        PERFORM 1855-FECHAR-CSR01-RUECB046
BR1217        PERFORM 2700-UPDATE-RUECB042
BR1217        MOVE  2                          TO IN-FASE
BR1217                                            OU-FASE.
BR1217
BR1217 1700-99-FIM. EXIT.
BR1217
BR1217*---------------------------------------------------------------*
BR1217 1800-AVANCAR-PAGINA         SECTION.
BR1217*---------------------------------------------------------------*
BR1217
BR1217     PERFORM 1851-SELECT-COUNT-RUECB046
BR1217
BR1217     PERFORM 1852-ABRIR-CSR01-RUECB046.
BR1217
BR1217
BR1217     MOVE IN-PAGATU              TO WRK-PAG
BR1217     ADD  1                      TO WRK-PAG
BR1217     MOVE WRK-PAG                TO IN-PAGATU
BR1217                                    OU-PAGATU
BR1217
BR1217     MOVE IN-PAGATU              TO WRK-PAG-HEX.
BR1217
BRQ001     COMPUTE WRK-DESPREZA  = ((WRK-PAG-HEX - 1 ) * 03 ) + 1.
BR1217
BR1217     MOVE ZEROS                      TO IND-1
BR1217                                        IND-2
BR1217
BR1217     MOVE WRK-DESPREZA               TO WRK-ACUM-OCORR
BR1217
BR1217     PERFORM 1854-PROCES-CSR-RUECB046  VARYING
BR1217                 WRK-DESPREZA         FROM WRK-DESPREZA BY 1 UNTIL
BR1217                 WRK-FIM-CSR-RUECB046 EQUAL 'S'   OR
BRQ001                 IND-1                GREATER 02  OR
BR1217                 IND-2                GREATER WRK-COUNT
BR1217
BR1217
BR1217     IF IND-1                         EQUAL ZEROS
BR1217
BR1217        MOVE 'NAO HA MAIS PAGINAS PARA AVANCAR'
BR1217                                TO OU-MENSAGEM
BR1217        MOVE IN-PAGATU          TO WRK-PAG
BR1217        SUBTRACT 1              FROM WRK-PAG
BR1217        MOVE WRK-PAG            TO IN-PAGATU
BR1217                                   OU-PAGATU
BR1217        GO TO 1800-99-FIM
BR1217     END-IF.
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
RLOS                              WRK-IN-RFBACEN-PRO(IND-1)
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
BR1217
BR1217     PERFORM 2700-UPDATE-RUECB042.
BR1217
BR1217 1800-99-FIM.            EXIT.
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
BR1217
BR1217     IF (SQLCODE                 NOT EQUAL ZEROS )
BR1217     OR (SQLWARN0                EQUAL 'W'       )
BR1217         MOVE 'DB2'              TO   ERR-TIPO-ACESSO
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
BR1217
BR1217     EXEC SQL
BR1217           OPEN   CSR01-RUECB046
BR1217     END-EXEC.
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
BR1217 1853-LER-CSR01-RUECB046          SECTION.
BR1217*---------------------------------------------------------------*
BR1217
BR1217     EXEC SQL
BR1217       FETCH CSR01-RUECB046
BR1217       INTO
BR1217         :RUECB046.CBCO
BR1217       , :RUECB046.VPCELA-COOP-RURAL
BR0918       , :RUECB046.VFINAN
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
BRQ001       , :RUECB046.CCNPJ-ENTID-RESP
BRQ001       , :RUECB046.CCTRL-CNPJ-ENTID
BRQ001       , :RUECB046.CCNPJ-PARTC
BRQ001       , :RUECB046.CFLIAL-CNPJ-PARTC
BRQ001       , :RUECB046.CCTRL-CNPJ-PARTC
BR1217     END-EXEC.
BR1217
BR1217     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100)   OR
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
BR1217 1853-99-FIM.                   EXIT.
BR1217*---------------------------------------------------------------*
BR1217*---------------------------------------------------------------*
BR1217 1854-PROCES-CSR-RUECB046       SECTION.
BR1217*---------------------------------------------------------------*
BR1217
BR1217     MOVE CEMPTO-BACEN          OF RUECB046
BR1217                                TO WRK-EMPREEND-RECOR.
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
BR1217                                   OU-BANCO         (IND-1)
BR1217        END-IF
BR0918        IF VPCELA-COOP-RURAL  OF RUECB046 EQUAL ZEROS
BR0918           MOVE VFINAN          OF RUECB046
BR1217                                TO IN-VALOR         (IND-1)
BR1217                                   OU-VALOR         (IND-1)
BR0918        ELSE
BR1217           MOVE VPCELA-COOP-RURAL  OF RUECB046
BR1217                                TO IN-VALOR         (IND-1)
BR1217                                   OU-VALOR         (IND-1)
BR0918        END-IF
BR1217        MOVE DCDULA-CREDT-RURAL OF RUECB046
BR1217                                TO WRK-IN-DTEM      (IND-1)
BR1217                                   WRK-OU-DTEM      (IND-1)
BR1217        MOVE DVCTO-CREDT-RURAL  OF RUECB046
BR1217                                TO WRK-IN-DTVCTO    (IND-1)
BR1217                                   WRK-OU-DTVCTO    (IND-1)
BR1217        MOVE CREFT-RECOR        OF RUECB046
BR1217                                TO WRK-IN-RFBACEN   (IND-1)
BR1217                                   WRK-OU-RFBACEN   (IND-1)
BR1217
BR1217        MOVE IATVDD-RURAL       OF CRURB002(01:03)
BR1217                                TO WRK-IN-ATIV      (IND-1)
BR1217                                   WRK-OU-ATIV      (IND-1)
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
                                      TO WRK-REF-BACEN-9
.             MOVE WRK-REF-BACEN-X    TO WRK-IN-RFBACEN-PRO (IND-1)
.                                        WRK-OU-RFBACEN-PRO (IND-1)
.
.             MOVE VPCELA-INVES-RURAL OF RUECB046
RLOS                                     WRK-OU-VALOR-PRO   (IND-1)

BRQ001        IF CFLIAL-CNPJ-ENTID OF RUECB046 EQUAL ZEROS
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
BRQ001                          WRK-IN-CNPJ-ENT-RESP-ORIG(IND-1)
BRQ001                          WRK-OU-CNPJ-ENT-RESP-ORIG(IND-1)
BRQ001        ELSE
BRQ001           MOVE CCNPJ-ENTID-RESP OF RUECB046
BRQ001                                TO WRK-CNPJERO-BASE-CS
BRQ001           MOVE WRK-CNPJERO-BASE-TELA1
BRQ001                                TO WRK-CNPJERO-BASE1
BRQ001                                TO WRK-CNPJERO-BASE2
BRQ001           MOVE WRK-CNPJERO-BASE-TELA3
BRQ001                                TO WRK-CNPJERO-BASE3
BRQ001           MOVE CFLIAL-CNPJ-ENTID OF RUECB046
BRQ001                                TO WRK-CNPJERO-FILI-N
BRQ001           MOVE CCTRL-CNPJ-ENTID OF RUECB046
BRQ001                                TO WRK-CNPJERO-CTRL-N
BRQ001           MOVE WRK-CNPJERO-FORM
BRQ001                       TO WRK-IN-CNPJ-ENT-RESP-ORIG(IND-1)
BRQ001                          WRK-OU-CNPJ-ENT-RESP-ORIG(IND-1)
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
BRQ001                                TO WRK-CPFERO-CTRL-N
BRQ001           MOVE WRK-CPFERO-FORM TO WRK-IN-CNPJ-PARTC(IND-1)
BRQ001                                   WRK-OU-CNPJ-PARTC(IND-1)
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
BR1217     END-IF.
BR1217
BR1217*---------------------------------------------------------------*
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
BR1217     END-IF.
BR1217
BR1217*---------------------------------------------------------------*
BR1217 1855-99-FIM.                   EXIT.
BR1217*---------------------------------------------------------------*
BR1217
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
.          END-EXEC
.     *
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
.          OR (SQLWARN0                EQUAL 'W'       )
.              MOVE 'DB2'                TO ERR-TIPO-ACESSO
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
.     *================================================================*
.
.          MOVE WRK-FINAL-RECOR    TO CFNALD-RURAL OF CRURB016
.
.          EXEC SQL
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
.          IF  SQLCODE EQUAL  +100
.              MOVE SPACES TO IFNALD-RURAL OF CRURB016
.          END-IF.
.     *----------------------------------------------------------------*
.      1920-99-FIM.                    EXIT.
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
.            FETCH FIRST 1 ROW ONLY
.          END-EXEC.
.
.          IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0      EQUAL 'W')
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
      /------------------------------------------------------------*
       2500-SELECT-RUECB042        SECTION.
      *------------------------------------------------------------*

           MOVE  IN-CTERM         TO   CTERM OF RUECB042
                                       OU-CTERM.
           EXEC SQL
             SELECT
                   WNAVG_CREDT_RURAL
             INTO
                   :RUECB042.WNAVG-CREDT-RURAL
             FROM   DB2PRD.NAVEGACAO_RURAL
             WHERE
                   CTERM              = :RUECB042.CTERM

           END-EXEC.

           IF SQLCODE  NOT EQUAL  ZEROS  AND  +100
              MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
              MOVE 'NAVEGACAO_RURAL   '     TO   ERR-DBD-TAB
              MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
              MOVE 0020                     TO   ERR-LOCAL
              MOVE SPACES                   TO   ERR-SEGM
              GO                            TO   9999-ROTINA-ERRO.

           MOVE WNAVG-CREDT-RURAL-TEXT  TO  NAVEGACAO-RURAL.

       2500-99-FIM.            EXIT.

      /------------------------------------------------------------*
       2600-SELECT-RUECV007        SECTION.
      *------------------------------------------------------------*

           MOVE IN-AGENC        TO  CJUNC-DEPDC OF RUECV007.
           MOVE IN-CEDULA       TO  CCDULA      OF RUECV007.

           EXEC SQL
             SELECT
                   HULT_ATULZ
             INTO
                   :RUECV007.HULT-ATULZ
             FROM   DB2PRD.V01CEDULA_TEMPR
                   CJUNC_DEPDC        = :RUECV007.CJUNC-DEPDC AND
                   CCDULA             = :RUECV007.CCDULA
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS) OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'CEDULA_TEMPR     '     TO   ERR-DBD-TAB
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0021                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   GO                            TO   9999-ROTINA-ERRO.

       2600-99-FIM.            EXIT.

      /------------------------------------------------------------*
       2700-UPDATE-RUECB042        SECTION.
      *------------------------------------------------------------*

           MOVE 'RUEC8200'       TO  PROGRAMA-ANT
ACT137     MOVE  IN-CEDULA       TO  CCDULA       OF NAVEGACAO-RURAL

           MOVE  IN-CTERM        TO  CTERM OF RUECB042

           MOVE 4000             TO  WNAVG-CREDT-RURAL-LEN.
           MOVE NAVEGACAO-RURAL  TO  WNAVG-CREDT-RURAL-TEXT.

           EXEC SQL
             UPDATE   DB2PRD.NAVEGACAO_RURAL
               SET
                   WNAVG_CREDT_RURAL = :RUECB042.WNAVG-CREDT-RURAL
             WHERE
                   CTERM    = :RUECB042.CTERM
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS  AND  +100)   OR
                 ( SQLWARN0       =    'W'   )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'NAVEGACAO_RURAL   '     TO   ERR-DBD-TAB
                   MOVE 'UPDATE  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE SPACES                   TO   ERR-SEGM
                   GO                            TO   9999-ROTINA-ERRO.

           IF      SQLCODE       EQUAL   +100
              PERFORM       2730-INSERT-RUECB042.

       2700-99-FIM.            EXIT.

      /------------------------------------------------------------*
       2730-INSERT-RUECB042        SECTION.
      *------------------------------------------------------------*

ACT137     MOVE   'RUEC8200'       TO  PROGRAMA-ANT OF NAVEGACAO-RURAL
ACT137     MOVE    IN-AGENC        TO  AGENCIA      OF NAVEGACAO-RURAL
ACT137     MOVE    IN-CEDULA       TO  CCDULA       OF NAVEGACAO-RURAL
           MOVE    NAVEGACAO-RURAL     TO  WNAVG-CREDT-RURAL-TEXT.
           MOVE    4000                TO  WNAVG-CREDT-RURAL-LEN.

           EXEC SQL
             INSERT INTO DB2PRD.NAVEGACAO_RURAL
                  (CTERM,
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
                   MOVE 0023                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   GO                            TO   9999-ROTINA-ERRO.

       2730-99-FIM.            EXIT.

ACT137*---------------------------------------------------------------*
ACT137 2900-TOTAL-BANCOS         SECTION.
ACT137*---------------------------------------------------------------*
ACT137
ACT137                                         WRK-VFINAN-TELA
ACT137     MOVE IN-AGENC                    TO CAG-BCRIA   OF RUECB046
ACT137     MOVE IN-CEDULA                   TO CCDULA      OF RUECB046
BR1217     MOVE 999                         TO CBCO        OF RUECB046
ACT137
BR1217     EXEC SQL
BR1217          SELECT
BR1217            VALUE(SUM(VPCELA_COOP_RURAL), 0)
BR1217          INTO   :WRK-VFINAN-BCO
BR1217          FROM    DB2PRD.RESP_CONTR_RURAL
BR1217          WHERE   CAG_BCRIA           = :RUECB046.CAG-BCRIA
BR1217          AND     CCDULA              = :RUECB046.CCDULA
BR1217          AND     CBCO                = :RUECB046.CBCO
BR1217     END-EXEC
ACT137
ACT137     IF   SQLCODE              NOT EQUAL ZEROS
ACT137     OR   SQLWARN0                EQUAL 'W'
ACT137          MOVE   'DB2'                TO ERR-TIPO-ACESSO
ACT137          MOVE   'RESP_CONTR_RURAL'   TO ERR-DBD-TAB
ACT137          MOVE   'SELECT'             TO ERR-FUN-COMANDO
ACT137          MOVE    SQLCODE             TO ERR-SQL-CODE
ACT137          MOVE    SPACES              TO ERR-SEGM
ACT137          PERFORM 9999-ROTINA-ERRO
ACT137     END-IF
ACT137     .
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
BR1217          WHERE   CAG_BCRIA           = :RUECB046.CAG-BCRIA
BR1217          AND     CCDULA              = :RUECB046.CCDULA
BR1217          AND     CBCO                = :RUECB046.CBCO
BR1217     END-EXEC
BR1217
ACT137     IF  (SQLCODE               NOT EQUAL ZEROS)
ACT137     OR  (SQLWARN0                  EQUAL 'W')
ACT137          MOVE 'DB2'                   TO  ERR-TIPO-ACESSO
ACT137          MOVE 'SELECT'                TO  ERR-FUN-COMANDO
ACT137          MOVE  SQLCODE                TO  ERR-SQL-CODE
ACT137          MOVE 'SEL TOT BRADESCO'      TO  ERR-DBD-TAB
ACT137          MOVE  0028                   TO  ERR-LOCAL
ACT137          MOVE  SPACES                 TO  ERR-SEGM
ACT137          GO                           TO  9999-ROTINA-ERRO
ACT137     END-IF
ACT137     .
ACT137*---------------------------------------------------------------*
ACT137 2950-EXIT.              EXIT.
ACT137*---------------------------------------------------------------*
ACT137
      /---------------------------------------------------------------*
      *---------------------------------------------------------------*

           MOVE AGENCIA OF NAVEGACAO-RURAL  TO LK8-CODIGO-AGENCIA.
           MOVE CCDULA  OF NAVEGACAO-RURAL  TO LK8-CODIGO-CEDULA.
           MOVE  IO-TERM                    TO LK8-CODIGO-TERMINAL.
PW1577     MOVE  05                         TO LK8-NUMORD-CHAMADOR.
           MOVE  SPACES                     TO LK8-STATUS-RETORNO.

           CALL    'POOL0081'      USING  WRK-RUEC8500
                                          IO-PCB
                                          ALT-PCB
                                          LK8-AREA-NAVEGA.

           IF  LK8-STATUS-RETORNO  EQUAL '  '
               MOVE  WRK-CHNG             TO    WRK-FUNCAO
               MOVE  LK8-CODIGO-PROXPGM   TO    WRK-MODNAME
               IF WRK-MODNAME   EQUAL   'RUEC8600'
                  MOVE 'RUEC7890' TO WRK-MODNAME
                  MOVE 'RUEC8200' TO COMU-TRANSACAO
                                                  OF COMU-AREA-IMPRESSAO
                  MOVE ZEROS   TO COMU-OPCAO      OF COMU-AREA-IMPRESSAO
                  MOVE CCDULA  OF NAVEGACAO-RURAL TO COMU-CEDULA
                  MOVE IN-NAVEGA                  TO COMU-SENHAS
                  MOVE COMU-AREA-IMPRESSAO        TO WRK-MENSAGEM
               ELSE
                  MOVE  IN-NAVEGA                 TO AREA-SENHA
                  MOVE  IN-CTERM                  TO AREA-CTERM
                  MOVE  SPACES                    TO AREA-STATUS
                  MOVE  LK8-MENSAGEM              TO AREA-MENSAGEM
                  MOVE  AREA-NAVEGA               TO WRK-MENSAGEM
           ELSE
               IF  LK8-STATUS-RETORNO  EQUAL 'DB'
                   GOBACK
               ELSE
               IF  LK8-STATUS-RETORNO  EQUAL 'ER'
                   MOVE  'ERRO - MODULO DE CONTROLE DE PASSOS'
                                            TO  OU-MENSAGEM.

       8888-99-FIM.            EXIT.

      /---------------------------------------------------------------*
       9999-ROTINA-ERRO            SECTION.

           MOVE 'RUEC8200'             TO        ERR-PGM.
           MOVE WRK-COD-USER           TO        ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO        ERR-COD-DEPTO.
           MOVE 'RUEC8200'             TO        ERR-MODULO.
           CALL 'POOL7100'             USING     IO-PCB ERRO-AREA
                                                 ALT-PCB SQLCA.
           GOBACK.

