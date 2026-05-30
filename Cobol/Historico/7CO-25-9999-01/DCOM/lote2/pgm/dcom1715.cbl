
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1715.
       AUTHOR.     FABRICA-ALTRAN.
      *================================================================*
      *                   A L T R A N                                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  DCOM1715                                     *
      *    ANALISTA....:  FERNANDO LUIZ DE SANTI                       *
      *    DATA........:  11/01/2008                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  INCLUIR ESTORNO DA BAIXA DA PARCELA POR      *
      *      RATEIO - DETALHES.                                        *
      *----------------------------------------------------------------*
      *    TELA........:  DCOMQOTM - INCLUIR ESTORNO DA BAIXA DA       *
      *      PARCELA POR RATEIO - DETALHES.                            *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *

      *    I#BRAD7C - AREA DE COMUNICACAO COM A BRAD7100               *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERE CARACTERES DE NULOS NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS                              *
      *    DCOM5307 - CONSULTAR MOVIMENTO DE BAIXA POR RATEIO.         *
      *    DCOM5311 - INCLUIR MOVIMENTO DE ESTORNO BAIXA RATEIO.       *
      *    POOL5005 - ENVIA / RECEBE MENSAGENS DO IMS                  *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.:   DCOM1702                  CHAMA:  DCOM1716  *
      *                    DCOM1703                                    *
      *                    DCOM1704                                    *
      *================================================================*
BSI001*                     BSI TECNOLOGIA - ALTERACAO                 *
BSI001*----------------------------------------------------------------*
BSI001*    PROGRAMADOR.:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI001*    ANALISTA BSI:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI001*    ANALISTA DDS:  RICARDO JAMMAL     - GRUPO 39                *
BSI001*    DATA........:  10/01/2016                                   *
BSI001*----------------------------------------------------------------*

BSI001*    OBJETIVO....:  INCLUIR TRATAMENTO PARA BAIXA POR PREJUIZO   *
BSI001*                   BSI - 16-0116 - BAIXA AMIGAVEL E JUDICIAL    *
BSI001*================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       77  WRK-MENSA-01                PIC  X(079)         VALUE
           'PF INVALIDA'.
       77  WRK-MENSA-03                PIC  X(079)         VALUE
           'TECLE <PF6> PARA CONFIRMAR OU <PF11> PARA VISUALIZAR O RESUM
      -    'O'.
       77  WRK-MENSA-04                PIC  X(079)         VALUE
           'INCLUSAO EFETUADA COM SUCESSO'.

      *----------------------------------------------------------------*

       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-INCONSISTENCIA          PIC  X(001)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-MENSAGEM-AUX            PIC  X(079)         VALUE SPACES.

       01  WRK-COD-RET                 PIC +9(004)         VALUE ZEROS.
       01  WRK-COD-RET-R               REDEFINES           WRK-COD-RET.
           05  FILLER                  PIC  X(003).
           05  WRK-COD-RET-N           PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       01  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.

       01  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
BSI001 01  WRK-ROLB                    PIC  X(004)         VALUE 'ROLB'.
       01  WRK-COD-USER.
           05  WRK-COD-USER-N          PIC  9(007)         VALUE ZEROS.
       01  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       01  WRK-TELA                    PIC  X(008)         VALUE SPACES.
       01  WRK-VERSAO                  PIC  X(006)         VALUE
           'VRS002'.

       01  WRK-COD-DEPTO.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-COD-DEPTO-N         PIC  9(004)         VALUE ZEROS.

       01  WRK-MENSAGEM.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  WRK-TRANSACAO           PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO ***'.

      *----------------------------------------------------------------*

       01  WRK-COMU-AREA.
           05 WRK-COMU-LL              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-ZZ              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-TRANCODE.
             10 WRK-COMU-TRANSACAO     PIC  X(008)         VALUE SPACES.
             10 FILLER                 PIC  X(006)         VALUE SPACES.
             10 WRK-COMU-PFK           PIC  X(001)         VALUE SPACES.
           05 WRK-COMU-SENHAS          PIC  X(037)         VALUE SPACES.
           05 WRK-COMU-COMANDO         PIC  X(068)         VALUE SPACES.
           05 WRK-COMU-OPCAO.
             10 WRK-COMU-OPCAO-N       PIC  9(001)         VALUE ZEROS.
           05 WRK-COMU-TIMESTAMP       PIC  X(026)         VALUE SPACES.
           05 WRK-COMU-DADOS.
            10 WRK-COMU-OPERACAO      PIC  9(013)         VALUE ZEROS.
            10 FILLER REDEFINES       WRK-COMU-OPERACAO.
              15 WRK-COMU-DANO-OPER   PIC  9(004).
              15 WRK-COMU-NSEQ-OPER   PIC  9(009).
            10 WRK-COMU-PARCELA       PIC  9(005)         VALUE ZEROS.
            10 WRK-COMU-BANCO         PIC  9(003)         VALUE ZEROS.

            10 WRK-COMU-AGENCIA       PIC  9(005)         VALUE ZEROS.
            10 WRK-COMU-CONTA         PIC  9(013)         VALUE ZEROS.
4S2511      10 WRK-COMU-CCNPJ-CPF     PIC  X(09)          VALUE SPACES.
4S2511      10 WRK-COMU-CFLIAL-CNPJ   PIC  X(04)          VALUE SPACES.
            10 WRK-COMU-CCTRL-CNPJCPF PIC  9(002)         VALUE ZEROS.
            10 WRK-COMU-PAGINA-1      PIC  9(004)         VALUE ZEROS.
            10 WRK-COMU-PAGINA-2      PIC  9(004)         VALUE ZEROS.
            10 WRK-COMU-OP-MENU       PIC  9(001)         VALUE ZEROS.
            10 WRK-COMU-TP-SELECAO    PIC  9(001)         VALUE ZEROS.
            10 WRK-COMU-MODAL-BAIXA   PIC  X(001)         VALUE SPACES.
            10 WRK-COMU-MENSAGEM      PIC  X(079)         VALUE SPACES.
            10 WRK-COMU-CHAMADOR      PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

      *----------------------------------------------------------------*

       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO - DCOM5307 ***'.
      *----------------------------------------------------------------*

       01  WRK-5307-ENTRADA.
           05 WRK-5307E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5307E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5307E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5307E-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5307E-START-COUNT    PIC  X(001)         VALUE SPACES.
           05 WRK-5307E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5307E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5307E-DANO-OPER      PIC  9(004)         VALUE ZEROS.
           05 WRK-5307E-NSEQ-OPER      PIC  9(009)         VALUE ZEROS.
           05 WRK-5307E-AGENCIA-FUNC   PIC  9(005)         VALUE ZEROS.
           05 WRK-5307E-FLAG-FUNCAO    PIC  X(001)         VALUE SPACES.

       01  WRK-5307-SAIDA.
           05 WRK-5307S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5307S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5307S-RESTART        PIC  9(005)         VALUE ZEROS.

           05 WRK-5307S-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5307S-QTDE-PARCELAS  PIC  9(005)         VALUE ZEROS.
           05 WRK-5307S-VPRINC-PCELA   PIC  9(015)V99      VALUE ZEROS.
           05 WRK-5307S-VATUAL-PCELA   PIC  9(015)V99      VALUE ZEROS.
           05 WRK-5307S-VEFETV-BAIXA   PIC  9(015)V99      VALUE ZEROS.
           05 WRK-5307S-CINDCD-LOC-DEB PIC  9(001)         VALUE ZEROS.
           05 WRK-5307S-CINDCD-CON-EXT PIC  X(001)         VALUE SPACES.
           05 WRK-5307S-CAG-BCRIA      PIC  9(005)         VALUE ZEROS.
           05 WRK-5307S-CCTA-BCRIA     PIC  9(013)         VALUE ZEROS.
           05 WRK-5307S-CINDCD-BX-COBR PIC  X(001)         VALUE SPACES.
           05 WRK-5307S-RJUSTF-BX-PARC PIC  X(240)         VALUE SPACES.
           05 WRK-5307S-CTPO-BX-PCELA  PIC  X(001)         VALUE SPACES.
           05 WRK-5307S-IRSUMO-TPO-BX  PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5307S-CMOTVO-BAIXA-PREJ
BSI001                                 PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5307S-RMOTVO-BAIXA-PREJ
BSI001                                 PIC  X(030)         VALUE SPACES.
BSI001     05 WRK-5307S-CFUNC-BDSCO-AUTRZ
BSI001                                 PIC  9(009)         VALUE ZEROS.
BSI001     05 WRK-5307S-NFUNC-BDSCO-AUTRZ
BSI001                                 PIC  X(080)         VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO - DCOM5311 ***'.
      *----------------------------------------------------------------*

       01  WRK-5311-ENTRADA.
           05 WRK-5311E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5311E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5311E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5311E-FLAG           PIC  9(001)         VALUE ZEROS.
           05 WRK-5311E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5311E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5311E-OPERACAO       PIC  9(013)         VALUE ZEROS.
           05 FILLER REDEFINES         WRK-5311E-OPERACAO.
              10 WRK-5311E-DANO-OPER   PIC  9(004).
              10 WRK-5311E-NSEQ-OPER   PIC  9(009).
           05 WRK-5311E-CPRODT         PIC  9(003)         VALUE ZEROS.
           05 WRK-5311E-CSPROD-DESC-COML
                                       PIC  9(003)         VALUE ZEROS.
           05 WRK-5311E-AGENCIA-FUNC   PIC  9(005)         VALUE ZEROS.

           05 WRK-5311E-CCNPJ-CPF      PIC  9(009)         VALUE ZEROS.
           05 WRK-5311E-CFLIAL-CNPJ    PIC  9(004)         VALUE ZEROS.
           05 WRK-5311E-CCTRL-CNPJ-CPF PIC  9(002)         VALUE ZEROS.
           05 WRK-5311E-CAG-BCRIA      PIC  9(005)         VALUE ZEROS.
           05 WRK-5311E-CCTA-BCRIA-CLI PIC  9(013)         VALUE ZEROS.
           05 WRK-5311E-CAG-BCRIA-BX   PIC  9(005)         VALUE ZEROS.
           05 WRK-5311E-CCTA-BCRIA-CLI-BX
                                       PIC  9(013)         VALUE ZEROS.
           05 WRK-5311E-CINDCD-LOC-DEB-BX
                                       PIC  X(001)         VALUE SPACES.

       01  WRK-5311-SAIDA.
           05 WRK-5311S-COD-RETORNO    PIC  9(004)         VALUE ZEROS.
           05 WRK-5311S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA DCOM5335 ***'.
      *----------------------------------------------------------------*

       01  5335-ENTRADA.

           05 5335E-COD-RETORNO        PIC  X(004)         VALUE SPACES.
           05 5335E-MSG-RETORNO        PIC  X(079)         VALUE SPACES.
           05 5335E-RESTART            PIC  9(005)         VALUE ZEROS.
           05 5335E-FLAG               PIC  9(001)         VALUE ZEROS.
           05 5335E-CFUNC-BDSCO        PIC  9(009)         VALUE ZEROS.
           05 5335E-CTERM              PIC  X(008)         VALUE SPACES.
           05 5335E-OPERACAO.
              10 5335E-DANO-OPER-DESC  PIC  9(004)         VALUE ZEROS.
              10 5335E-NSEQ-OPER-DESC  PIC  9(009)         VALUE ZEROS.
           05 5335E-NPCELA-DESC-COML   PIC  9(005)         VALUE ZEROS.
           05 5335E-CELMTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-CEVNTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-ACAO               PIC  X(001)         VALUE SPACES.

       01  5335-SAIDA.
           05 5335S-COD-RETORNO        PIC  9(004)         VALUE ZEROS.
           05 5335S-MSG-RETORNO        PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.

      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)         VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660 '.
           05  FILLER                  PIC  X(012)         VALUE
               '- RET.COD = '.
           05  WRK-RETURN-CODE         PIC +9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(011)         VALUE
               ' - LOCAL = '.
           05  WRK-LOCAL-ERRO          PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE ' *'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQOTM - INPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-INPUT-QOTM.
           05 WRK-INP-LL-QOTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-INP-ZZ-QOTM             PIC  9(004) COMP VALUE ZEROS.

           05 WRK-INP-TRANCODE-QOTM       PIC  X(008)      VALUE SPACES.
           05 FILLER                      PIC  X(006)      VALUE SPACES.
           05 WRK-INP-DADOS-QOTM.
             10 WRK-INP-PFK-QOTM          PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SENHAS-QOTM       PIC  X(037)      VALUE SPACES.
             10 WRK-INP-COMANDO-QOTM      PIC  X(068)      VALUE SPACES.
             10 WRK-INP-OCULTOS-QOTM.
               15 WRK-INP-OP-MENU-QOTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-INP-OPCAO-QOTM      PIC  9(001)      VALUE ZEROS.
               15 WRK-INP-TPMANUT-QOTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-INP-PAGINA-QOTM     PIC  9(003)      VALUE ZEROS.
               15 WRK-INP-PARCELA-QOTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-INP-BCO-QOTM        PIC  9(003)      VALUE ZEROS.
               15 WRK-INP-AGENOFF-QOTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-INP-CONTA-QOTM      PIC  9(013)      VALUE ZEROS.
4S2511*        15 WRK-INP-CNPJ-QOTM       PIC  9(009)      VALUE ZEROS.
4S2511         15 WRK-INP-CNPJ-QOTM       PIC  X(009)      VALUE SPACES.
4S2511*        15 WRK-INP-FILIAL-QOTM     PIC  9(004)      VALUE ZEROS.
4S2511         15 WRK-INP-FILIAL-QOTM     PIC  X(04)       VALUE SPACES.
               15 WRK-INP-CTRL-QOTM       PIC  9(002)      VALUE ZEROS.
               15 WRK-INP-TIMES-QOTM      PIC  X(026)      VALUE SPACES.
               15 FILLER                  PIC  X(006)      VALUE SPACES.
             10 WRK-INP-NOMECLI-QOTM      PIC  X(040)      VALUE SPACES.

             10 WRK-INP-CHAMADOR-QOTM     PIC  X(008)      VALUE SPACES.
             10 WRK-INP-CNPJCPF-QOTM      PIC  X(019)      VALUE SPACES.
             10 WRK-INP-DADOSBC-QOTM.
               15 WRK-INP-BANCO-QOTM      PIC  9(003)      VALUE ZEROS.
               15 WRK-INP-BARRA2-QOTM     PIC  X(001)      VALUE SPACES.
               15 WRK-INP-AGEN-QOTM       PIC  9(005)      VALUE ZEROS.
               15 WRK-INP-BARRA3-QOTM     PIC  X(001)      VALUE SPACES.
               15 WRK-INP-CTA-QOTM        PIC  9(013)      VALUE ZEROS.
             10 WRK-INP-CODPROD-QOTM.
               15 WRK-INP-CODPROD-QOTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-DSPROD-QOTM       PIC  X(012)      VALUE SPACES.
             10 WRK-INP-SUBPROD-QOTM.
               15 WRK-INP-SUBPROD-QOTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-DSSBPROD-QOTM     PIC  X(012)      VALUE SPACES.
             10 WRK-INP-OPERACAO-QOTM.
               15 WRK-INP-OPER1-QOTM      PIC  9(004)      VALUE ZEROS.
               15 WRK-INP-OPER2-QOTM      PIC  9(009)      VALUE ZEROS.
             10 WRK-INP-SITUA-QOTM        PIC  X(015)      VALUE SPACES.
             10 WRK-INP-DESSIT-QOTM       PIC  X(013)      VALUE SPACES.
             10 WRK-INP-SITUA2-QOTM.
               15 WRK-INP-SITUA2-QOTM-N   PIC  9(009)      VALUE ZEROS.

             10 WRK-INP-SITUA4-QOTM       PIC  X(013)      VALUE SPACES.
             10 WRK-INP-SITUA5-QOTM       PIC  X(009)      VALUE SPACES.
             10 WRK-INP-QTPAR-QOTM        PIC  X(005)      VALUE SPACES.
             10 WRK-INP-VLRPAR-QOTM       PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-INP-VLRATU-QOTM       PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-INP-VLRBAIX-QOTM.
               15 WRK-INP-VLRBAIX-QOTM-N  PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-INP-TPBAIXA-QOTM.
               15 WRK-INP-TPBAIXA-QOTM-N  PIC  9(001)      VALUE ZEROS.
             10 WRK-INP-DSBAIXA-QOTM      PIC  X(010)      VALUE SPACES.
             10 WRK-INP-DEBEM-QOTM.
               15 WRK-INP-DEBEM-QOTM-N    PIC  9(001)      VALUE ZEROS.
             10 WRK-INP-LANCAM-QOTM       PIC  X(001)      VALUE SPACES.
             10 WRK-INP-AGENCIA-QOTM.
               15 WRK-INP-AGENCIA-QOTM-N  PIC  9(005)      VALUE ZEROS.
             10 WRK-INP-CTADEB-QOTM.
               15 WRK-INP-CTADEB-QOTM-N   PIC  9(013)      VALUE ZEROS.
             10 WRK-INP-BAITIT-QOTM       PIC  X(001)      VALUE SPACES.

BSI001       10 WRK-INP-LMOTVO-QOTM       PIC  X(019)      VALUE SPACES.
BSI001       10 WRK-INP-CMOTVO-QOTM.
BSI001          15 WRK-INP-CMOTVO-QOTM-N
BSI001                                    PIC  9(003)      VALUE ZEROS.
BSI001       10 WRK-INP-RMOTVO-QOTM       PIC  X(015)      VALUE SPACES.
BSI001       10 WRK-INP-LAUTRZ-QOTM       PIC  X(010)      VALUE SPACES.
BSI001       10 WRK-INP-CAUTRZ-QOTM.
BSI001          15 WRK-INP-CAUTRZ-QOTM-N  PIC  9(009)      VALUE ZEROS.
BSI001       10 WRK-INP-NAUTRZ-QOTM       PIC  X(017)      VALUE SPACES.
BSI001       10 WRK-INP-JUSTIF-QOTM       PIC  X(059)      VALUE SPACES.
             10 WRK-INP-MENSA-QOTM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQOTM - OUTPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-OUTPUT-QOTM.
           05 WRK-OUT-LL-QOTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-OUT-ZZ-QOTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-OUT-DADOS-QOTM.

             10 WRK-OUT-SENHAS-QOTM       PIC  X(037)      VALUE SPACES.
             10 WRK-OUT-COMANDO-QOTM      PIC  X(068)      VALUE SPACES.
             10 WRK-OUT-OCULTOS-QOTM.
               15 WRK-OUT-OP-MENU-QOTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-OUT-OPCAO-QOTM      PIC  9(001)      VALUE ZEROS.
               15 WRK-OUT-TPMANUT-QOTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-OUT-PAGINA-QOTM     PIC  9(003)      VALUE ZEROS.
               15 WRK-OUT-PARCELA-QOTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-OUT-BCO-QOTM        PIC  9(003)      VALUE ZEROS.
               15 WRK-OUT-AGENOFF-QOTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-OUT-CONTA-QOTM      PIC  9(013)      VALUE ZEROS.
4S2511*        15 WRK-OUT-CNPJ-QOTM       PIC  9(009)      VALUE ZEROS.
4S2511         15 WRK-OUT-CNPJ-QOTM       PIC  X(09)       VALUE SPACES.
4S2511*        15 WRK-OUT-FILIAL-QOTM     PIC  9(004)      VALUE ZEROS.
4S2511         15 WRK-OUT-FILIAL-QOTM     PIC  X(04)       VALUE SPACES.
               15 WRK-OUT-CTRL-QOTM       PIC  9(002)      VALUE ZEROS.
               15 WRK-OUT-TIMES-QOTM      PIC  X(026)      VALUE SPACES.
               15 FILLER                  PIC  X(006)      VALUE SPACES.
             10 WRK-OUT-NOMECLI-QOTM      PIC  X(040)      VALUE SPACES.
             10 WRK-OUT-CHAMADOR-QOTM     PIC  X(008)      VALUE SPACES.
             10 WRK-OUT-CNPJCPF-QOTM      PIC  X(019)      VALUE SPACES.
             10 WRK-OUT-DADOSBC-QOTM.
               15 WRK-OUT-BANCO-QOTM      PIC  9(003)      VALUE ZEROS.

               15 WRK-OUT-BARRA2-QOTM     PIC  X(001)      VALUE SPACES.
               15 WRK-OUT-AGEN-QOTM       PIC  9(005)      VALUE ZEROS.
               15 WRK-OUT-BARRA3-QOTM     PIC  X(001)      VALUE SPACES.
               15 WRK-OUT-CTA-QOTM        PIC  9(013)      VALUE ZEROS.
             10 WRK-OUT-CODPROD-QOTM.
               15 WRK-OUT-CODPROD-QOTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-OUT-DSPROD-QOTM       PIC  X(012)      VALUE SPACES.
             10 WRK-OUT-SUBPROD-QOTM.
               15 WRK-OUT-SUBPROD-QOTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-OUT-DSSBPROD-QOTM     PIC  X(012)      VALUE SPACES.
             10 WRK-OUT-OPERACAO-QOTM.
               15 WRK-OUT-OPER1-QOTM      PIC  9(004)      VALUE ZEROS.
               15 WRK-OUT-OPER2-QOTM      PIC  9(009)      VALUE ZEROS.
             10 WRK-OUT-SITUA-QOTM        PIC  X(015)      VALUE SPACES.
             10 WRK-OUT-DESSIT-QOTM       PIC  X(013)      VALUE SPACES.
             10 WRK-OUT-SITUA2-QOTM.
               15 WRK-OUT-SITUA2-QOTM-N   PIC  9(009)      VALUE ZEROS.
             10 WRK-OUT-SITUA4-QOTM       PIC  X(013)      VALUE SPACES.
             10 WRK-OUT-SITUA5-QOTM       PIC  X(009)      VALUE SPACES.
             10 WRK-OUT-QTPAR-QOTM.
               15 WRK-OUT-QTPAR-QOTM-N    PIC  9(005)      VALUE ZEROS.

             10 WRK-OUT-VLRPAR-QOTM.
               15 WRK-OUT-VLRPAR-QOTM-N   PIC ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-VLRATU-QOTM.
               15 WRK-OUT-VLRATU-QOTM-N   PIC ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-VLRBAIX-QOTM.
               15 WRK-OUT-VLRBAIX-QOTM-N  PIC ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-TPBAIXA-QOTM.
               15 WRK-OUT-TPBAIXA-QOTM-N  PIC  9(001)      VALUE ZEROS.
             10 WRK-OUT-DSBAIXA-QOTM      PIC  X(010)      VALUE SPACES.
             10 WRK-OUT-DEBEM-QOTM.
               15 WRK-OUT-DEBEM-QOTM-N    PIC  9(001)      VALUE ZEROS.
             10 WRK-OUT-LANCAM-QOTM       PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-AGENCIA-QOTM.
               15 WRK-OUT-AGENCIA-QOTM-N  PIC  9(005)      VALUE ZEROS.
             10 WRK-OUT-CTADEB-QOTM.
               15 WRK-OUT-CTADEB-QOTM-N   PIC  9(013)      VALUE ZEROS.
             10 WRK-OUT-BAITIT-QOTM       PIC  X(001)      VALUE SPACES.
BSI001       10 WRK-OUT-LMOTVO-QOTM       PIC  X(019)      VALUE SPACES.

BSI001       10 WRK-OUT-CMOTVO-QOTM.
BSI001          15 WRK-OUT-CMOTVO-QOTM-N
BSI001                                    PIC  9(003)      VALUE ZEROS.
BSI001       10 WRK-OUT-RMOTVO-QOTM       PIC  X(015)      VALUE SPACES.
BSI001       10 WRK-OUT-LAUTRZ-QOTM       PIC  X(010)      VALUE SPACES.
BSI001       10 WRK-OUT-CAUTRZ-QOTM.
BSI001          15 WRK-OUT-CAUTRZ-QOTM-N  PIC  9(009)      VALUE ZEROS.
BSI001       10 WRK-OUT-NAUTRZ-QOTM       PIC  X(017)      VALUE SPACES.
BSI001       10 WRK-OUT-JUSTIF-QOTM       PIC  X(059)      VALUE SPACES.
             10 WRK-OUT-MENSA-QOTM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                         PIC X(070)       VALUE
           '*** TELA DCOMQOTM - BRAD0660 - SEG. 00 ***'.
      *---------------------------------------------------------------*

       01  WRK-660-DCOMQOTM.
           05 WRK-660-LL-AREA-QOTM        PIC  9(004) COMP VALUE  0000.
           05 WRK-660-LL-MENSAGEM-QOTM    PIC  9(004) COMP VALUE  0000.
           05 WRK-660-SENHAS-QOTM         PIC  9(004) COMP VALUE  0037.
           05 WRK-660-COMANDO-QOTM        PIC  9(004) COMP VALUE  0068.

           05 WRK-660-OCULTOS-QOTM        PIC  9(004) COMP VALUE  0079.
           05 WRK-660-NOMECLI-QOTM        PIC  9(004) COMP VALUE  0040.
           05 WRK-660-CHAMADOR-QOTM       PIC  9(004) COMP VALUE  0008.
           05 WRK-660-CNPJCPF-QOTM        PIC  9(004) COMP VALUE  0019.
           05 WRK-660-DADOSBC-QOTM        PIC  9(004) COMP VALUE  0023.
           05 WRK-660-CODPROD-QOTM        PIC  9(004) COMP VALUE  0003.
           05 WRK-660-DSPROD-QOTM         PIC  9(004) COMP VALUE  0012.
           05 WRK-660-SUBPROD-QOTM        PIC  9(004) COMP VALUE  0003.
           05 WRK-660-DSSBPROD-QOTM       PIC  9(004) COMP VALUE  0012.
           05 WRK-660-OPERACAO-QOTM       PIC  9(004) COMP VALUE  0013.
           05 WRK-660-SITUA-QOTM          PIC  9(004) COMP VALUE  0015.
           05 WRK-660-DESSIT-QOTM         PIC  9(004) COMP VALUE  0013.
           05 WRK-660-SITUA2-QOTM         PIC  9(004) COMP VALUE  0009.
           05 WRK-660-SITUA4-QOTM         PIC  9(004) COMP VALUE  0013.
           05 WRK-660-SITUA5-QOTM         PIC  9(004) COMP VALUE  0009.
           05 WRK-660-QTPAR-QOTM          PIC  9(004) COMP VALUE  0005.
           05 WRK-660-VLRPAR-QOTM         PIC  9(004) COMP VALUE  0017.
           05 WRK-660-VLRATU-QOTM         PIC  9(004) COMP VALUE  0017.
           05 WRK-660-VLRBAIX-QOTM        PIC  9(004) COMP VALUE  0017.
           05 WRK-660-TPBAIXA-QOTM        PIC  9(004) COMP VALUE  0001.
           05 WRK-660-DSBAIXA-QOTM        PIC  9(004) COMP VALUE  0010.

           05 WRK-660-DEBEM-QOTM          PIC  9(004) COMP VALUE  0001.
           05 WRK-660-LANCAM-QOTM         PIC  9(004) COMP VALUE  0001.
           05 WRK-660-AGENCIA-QOTM        PIC  9(004) COMP VALUE  0005.
           05 WRK-660-CTADEB-QOTM         PIC  9(004) COMP VALUE  0013.
           05 WRK-660-BAITIT-QOTM         PIC  9(004) COMP VALUE  0001.
BSI001     05 WRK-660-LMOTVO-QOTM         PIC  9(004) COMP VALUE  0019.
BSI001     05 WRK-660-CMOTVO-QOTM         PIC  9(004) COMP VALUE  0003.
BSI001     05 WRK-660-RMOTVO-QOTM         PIC  9(004) COMP VALUE  0015.
BSI001     05 WRK-660-LAUTRZ-QOTM         PIC  9(004) COMP VALUE  0010.
BSI001     05 WRK-660-CAUTRZ-QOTM         PIC  9(004) COMP VALUE  0009.
BSI001     05 WRK-660-NAUTRZ-QOTM         PIC  9(004) COMP VALUE  0017.
BSI001     05 WRK-660-JUSTIF-QOTM         PIC  9(004) COMP VALUE  0059.
           05 WRK-660-MENSA-QOTM          PIC  9(004) COMP VALUE  0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).

           05  LNK-ALT-MODNAME         PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      ******************************************************************
      * EFETUA O PROCESSAMENTO DA TRANSACAO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-PROCESSAR-TRANSACAO        SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PCB

                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.


           IF  RETURN-CODE             EQUAL 04 OR 12
               GOBACK
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-QOTM.

           PERFORM 1000-ROTINA-PRINCIPAL.

           GO TO 0000-PROCESSAR-TRANSACAO.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      * ROTINA PRINCIPAL DO PROGRAMA.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMQOTM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1715'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMQOTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMQOTM'
               PERFORM 1300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      * RECEBE CONTROLE DO PROGRAMA CHAMADOR - DCOM7900                *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.
           MOVE WRK-COMU-OP-MENU       TO WRK-INP-OP-MENU-QOTM
                                          WRK-OUT-OP-MENU-QOTM.
           MOVE WRK-COMU-OPCAO-N       TO WRK-INP-OPCAO-QOTM
                                          WRK-OUT-OPCAO-QOTM.
           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPMANUT-QOTM
                                          WRK-OUT-TPMANUT-QOTM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TIMES-QOTM
                                          WRK-OUT-TIMES-QOTM.
           MOVE WRK-COMU-PARCELA       TO WRK-INP-PARCELA-QOTM
                                          WRK-OUT-PARCELA-QOTM.
           MOVE WRK-COMU-BANCO         TO WRK-INP-BCO-QOTM

                                          WRK-OUT-BCO-QOTM.
           MOVE WRK-COMU-AGENCIA       TO WRK-INP-AGENOFF-QOTM
                                          WRK-OUT-AGENOFF-QOTM.
           MOVE WRK-COMU-CONTA         TO WRK-INP-CONTA-QOTM
                                          WRK-OUT-CONTA-QOTM.
           MOVE WRK-COMU-CCNPJ-CPF     TO WRK-INP-CNPJ-QOTM
                                          WRK-OUT-CNPJ-QOTM.
           MOVE WRK-COMU-CFLIAL-CNPJ   TO WRK-INP-FILIAL-QOTM
                                          WRK-OUT-FILIAL-QOTM.
           MOVE WRK-COMU-CCTRL-CNPJCPF TO WRK-INP-CTRL-QOTM
                                          WRK-OUT-CTRL-QOTM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGINA-QOTM
                                          WRK-OUT-PAGINA-QOTM.
           MOVE WRK-COMU-DANO-OPER     TO WRK-INP-OPER1-QOTM
                                          WRK-OUT-OPER1-QOTM.
           MOVE WRK-COMU-NSEQ-OPER     TO WRK-INP-OPER2-QOTM
                                          WRK-OUT-OPER2-QOTM.
           MOVE SPACES                 TO WRK-MENSAGEM-AUX.
           MOVE WRK-COMU-MENSAGEM      TO WRK-MENSAGEM-AUX.



           IF  WRK-COMU-TRANSACAO      NOT EQUAL 'DCOM1716'
               MOVE WRK-COMU-TRANSACAO TO WRK-INP-CHAMADOR-QOTM
                                          WRK-OUT-CHAMADOR-QOTM
           ELSE
               MOVE WRK-COMU-CHAMADOR  TO WRK-INP-CHAMADOR-QOTM
                                          WRK-OUT-CHAMADOR-QOTM
           END-IF.

           PERFORM 1110-ACESSAR-DCOM4174.

           IF  WRK-INCONSISTENCIA      NOT EQUAL '*'
               PERFORM 1120-ACESSAR-DCOM5307
               IF  WRK-INCONSISTENCIA  NOT EQUAL '*'
                   MOVE WRK-MENSA-03   TO WRK-OUT-MENSA-QOTM
               END-IF
           END-IF.

           IF  WRK-MENSAGEM-AUX        NOT EQUAL SPACES
               MOVE WRK-MENSAGEM-AUX   TO WRK-OUT-MENSA-QOTM
           END-IF.


      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-ACESSAR-DCOM4174           SECTION.
      *----------------------------------------------------------------*


           MOVE SPACES                 TO WRK-SQLCA
                                          ERRO-AREA.

           INITIALIZE 4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR
                      WRK-SQLCA
                      ERRO-AREA.

           MOVE WRK-COD-USER-N         TO 4174-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM.
           MOVE WRK-COMU-DANO-OPER     TO 4174-ENT-DANO-OPER-DESC.
           MOVE WRK-COMU-NSEQ-OPER     TO 4174-ENT-NSEQ-OPER-DESC.

           MOVE 'DCOM4174'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE 4174-SAI-COD-RETORNO
               WHEN '0000'
                    PERFORM 1111-MONTAR-DCOM4174

               WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    PERFORM 1210-RETORNAR-CHAMADOR
                    MOVE 4174-SAI-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                    MOVE WRK-COMU-AREA TO WRK-MENSAGEM

                    MOVE '*'           TO WRK-INCONSISTENCIA
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA MONTA A AREA RETORNADA PELO PROGRAMA DCOM4174     *
      *----------------------------------------------------------------*
       1111-MONTAR-DCOM4174            SECTION.
      *----------------------------------------------------------------*


           MOVE 4174-SAI-IPSSOA-DESC-COML  TO WRK-OUT-NOMECLI-QOTM.

           STRING 4174-SAI-CCNPJ-CPF(1:3) '.'
                  4174-SAI-CCNPJ-CPF(4:3) '.'
                  4174-SAI-CCNPJ-CPF(7:3) '/'
                  4174-SAI-CFLIAL-CNPJ    '-'
                  4174-SAI-CCTRL-CNPJ-CPF

           DELIMITED BY SIZE               INTO WRK-OUT-CNPJCPF-QOTM.

           STRING 4174-SAI-CBCO      '/'
                  4174-SAI-CAG-BCRIA '/'
                  4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE               INTO WRK-OUT-DADOSBC-QOTM.

           MOVE 4174-SAI-CPRODT            TO WRK-OUT-CODPROD-QOTM.
           MOVE 4174-SAI-IABREV-PRODT      TO WRK-OUT-DSPROD-QOTM.
           MOVE 4174-SAI-CSPROD-DESC-COML  TO WRK-OUT-SUBPROD-QOTM.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC TO WRK-OUT-DSSBPROD-QOTM.
           MOVE 4174-SAI-DANO-OPER-DESC    TO WRK-OUT-OPER1-QOTM.
           MOVE 4174-SAI-NSEQ-OPER-DESC    TO WRK-OUT-OPER2-QOTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC   TO WRK-OUT-SITUA-QOTM.

           EVALUATE TRUE
               WHEN 4174-SAI-CCONTR-LIM-DESC      GREATER ZEROS
                    MOVE 'CONTR LIMITE:'          TO WRK-OUT-DESSIT-QOTM
                    MOVE 4174-SAI-CCONTR-LIM-DESC TO WRK-OUT-SITUA2-QOTM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND

                    4174-SAI-CELMTO-DESC-COML     EQUAL 8
                    MOVE 'CONV GERAL..:'          TO WRK-OUT-DESSIT-QOTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITUA2-QOTM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 9
                    MOVE 'CONV CLIENTE:'          TO WRK-OUT-DESSIT-QOTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITUA2-QOTM
           END-EVALUATE.

           IF  4174-SAI-NSMULA-OPER-DESC   GREATER ZEROS
               IF  WRK-OUT-SITUA2-QOTM     EQUAL SPACES
                   MOVE 'SIMULACAO...:'    TO WRK-OUT-DESSIT-QOTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-SITUA2-QOTM
               ELSE
                   MOVE 'SIMULACAO...:'    TO WRK-OUT-SITUA4-QOTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-SITUA5-QOTM

               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM5307           SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-5307-ENTRADA
                      WRK-5307-SAIDA
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE WRK-COD-USER-N         TO WRK-5307E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5307E-CTERM.
           MOVE WRK-INP-OPER1-QOTM     TO WRK-5307E-DANO-OPER.
           MOVE WRK-INP-OPER2-QOTM     TO WRK-5307E-NSEQ-OPER.

           MOVE WRK-COD-DEPTO-N        TO WRK-5307E-AGENCIA-FUNC.
           MOVE 'I'                    TO WRK-5307E-FLAG-FUNCAO.
           MOVE 'DCOM5307'             TO WRK-MODULO.


           CALL WRK-MODULO             USING WRK-5307-ENTRADA
                                             WRK-5307-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-5307S-COD-RETORNO
               WHEN '0000'
                    PERFORM 1121-FORMATAR-TELA

               WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    PERFORM 1210-RETORNAR-CHAMADOR
                    MOVE WRK-5307S-MSG-RETORNO

                                       TO WRK-COMU-MENSAGEM
                    MOVE WRK-COMU-AREA TO WRK-MENSAGEM
                    MOVE '*'           TO WRK-INCONSISTENCIA
           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  FORMATACAO DA TELA                                            *
      *----------------------------------------------------------------*
       1121-FORMATAR-TELA              SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-5307S-QTDE-PARCELAS  TO WRK-OUT-QTPAR-QOTM-N.

           MOVE WRK-5307S-VPRINC-PCELA   TO WRK-OUT-VLRPAR-QOTM-N.
           MOVE WRK-5307S-VATUAL-PCELA   TO WRK-OUT-VLRATU-QOTM-N.
           MOVE WRK-5307S-VEFETV-BAIXA   TO WRK-OUT-VLRBAIX-QOTM-N.

           MOVE WRK-5307S-CTPO-BX-PCELA  TO WRK-OUT-TPBAIXA-QOTM.
           MOVE WRK-5307S-IRSUMO-TPO-BX  TO WRK-OUT-DSBAIXA-QOTM.
           MOVE WRK-5307S-CINDCD-LOC-DEB TO WRK-OUT-DEBEM-QOTM-N
           MOVE WRK-5307S-CINDCD-CON-EXT TO WRK-OUT-LANCAM-QOTM.
           MOVE WRK-5307S-CINDCD-BX-COBR TO WRK-OUT-BAITIT-QOTM.

           IF  WRK-5307S-CINDCD-LOC-DEB  EQUAL 2
               MOVE SPACES               TO WRK-OUT-AGENCIA-QOTM
                                            WRK-OUT-CTADEB-QOTM
           ELSE
               MOVE WRK-5307S-CAG-BCRIA  TO WRK-OUT-AGENCIA-QOTM-N
               MOVE WRK-5307S-CCTA-BCRIA TO WRK-OUT-CTADEB-QOTM-N
           END-IF.

BSI001     IF  WRK-5307S-CMOTVO-BAIXA-PREJ GREATER ZEROS
BSI001         MOVE 'MOTIVO BAIXA......:'  TO WRK-OUT-LMOTVO-QOTM
BSI001         MOVE WRK-5307S-CMOTVO-BAIXA-PREJ
BSI001                                     TO WRK-OUT-CMOTVO-QOTM-N
BSI001         STRING '- ' WRK-5307S-RMOTVO-BAIXA-PREJ(1:13)
BSI001         DELIMITED BY SIZE         INTO WRK-OUT-RMOTVO-QOTM
BSI001     ELSE

BSI001         MOVE SPACES                 TO WRK-OUT-LMOTVO-QOTM
BSI001                                        WRK-OUT-CMOTVO-QOTM
BSI001                                        WRK-OUT-RMOTVO-QOTM
BSI001     END-IF.
BSI001
BSI001     IF  WRK-5307S-CFUNC-BDSCO-AUTRZ GREATER ZEROS
BSI001         MOVE 'COD. AUT.:'           TO WRK-OUT-LAUTRZ-QOTM
BSI001         MOVE WRK-5307S-CFUNC-BDSCO-AUTRZ
BSI001                                     TO WRK-OUT-CAUTRZ-QOTM-N
BSI001         STRING '- ' WRK-5307S-NFUNC-BDSCO-AUTRZ(1:15)
BSI001         DELIMITED BY SIZE         INTO WRK-OUT-NAUTRZ-QOTM
BSI001     ELSE
BSI001         MOVE SPACES                 TO WRK-OUT-LAUTRZ-QOTM
BSI001                                        WRK-OUT-CAUTRZ-QOTM
BSI001                                        WRK-OUT-NAUTRZ-QOTM
BSI001     END-IF.
BSI001
BSI001     MOVE WRK-5307S-RJUSTF-BX-PARC(1:59)
BSI001                                     TO WRK-OUT-JUSTIF-QOTM.
BSI001
      *----------------------------------------------------------------*

       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMQOTM         SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-MENSAGEM           TO WRK-INPUT-QOTM.

           PERFORM 1260-DEVOLVER-TELA

           EVALUATE TRUE
             WHEN  (WRK-INP-PFK-QOTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QOTM   EQUAL 'PFK01')  OR
                    WRK-INP-PFK-QOTM       EQUAL 'H'
                    MOVE WRK-INP-MENSA-QOTM TO WRK-OUT-MENSA-QOTM


             WHEN  (WRK-INP-PFK-QOTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QOTM   EQUAL 'PFK03')  OR
                    WRK-INP-PFK-QOTM       EQUAL '3'
                    PERFORM 1210-RETORNAR-CHAMADOR

             WHEN  (WRK-INP-PFK-QOTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QOTM   EQUAL 'PFK05')  OR
                    WRK-INP-PFK-QOTM       EQUAL '5'
                    PERFORM 1220-TRATAR-PF5

             WHEN  (WRK-INP-PFK-QOTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QOTM   EQUAL 'PFK10')  OR
                    WRK-INP-PFK-QOTM       EQUAL 'A'
                    PERFORM 1230-TRATAR-PF10

             WHEN  (WRK-INP-PFK-QOTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QOTM   EQUAL 'PFK06')   OR
                    WRK-INP-PFK-QOTM       EQUAL '6'
                    PERFORM 1235-ACESSAR-DCOM5335
                    IF  WRK-INCONSISTENCIA EQUAL SPACES
                        PERFORM 1240-TRATAR-PF6

                    END-IF

             WHEN  (WRK-INP-PFK-QOTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QOTM   EQUAL 'PFK11')  OR
                    WRK-INP-PFK-QOTM       EQUAL 'B'
                    PERFORM 1250-TRATAR-PF11

             WHEN  (WRK-INP-PFK-QOTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QOTM   EQUAL SPACES)
                    MOVE WRK-MENSA-03      TO WRK-OUT-MENSA-QOTM

             WHEN   OTHER
                    MOVE WRK-MENSA-01      TO WRK-OUT-MENSA-QOTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA O PROGRAMA CHAMADOR                              *

      ******************************************************************
      *----------------------------------------------------------------*
       1210-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*


           PERFORM 1211-FORMATAR-COMU-AREA.

           MOVE WRK-CHNG               TO WRK-FUNCAO.

           EVALUATE WRK-COMU-TP-SELECAO
               WHEN 1
                    MOVE 'DCOM1702'    TO WRK-TELA
               WHEN 3
                    MOVE 'DCOM1703'    TO WRK-TELA
               WHEN 4 THRU 5
                    MOVE 'DCOM1704'    TO WRK-TELA
           END-EVALUATE.

           MOVE 'DCOM1715'             TO WRK-COMU-TRANSACAO.
           MOVE LENGTH                 OF WRK-COMU-AREA

                                       TO WRK-COMU-LL.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.


      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FORMATAR A AREA DE COMUNICACAO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1211-FORMATAR-COMU-AREA         SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-INP-OP-MENU-QOTM   TO WRK-COMU-OP-MENU.
           MOVE WRK-INP-OPCAO-QOTM     TO WRK-COMU-OPCAO-N.
           MOVE WRK-INP-TPMANUT-QOTM   TO WRK-COMU-TP-SELECAO.
           MOVE WRK-INP-TIMES-QOTM     TO WRK-COMU-TIMESTAMP.
           MOVE WRK-INP-PARCELA-QOTM   TO WRK-COMU-PARCELA.

           MOVE WRK-INP-BCO-QOTM       TO WRK-COMU-BANCO.
           MOVE WRK-INP-AGENOFF-QOTM   TO WRK-COMU-AGENCIA.
           MOVE WRK-INP-CONTA-QOTM     TO WRK-COMU-CONTA.
           MOVE WRK-INP-CNPJ-QOTM      TO WRK-COMU-CCNPJ-CPF.
           MOVE WRK-INP-FILIAL-QOTM    TO WRK-COMU-CFLIAL-CNPJ.
           MOVE WRK-INP-CTRL-QOTM      TO WRK-COMU-CCTRL-CNPJCPF.
           MOVE WRK-INP-PAGINA-QOTM    TO WRK-COMU-PAGINA-1.
           MOVE WRK-INP-OPER1-QOTM     TO WRK-COMU-DANO-OPER.
           MOVE WRK-INP-OPER2-QOTM     TO WRK-COMU-NSEQ-OPER.

      *----------------------------------------------------------------*
       1211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA O SENHAS - MENU DE ROTINAS                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-TRATAR-PF5                 SECTION.
      *----------------------------------------------------------------*



           MOVE WRK-CHNG                    TO WRK-FUNCAO.
           MOVE 'DCOM1715'                  TO WRK-TELA
                                               WRK-COMU-TRANSACAO.
           MOVE '5'                         TO WRK-COMU-PFK.
           MOVE LENGTH OF WRK-COMU-AREA     TO WRK-COMU-LL.
           MOVE WRK-COMU-AREA               TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA O SENHAS - MENU DE DCOM                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1230-TRATAR-PF10                SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-CHNG                 TO WRK-FUNCAO.

           MOVE 'DCOM1715'               TO WRK-TELA
                                            WRK-COMU-TRANSACAO.
           MOVE 'A'                      TO WRK-COMU-PFK.
           MOVE LENGTH OF WRK-COMU-AREA  TO WRK-COMU-LL.
           MOVE WRK-COMU-AREA            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1235-ACESSAR-DCOM5335           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5335-ENTRADA
                      5335-SAIDA.

           MOVE WRK-INP-OPERACAO-QOTM  TO 5335E-OPERACAO.
           MOVE 007                    TO 5335E-CELMTO-DESC-COML.
           MOVE 089                    TO 5335E-CEVNTO-DESC-COML.
           MOVE 'I'                    TO 5335E-ACAO.

           MOVE 'DCOM5335'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 5335-ENTRADA
                                             5335-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  5335S-COD-RETORNO
               WHEN '0000'
                     CONTINUE

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE 5335S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-QOTM
                     MOVE '*'          TO WRK-INCONSISTENCIA
           END-EVALUATE.


      *----------------------------------------------------------------*
       1235-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * INCLUIR ESTORNA DA BAIXA DA PARCELA POR RATEIO                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1240-TRATAR-PF6                 SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-5311-ENTRADA
                      WRK-5311-SAIDA
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE WRK-COD-USER-N         TO WRK-5311E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5311E-CTERM.
           MOVE WRK-INP-OPER1-QOTM     TO WRK-5311E-DANO-OPER.
           MOVE WRK-INP-OPER2-QOTM     TO WRK-5311E-NSEQ-OPER.

           MOVE WRK-INP-CODPROD-QOTM-N TO WRK-5311E-CPRODT.
           MOVE WRK-INP-SUBPROD-QOTM-N TO WRK-5311E-CSPROD-DESC-COML.
           MOVE WRK-COD-DEPTO-N        TO WRK-5311E-AGENCIA-FUNC.

           STRING WRK-INP-CNPJCPF-QOTM(01:03)
                  WRK-INP-CNPJCPF-QOTM(05:03)
                  WRK-INP-CNPJCPF-QOTM(09:03)
           DELIMITED BY SIZE           INTO WRK-5311E-CCNPJ-CPF

           MOVE WRK-INP-CNPJCPF-QOTM  (13:04)
                                       TO WRK-5311E-CFLIAL-CNPJ.
           MOVE WRK-INP-CNPJCPF-QOTM  (18:02)
                                       TO WRK-5311E-CCTRL-CNPJ-CPF.
           MOVE WRK-INP-AGEN-QOTM      TO WRK-5311E-CAG-BCRIA.
           MOVE WRK-INP-CTA-QOTM       TO WRK-5311E-CCTA-BCRIA-CLI.
           MOVE WRK-INP-DEBEM-QOTM-N   TO WRK-5311E-CINDCD-LOC-DEB-BX.

           IF  WRK-INP-DEBEM-QOTM-N    EQUAL 1
               MOVE WRK-INP-AGENCIA-QOTM-N
                                       TO WRK-5311E-CAG-BCRIA-BX
               MOVE WRK-INP-CTADEB-QOTM-N

                                       TO WRK-5311E-CCTA-BCRIA-CLI-BX
           END-IF.

           MOVE 'DCOM5311'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5311-ENTRADA
                                             WRK-5311-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-5311S-COD-RETORNO
               WHEN '0000'
                    PERFORM 1210-RETORNAR-CHAMADOR
                    MOVE WRK-5311S-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                    MOVE WRK-COMU-AREA TO WRK-MENSAGEM

               WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO


               WHEN '3500'
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    PERFORM 9998-EXECUTAR-ROLLBACK
                    MOVE WRK-5311S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-QOTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PASSAR CONTROLE PARA O PROGRAMA DE RESUMO DAS PARCELAS         *
      ******************************************************************
      *----------------------------------------------------------------*
       1250-TRATAR-PF11                SECTION.
      *----------------------------------------------------------------*



           PERFORM 1211-FORMATAR-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1716'             TO WRK-TELA.
           MOVE 'DCOM1715'             TO WRK-COMU-TRANSACAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * DEVOLVER DADOS DA INPUT PARA OUTPUT (TELA)                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1260-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*



           MOVE WRK-INP-OCULTOS-QOTM   TO WRK-OUT-OCULTOS-QOTM.
           MOVE WRK-INP-NOMECLI-QOTM   TO WRK-OUT-NOMECLI-QOTM.
           MOVE WRK-INP-CHAMADOR-QOTM  TO WRK-OUT-CHAMADOR-QOTM.
           MOVE WRK-INP-CNPJCPF-QOTM   TO WRK-OUT-CNPJCPF-QOTM.
           MOVE WRK-INP-DADOSBC-QOTM   TO WRK-OUT-DADOSBC-QOTM.
           MOVE WRK-INP-CODPROD-QOTM   TO WRK-OUT-CODPROD-QOTM.
           MOVE WRK-INP-DSPROD-QOTM    TO WRK-OUT-DSPROD-QOTM.
           MOVE WRK-INP-SUBPROD-QOTM   TO WRK-OUT-SUBPROD-QOTM.
           MOVE WRK-INP-DSSBPROD-QOTM  TO WRK-OUT-DSSBPROD-QOTM.
           MOVE WRK-INP-OPERACAO-QOTM  TO WRK-OUT-OPERACAO-QOTM.
           MOVE WRK-INP-SITUA-QOTM     TO WRK-OUT-SITUA-QOTM.
           MOVE WRK-INP-DESSIT-QOTM    TO WRK-OUT-DESSIT-QOTM.
           MOVE WRK-INP-SITUA2-QOTM    TO WRK-OUT-SITUA2-QOTM.
           MOVE WRK-INP-SITUA4-QOTM    TO WRK-OUT-SITUA4-QOTM.
           MOVE WRK-INP-SITUA5-QOTM    TO WRK-OUT-SITUA5-QOTM.
           MOVE WRK-INP-QTPAR-QOTM     TO WRK-OUT-QTPAR-QOTM.
           MOVE WRK-INP-VLRPAR-QOTM    TO WRK-OUT-VLRPAR-QOTM.
           MOVE WRK-INP-VLRATU-QOTM    TO WRK-OUT-VLRATU-QOTM.
           MOVE WRK-INP-VLRBAIX-QOTM   TO WRK-OUT-VLRBAIX-QOTM.
           MOVE WRK-INP-TPBAIXA-QOTM   TO WRK-OUT-TPBAIXA-QOTM.
           MOVE WRK-INP-DSBAIXA-QOTM   TO WRK-OUT-DSBAIXA-QOTM.

           MOVE WRK-INP-DEBEM-QOTM     TO WRK-OUT-DEBEM-QOTM.
           MOVE WRK-INP-LANCAM-QOTM    TO WRK-OUT-LANCAM-QOTM.
           MOVE WRK-INP-AGENCIA-QOTM   TO WRK-OUT-AGENCIA-QOTM.
           MOVE WRK-INP-CTADEB-QOTM    TO WRK-OUT-CTADEB-QOTM.
           MOVE WRK-INP-BAITIT-QOTM    TO WRK-OUT-BAITIT-QOTM.
BSI001     MOVE WRK-INP-LMOTVO-QOTM    TO WRK-OUT-LMOTVO-QOTM.
BSI001     MOVE WRK-INP-CMOTVO-QOTM    TO WRK-OUT-CMOTVO-QOTM.
BSI001     MOVE WRK-INP-RMOTVO-QOTM    TO WRK-OUT-RMOTVO-QOTM.
BSI001     MOVE WRK-INP-LAUTRZ-QOTM    TO WRK-OUT-LAUTRZ-QOTM.
BSI001     MOVE WRK-INP-CAUTRZ-QOTM    TO WRK-OUT-CAUTRZ-QOTM.
BSI001     MOVE WRK-INP-NAUTRZ-QOTM    TO WRK-OUT-NAUTRZ-QOTM.
BSI001     MOVE WRK-INP-JUSTIF-QOTM    TO WRK-OUT-JUSTIF-QOTM.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSA BRAD660 COMPACTA DADOS PARA DCOMQOTM                  *
      ******************************************************************
      *----------------------------------------------------------------*

       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*


           MOVE LENGTH                 OF WRK-OUTPUT-QOTM
                                       TO WRK-OUT-LL-QOTM
                                          WRK-660-LL-MENSAGEM-QOTM.
           MOVE LENGTH                 OF WRK-660-DCOMQOTM
                                       TO WRK-660-LL-AREA-QOTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-QOTM
                                             WRK-660-DCOMQOTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-COD-RET
               MOVE WRK-COD-RET-N      TO WRK-RETURN-CODE
               MOVE 010                TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.


           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-QOTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BSI001*----------------------------------------------------------------*
BSI001 9998-EXECUTAR-ROLLBACK          SECTION.
BSI001*----------------------------------------------------------------*
BSI001
BSI001      CALL 'CBLTDLI'             USING WRK-ROLB
BSI001                                       LNK-IO-PCB.
BSI001
BSI001*----------------------------------------------------------------*
BSI001 9998-99-FIM.                    EXIT.
BSI001*----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA A BRAD7100 - TRATAMENTO DE ERRO                        *

      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES OR LOW-VALUES
               MOVE 'SENH7020'         TO ERR-PGM
               MOVE 'DCOM1715'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM1715'         TO ERR-PGM
           END-IF.

           MOVE  WRK-COD-USER          TO ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO ERR-COD-DEPTO.

           CALL 'BRAD7100'             USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA.

           GOBACK.


      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
