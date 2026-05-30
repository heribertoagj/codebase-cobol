      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1718.
       AUTHOR.     FABRICA-ALTRAN.
      *================================================================*
      *                   A L T R A N                                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  DCOM1718                                     *
      *    ANALISTA....:  FERNANDO LUIZ DE SANTI                       *
      *    DATA........:  11/01/2008                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  CONSULTA ESTORNO DA BAIXA DA PARCELA POR     *
      *      RATEIO - DETALHES.                                        *
      *----------------------------------------------------------------*
      *    TELA........:  DCOMQRTM - CONSULTA ESTORNO DA BAIXA DA      *
      *      PARCELA POR RATEIO - DETALHES.                            *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *

      *    I#BRAD7C - AREA DE COMUNICACAO COM A BRAD7100.              *
      *    I#DCOMXJ - AREA DE COMUNICACAO DCOM4174.                    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERE CARACTERES DE NULOS NA MENSAGEM DE SAIDA. *
      *    BRAD7100 - TRATAMENTO DE ERROS.                             *
      *    DCOM5307 - CONSULTAR MOVIMENTO DE ESTORNO BAIXA RATEIO.     *
      *    POOL5005 - ENVIA / RECEBE MENSAGENS DO IMS.                 *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.:   DCOM1702                  CHAMA: DCOM1716   *
      *                    DCOM1703                                    *
      *                    DCOM1704                                    *
      *================================================================*
BSI001*                   BSI TECNOLOGIA - ALTERACAO                   *
BSI001*----------------------------------------------------------------*
BSI001*    PROGRAMADOR.:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI001*    ANALISTA BSI:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI001*    ANALISTA DDS:  RICARDO JAMMAL     - GRUPO 39                *
BSI001*    DATA........:  10/01/2017                                   *
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
       77  WRK-MENSA-02                PIC  X(079)         VALUE
           'TECLE <PF3> PARA RETORNAR OU <PF11> PARA VISUALIZAR O RESUMO
      -    ''.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.

      *----------------------------------------------------------------*

       77  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.

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
           '*** TELA DCOMQRTM - INPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-INPUT-QRTM.
           05 WRK-INP-LL-QRTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-INP-ZZ-QRTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-INP-TRANCODE-QRTM       PIC  X(008)      VALUE SPACES.
           05 FILLER                      PIC  X(006)      VALUE SPACES.
           05 WRK-INP-DADOS-QRTM.
             10 WRK-INP-PFK-QRTM          PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SENHAS-QRTM       PIC  X(037)      VALUE SPACES.
             10 WRK-INP-COMANDO-QRTM      PIC  X(068)      VALUE SPACES.
             10 WRK-INP-OCULTOS-QRTM.

               15 WRK-INP-OP-MENU-QRTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-INP-OPCAO-QRTM      PIC  9(001)      VALUE ZEROS.
               15 WRK-INP-TPMANUT-QRTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-INP-PAGINA-QRTM     PIC  9(003)      VALUE ZEROS.
               15 WRK-INP-PARCELA-QRTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-INP-BCO-QRTM        PIC  9(003)      VALUE ZEROS.
               15 WRK-INP-AGENOFF-QRTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-INP-CONTA-QRTM      PIC  9(013)      VALUE ZEROS.
4S2511*        15 WRK-INP-CNPJ-QRTM       PIC  9(009)      VALUE ZEROS.
4S2511         15 WRK-INP-CNPJ-QRTM       PIC  X(009)      VALUE SPACES.
4S2511*        15 WRK-INP-FILIAL-QRTM     PIC  9(004)      VALUE ZEROS.
4S2511         15 WRK-INP-FILIAL-QRTM     PIC  X(004)      VALUE SPACES.
               15 WRK-INP-CTRL-QRTM       PIC  9(002)      VALUE ZEROS.
               15 WRK-INP-TIMES-QRTM      PIC  X(026)      VALUE SPACES.
               15 FILLER                  PIC  X(006)      VALUE SPACES.
             10 WRK-INP-NOMECLI-QRTM      PIC  X(040)      VALUE SPACES.
             10 WRK-INP-CHAMADOR-QRTM     PIC  X(008)      VALUE SPACES.
             10 WRK-INP-CNPJCPF-QRTM      PIC  X(019)      VALUE SPACES.
             10 WRK-INP-DADOSBC-QRTM.
               15 WRK-INP-BANCO-QRTM      PIC  9(003)      VALUE ZEROS.
               15 WRK-INP-BARRA2-QRTM     PIC  X(001)      VALUE SPACES.
               15 WRK-INP-AGEN-QRTM       PIC  9(005)      VALUE ZEROS.
               15 WRK-INP-BARRA3-QRTM     PIC  X(001)      VALUE SPACES.

               15 WRK-INP-CTA-QRTM        PIC  9(013)      VALUE ZEROS.
             10 WRK-INP-CODPROD-QRTM.
               15 WRK-INP-CODPROD-QRTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-DSPROD-QRTM       PIC  X(012)      VALUE SPACES.
             10 WRK-INP-SUBPROD-QRTM.
               15 WRK-INP-SUBPROD-QRTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-DSSBPROD-QRTM     PIC  X(012)      VALUE SPACES.
             10 WRK-INP-OPERACAO-QRTM.
               15 WRK-INP-OPER1-QRTM      PIC  9(004)      VALUE ZEROS.
               15 WRK-INP-OPER2-QRTM      PIC  9(009)      VALUE ZEROS.
             10 WRK-INP-SITUA1-QRTM       PIC  X(015)      VALUE SPACES.
             10 WRK-INP-SITUA2-QRTM       PIC  X(013)      VALUE SPACES.
             10 WRK-INP-SITUA3-QRTM.
               15 WRK-INP-SITUA3-QRTM-N   PIC  9(009)      VALUE ZEROS.
             10 WRK-INP-SITUA4-QRTM       PIC  X(013)      VALUE SPACES.
             10 WRK-INP-SITUA5-QRTM       PIC  X(009)      VALUE SPACES.
             10 WRK-INP-QTPAR-QRTM        PIC  9(005)      VALUE ZEROS.
             10 WRK-INP-VLRPAR-QRTM       PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-INP-VLRATU-QRTM       PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.

             10 WRK-INP-VLRBAIX-QRTM.
               15 WRK-INP-VLRBAIX-QRTM-N  PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-INP-TPBAIXA-QRTM.
               15 WRK-INP-TPBAIXA-QRTM-N  PIC  9(001)      VALUE ZEROS.
             10 WRK-INP-DSBAIXA-QRTM      PIC  X(010)      VALUE SPACES.
             10 WRK-INP-DEBEM-QRTM.
               15 WRK-INP-DEBEM-QRTM-N    PIC  9(001)      VALUE ZEROS.
             10 WRK-INP-LANCAM-QRTM       PIC  X(001)      VALUE SPACES.
             10 WRK-INP-AGENCIA-QRTM.
               15 WRK-INP-AGENCIA-QRTM-N  PIC  9(005)      VALUE ZEROS.
             10 WRK-INP-CTADEB-QRTM.
               15 WRK-INP-CTADEB-QRTM-N   PIC  9(013)      VALUE ZEROS.
             10 WRK-INP-BAITIT-QRTM       PIC  X(001)      VALUE SPACES.
BSI001       10 WRK-INP-LMOTVO-QRTM       PIC  X(019)      VALUE SPACES.
BSI001       10 WRK-INP-CMOTVO-QRTM.
BSI001          15 WRK-INP-CMOTVO-QRTM-N
BSI001                                    PIC  9(003)      VALUE ZEROS.
BSI001       10 WRK-INP-RMOTVO-QRTM       PIC  X(015)      VALUE SPACES.
BSI001       10 WRK-INP-LAUTRZ-QRTM       PIC  X(010)      VALUE SPACES.
BSI001       10 WRK-INP-CAUTRZ-QRTM.

BSI001          15 WRK-INP-CAUTRZ-QRTM-N  PIC  9(009)      VALUE ZEROS.
BSI001       10 WRK-INP-NAUTRZ-QRTM       PIC  X(017)      VALUE SPACES.
BSI001       10 WRK-INP-JUSTIF-QRTM       PIC  X(059)      VALUE SPACES.
             10 WRK-INP-MENSA-QRTM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQRTM - OUTPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-OUTPUT-QRTM.
           05 WRK-OUT-LL-QRTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-OUT-ZZ-QRTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-OUT-DADOS-QRTM.
             10 WRK-OUT-SENHAS-QRTM       PIC  X(037)      VALUE SPACES.
             10 WRK-OUT-COMANDO-QRTM      PIC  X(068)      VALUE SPACES.
             10 WRK-OUT-OCULTOS-QRTM.
               15 WRK-OUT-OP-MENU-QRTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-OUT-OPCAO-QRTM      PIC  9(001)      VALUE ZEROS.
               15 WRK-OUT-TPMANUT-QRTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-OUT-PAGINA-QRTM     PIC  9(003)      VALUE ZEROS.

               15 WRK-OUT-PARCELA-QRTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-OUT-BCO-QRTM        PIC  9(003)      VALUE ZEROS.
               15 WRK-OUT-AGENOFF-QRTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-OUT-CONTA-QRTM      PIC  9(013)      VALUE ZEROS.
4S2511*        15 WRK-OUT-CNPJ-QRTM       PIC  9(009)      VALUE ZEROS.
4S2511         15 WRK-OUT-CNPJ-QRTM       PIC  X(009)      VALUE SPACES.
4S2511*        15 WRK-OUT-FILIAL-QRTM     PIC  9(004)      VALUE ZEROS.
4S2511         15 WRK-OUT-FILIAL-QRTM     PIC  X(004)      VALUE SPACES.
               15 WRK-OUT-CTRL-QRTM       PIC  9(002)      VALUE ZEROS.
               15 WRK-OUT-TIMES-QRTM      PIC  X(026)      VALUE SPACES.
               15 FILLER                  PIC  X(006)      VALUE SPACES.
             10 WRK-OUT-NOMECLI-QRTM      PIC  X(040)      VALUE SPACES.
             10 WRK-OUT-CHAMADOR-QRTM     PIC  X(008)      VALUE SPACES.
             10 WRK-OUT-CNPJCPF-QRTM      PIC  X(019)      VALUE SPACES.
             10 WRK-OUT-DADOSBC-QRTM.
               15 WRK-OUT-BANCO-QRTM      PIC  9(003)      VALUE ZEROS.
               15 WRK-OUT-BARRA2-QRTM     PIC  X(001)      VALUE SPACES.
               15 WRK-OUT-AGEN-QRTM       PIC  9(005)      VALUE ZEROS.
               15 WRK-OUT-BARRA3-QRTM     PIC  X(001)      VALUE SPACES.
               15 WRK-OUT-CTA-QRTM        PIC  9(013)      VALUE ZEROS.
             10 WRK-OUT-CODPROD-QRTM.
               15 WRK-OUT-CODPROD-QRTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-OUT-DSPROD-QRTM       PIC  X(012)      VALUE SPACES.

             10 WRK-OUT-SUBPROD-QRTM.
               15 WRK-OUT-SUBPROD-QRTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-OUT-DSSBPROD-QRTM     PIC  X(012)      VALUE SPACES.
             10 WRK-OUT-OPERACAO-QRTM.
               15 WRK-OUT-OPER1-QRTM      PIC  9(004)      VALUE ZEROS.
               15 WRK-OUT-OPER2-QRTM      PIC  9(009)      VALUE ZEROS.
             10 WRK-OUT-SITUA1-QRTM       PIC  X(015)      VALUE SPACES.
             10 WRK-OUT-SITUA2-QRTM       PIC  X(013)      VALUE SPACES.
             10 WRK-OUT-SITUA3-QRTM.
               15 WRK-OUT-SITUA3-QRTM-N   PIC  9(009)      VALUE ZEROS.
             10 WRK-OUT-SITUA4-QRTM       PIC  X(013)      VALUE SPACES.
             10 WRK-OUT-SITUA5-QRTM       PIC  X(009)      VALUE SPACES.
             10 WRK-OUT-QTPAR-QRTM.
               15 WRK-OUT-QTPAR-QRTM-N    PIC  9(005)      VALUE ZEROS.
             10 WRK-OUT-VLRPAR-QRTM.
               15 WRK-OUT-VLRPAR-QRTM-N   PIC ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-VLRATU-QRTM.
               15 WRK-OUT-VLRATU-QRTM-N   PIC ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-VLRBAIX-QRTM.

               15 WRK-OUT-VLRBAIX-QRTM-N  PIC ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-TPBAIXA-QRTM.
               15 WRK-OUT-TPBAIXA-QRTM-N  PIC  9(001)      VALUE ZEROS.
             10 WRK-OUT-DSBAIXA-QRTM      PIC  X(010)      VALUE SPACES.
             10 WRK-OUT-DEBEM-QRTM.
               15 WRK-OUT-DEBEM-QRTM-N    PIC  9(001)      VALUE ZEROS.
             10 WRK-OUT-LANCAM-QRTM       PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-AGENCIA-QRTM.
               15 WRK-OUT-AGENCIA-QRTM-N  PIC  9(005)      VALUE ZEROS.
             10 WRK-OUT-CTADEB-QRTM.
               15 WRK-OUT-CTADEB-QRTM-N   PIC  9(013)      VALUE ZEROS.
             10 WRK-OUT-BAITIT-QRTM       PIC  X(001)      VALUE SPACES.
BSI001       10 WRK-OUT-LMOTVO-QRTM       PIC  X(019)      VALUE SPACES.
BSI001       10 WRK-OUT-CMOTVO-QRTM.
BSI001          15 WRK-OUT-CMOTVO-QRTM-N
BSI001                                    PIC  9(003)      VALUE ZEROS.
BSI001       10 WRK-OUT-RMOTVO-QRTM       PIC  X(015)      VALUE SPACES.
BSI001       10 WRK-OUT-LAUTRZ-QRTM       PIC  X(010)      VALUE SPACES.
BSI001       10 WRK-OUT-CAUTRZ-QRTM.
BSI001          15 WRK-OUT-CAUTRZ-QRTM-N  PIC  9(009)      VALUE ZEROS.

BSI001       10 WRK-OUT-NAUTRZ-QRTM       PIC  X(017)      VALUE SPACES.
BSI001       10 WRK-OUT-JUSTIF-QRTM       PIC  X(059)      VALUE SPACES.
             10 WRK-OUT-MENSA-QRTM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                         PIC X(070)       VALUE
           '*** TELA DCOMQRTM - BRAD0660 - SEG. 00 ***'.
      *---------------------------------------------------------------*

       01  WRK-660-DCOMQRTM.
           05 WRK-660-LL-AREA-QRTM        PIC  9(004) COMP VALUE  0000.
           05 WRK-660-LL-MENSAGEM-QRTM    PIC  9(004) COMP VALUE  0000.
           05 WRK-660-SENHAS-QRTM         PIC  9(004) COMP VALUE  0037.
           05 WRK-660-COMANDO-QRTM        PIC  9(004) COMP VALUE  0068.
           05 WRK-660-OCULTOS-QRTM        PIC  9(004) COMP VALUE  0079.
           05 WRK-660-NOMECLI-QRTM        PIC  9(004) COMP VALUE  0040.
           05 WRK-660-CHAMADOR-QRTM       PIC  9(004) COMP VALUE  0008.
           05 WRK-660-CNPJCPF-QRTM        PIC  9(004) COMP VALUE  0019.
           05 WRK-660-DADOSBC-QRTM        PIC  9(004) COMP VALUE  0023.
           05 WRK-660-CODPROD-QRTM        PIC  9(004) COMP VALUE  0003.
           05 WRK-660-DSPROD-QRTM         PIC  9(004) COMP VALUE  0012.

           05 WRK-660-SUBPROD-QRTM        PIC  9(004) COMP VALUE  0003.
           05 WRK-660-DSSBPROD-QRTM       PIC  9(004) COMP VALUE  0012.
           05 WRK-660-OPERACAO-QRTM       PIC  9(004) COMP VALUE  0013.
           05 WRK-660-SITUA1-QRTM         PIC  9(004) COMP VALUE  0015.
           05 WRK-660-SITUA2-QRTM         PIC  9(004) COMP VALUE  0013.
           05 WRK-660-SITUA3-QRTM         PIC  9(004) COMP VALUE  0009.
           05 WRK-660-SITUA4-QRTM         PIC  9(004) COMP VALUE  0013.
           05 WRK-660-SITUA5-QRTM         PIC  9(004) COMP VALUE  0009.
           05 WRK-660-QTPAR-QRTM          PIC  9(004) COMP VALUE  0005.
           05 WRK-660-VLRPAR-QRTM         PIC  9(004) COMP VALUE  0017.
           05 WRK-660-VLRATU-QRTM         PIC  9(004) COMP VALUE  0017.
           05 WRK-660-VLRBAIX-QRTM        PIC  9(004) COMP VALUE  0017.
           05 WRK-660-TPBAIXA-QRTM        PIC  9(004) COMP VALUE  0001.
           05 WRK-660-DSBAIXA-QRTM        PIC  9(004) COMP VALUE  0010.
           05 WRK-660-DEBEM-QRTM          PIC  9(004) COMP VALUE  0001.
           05 WRK-660-LANCAM-QRTM         PIC  9(004) COMP VALUE  0001.
           05 WRK-660-AGENCIA-QRTM        PIC  9(004) COMP VALUE  0005.
           05 WRK-660-CTADEB-QRTM         PIC  9(004) COMP VALUE  0013.
           05 WRK-660-BAITIT-QRTM         PIC  9(004) COMP VALUE  0001.
BSI001     05 WRK-660-LMOTVO-QRTM         PIC  9(004) COMP VALUE  0019.
BSI001     05 WRK-660-CMOTVO-QRTM         PIC  9(004) COMP VALUE  0003.

BSI001     05 WRK-660-RMOTVO-QRTM         PIC  9(004) COMP VALUE  0015.
BSI001     05 WRK-660-LAUTRZ-QRTM         PIC  9(004) COMP VALUE  0010.
BSI001     05 WRK-660-CAUTRZ-QRTM         PIC  9(004) COMP VALUE  0009.
BSI001     05 WRK-660-NAUTRZ-QRTM         PIC  9(004) COMP VALUE  0017.
BSI001     05 WRK-660-JUSTIF-QRTM         PIC  9(004) COMP VALUE  0059.
           05 WRK-660-MENSA-QRTM          PIC  9(004) COMP VALUE  0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

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

           MOVE SPACES                 TO WRK-OUT-DADOS-QRTM.

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

           MOVE 'DCOMQRTM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1718'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMQRTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMQRTM'
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
           MOVE WRK-COMU-OP-MENU       TO WRK-INP-OP-MENU-QRTM
                                          WRK-OUT-OP-MENU-QRTM.
           MOVE WRK-COMU-OPCAO-N       TO WRK-INP-OPCAO-QRTM
                                          WRK-OUT-OPCAO-QRTM.
           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPMANUT-QRTM
                                          WRK-OUT-TPMANUT-QRTM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TIMES-QRTM
                                          WRK-OUT-TIMES-QRTM.
           MOVE WRK-COMU-PARCELA       TO WRK-INP-PARCELA-QRTM
                                          WRK-OUT-PARCELA-QRTM.
           MOVE WRK-COMU-BANCO         TO WRK-INP-BCO-QRTM
                                          WRK-OUT-BCO-QRTM.

           MOVE WRK-COMU-AGENCIA       TO WRK-INP-AGENOFF-QRTM
                                          WRK-OUT-AGENOFF-QRTM.
           MOVE WRK-COMU-CONTA         TO WRK-INP-CONTA-QRTM
                                          WRK-OUT-CONTA-QRTM.
           MOVE WRK-COMU-CCNPJ-CPF     TO WRK-INP-CNPJ-QRTM
                                          WRK-OUT-CNPJ-QRTM.
           MOVE WRK-COMU-CFLIAL-CNPJ   TO WRK-INP-FILIAL-QRTM
                                          WRK-OUT-FILIAL-QRTM.
           MOVE WRK-COMU-CCTRL-CNPJCPF TO WRK-INP-CTRL-QRTM
                                          WRK-OUT-CTRL-QRTM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGINA-QRTM
                                          WRK-OUT-PAGINA-QRTM.
           MOVE WRK-COMU-DANO-OPER     TO WRK-INP-OPER1-QRTM
                                          WRK-OUT-OPER1-QRTM.
           MOVE WRK-COMU-NSEQ-OPER     TO WRK-INP-OPER2-QRTM
                                          WRK-OUT-OPER2-QRTM.


           IF  WRK-COMU-TRANSACAO      NOT EQUAL 'DCOM1716'
               MOVE WRK-COMU-TRANSACAO TO WRK-INP-CHAMADOR-QRTM
                                          WRK-OUT-CHAMADOR-QRTM

           ELSE
               MOVE WRK-COMU-CHAMADOR  TO WRK-INP-CHAMADOR-QRTM
                                          WRK-OUT-CHAMADOR-QRTM
           END-IF.

           PERFORM 1110-ACESSAR-DCOM4174.

           IF  WRK-INCONSIS            NOT EQUAL '*'
               PERFORM 1120-ACESSAR-DCOM5307
               IF  WRK-INCONSIS        NOT EQUAL '*'
                   MOVE WRK-MENSA-02   TO WRK-OUT-MENSA-QRTM
               END-IF
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

           MOVE WRK-COD-USER           TO 4174-ENT-CFUNC-BDSCO.
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
                    MOVE '*'           TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *  ESSA ROTINA MONTA A AREA RETORNADA PELO PROGRAMA DCOM4174     *
      *----------------------------------------------------------------*
       1111-MONTAR-DCOM4174            SECTION.
      *----------------------------------------------------------------*


           MOVE 4174-SAI-IPSSOA-DESC-COML  TO WRK-OUT-NOMECLI-QRTM.

           STRING 4174-SAI-CCNPJ-CPF(1:3) '.'
                  4174-SAI-CCNPJ-CPF(4:3) '.'
                  4174-SAI-CCNPJ-CPF(7:3) '/'
                  4174-SAI-CFLIAL-CNPJ    '-'
                  4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE               INTO WRK-OUT-CNPJCPF-QRTM.

           STRING 4174-SAI-CBCO      '/'
                  4174-SAI-CAG-BCRIA '/'
                  4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE               INTO WRK-OUT-DADOSBC-QRTM.


           MOVE 4174-SAI-CPRODT            TO WRK-OUT-CODPROD-QRTM.
           MOVE 4174-SAI-IABREV-PRODT      TO WRK-OUT-DSPROD-QRTM.
           MOVE 4174-SAI-CSPROD-DESC-COML  TO WRK-OUT-SUBPROD-QRTM.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC TO WRK-OUT-DSSBPROD-QRTM.
           MOVE 4174-SAI-DANO-OPER-DESC    TO WRK-OUT-OPER1-QRTM.
           MOVE 4174-SAI-NSEQ-OPER-DESC    TO WRK-OUT-OPER2-QRTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC   TO WRK-OUT-SITUA1-QRTM.

           EVALUATE TRUE
               WHEN 4174-SAI-CCONTR-LIM-DESC      GREATER ZEROS
                    MOVE 'CONTR LIMITE:'          TO WRK-OUT-SITUA2-QRTM
                    MOVE 4174-SAI-CCONTR-LIM-DESC TO WRK-OUT-SITUA3-QRTM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 8
                    MOVE 'CONV GERAL..:'          TO WRK-OUT-SITUA2-QRTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITUA3-QRTM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND

                    4174-SAI-CELMTO-DESC-COML     EQUAL 9
                    MOVE 'CONV CLIENTE:'          TO WRK-OUT-SITUA2-QRTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITUA3-QRTM
           END-EVALUATE.

           IF  4174-SAI-NSMULA-OPER-DESC   GREATER ZEROS
               IF  WRK-OUT-SITUA3-QRTM     EQUAL SPACES
                   MOVE 'SIMULACAO...:'    TO WRK-OUT-SITUA2-QRTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-SITUA3-QRTM
               ELSE
                   MOVE 'SIMULACAO...:'    TO WRK-OUT-SITUA4-QRTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-SITUA5-QRTM
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
           MOVE WRK-INP-OPER1-QRTM     TO WRK-5307E-DANO-OPER.
           MOVE WRK-INP-OPER2-QRTM     TO WRK-5307E-NSEQ-OPER.
           MOVE WRK-COD-DEPTO-N        TO WRK-5307E-AGENCIA-FUNC.
           MOVE 'C'                    TO WRK-5307E-FLAG-FUNCAO.
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
                    MOVE '*'           TO WRK-INCONSIS
           END-EVALUATE.


      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  FORMATACAO DA TELA                                            *
      *----------------------------------------------------------------*
       1121-FORMATAR-TELA              SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-5307S-QTDE-PARCELAS  TO WRK-OUT-QTPAR-QRTM-N.

           MOVE WRK-5307S-VPRINC-PCELA   TO WRK-OUT-VLRPAR-QRTM-N.
           MOVE WRK-5307S-VATUAL-PCELA   TO WRK-OUT-VLRATU-QRTM-N.
           MOVE WRK-5307S-VEFETV-BAIXA   TO WRK-OUT-VLRBAIX-QRTM-N.
           MOVE WRK-5307S-CTPO-BX-PCELA  TO WRK-OUT-TPBAIXA-QRTM.
           MOVE WRK-5307S-IRSUMO-TPO-BX  TO WRK-OUT-DSBAIXA-QRTM.
           MOVE WRK-5307S-CINDCD-LOC-DEB TO WRK-OUT-DEBEM-QRTM-N
           MOVE WRK-5307S-CINDCD-CON-EXT TO WRK-OUT-LANCAM-QRTM.
           MOVE WRK-5307S-CINDCD-BX-COBR TO WRK-OUT-BAITIT-QRTM.


           IF  WRK-5307S-CINDCD-LOC-DEB  EQUAL 2
               MOVE SPACES               TO WRK-OUT-AGENCIA-QRTM
                                            WRK-OUT-CTADEB-QRTM
           ELSE
               MOVE WRK-5307S-CAG-BCRIA  TO WRK-OUT-AGENCIA-QRTM-N
               MOVE WRK-5307S-CCTA-BCRIA TO WRK-OUT-CTADEB-QRTM-N
           END-IF.

BSI001     IF  WRK-5307S-CMOTVO-BAIXA-PREJ GREATER ZEROS
BSI001         MOVE 'MOTIVO BAIXA......:'  TO WRK-OUT-LMOTVO-QRTM
BSI001         MOVE WRK-5307S-CMOTVO-BAIXA-PREJ
BSI001                                     TO WRK-OUT-CMOTVO-QRTM-N
BSI001         STRING '- ' WRK-5307S-RMOTVO-BAIXA-PREJ(1:13)
BSI001         DELIMITED BY SIZE         INTO WRK-OUT-RMOTVO-QRTM
BSI001     ELSE
BSI001         MOVE SPACES                 TO WRK-OUT-LMOTVO-QRTM
BSI001                                        WRK-OUT-CMOTVO-QRTM
BSI001                                        WRK-OUT-RMOTVO-QRTM
BSI001     END-IF.
BSI001

BSI001     IF  WRK-5307S-CFUNC-BDSCO-AUTRZ GREATER ZEROS
BSI001         MOVE 'COD. AUT.:'           TO WRK-OUT-LAUTRZ-QRTM
BSI001         MOVE WRK-5307S-CFUNC-BDSCO-AUTRZ
BSI001                                     TO WRK-OUT-CAUTRZ-QRTM-N
BSI001         STRING '- ' WRK-5307S-NFUNC-BDSCO-AUTRZ(1:15)
BSI001         DELIMITED BY SIZE         INTO WRK-OUT-NAUTRZ-QRTM
BSI001     ELSE
BSI001         MOVE SPACES                 TO WRK-OUT-LAUTRZ-QRTM
BSI001                                        WRK-OUT-CAUTRZ-QRTM
BSI001                                        WRK-OUT-NAUTRZ-QRTM
BSI001     END-IF.
BSI001
BSI001     MOVE WRK-5307S-RJUSTF-BX-PARC(1:59)
BSI001                                     TO WRK-OUT-JUSTIF-QRTM.
BSI001
      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL                                        *

      ******************************************************************
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMQRTM         SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-MENSAGEM           TO WRK-INPUT-QRTM.

           PERFORM 1260-DEVOLVER-TELA

           EVALUATE TRUE
             WHEN  (WRK-INP-PFK-QRTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QRTM   EQUAL 'PFK01')  OR
                    WRK-INP-PFK-QRTM       EQUAL 'H'
                    MOVE WRK-INP-MENSA-QRTM TO WRK-OUT-MENSA-QRTM

             WHEN  (WRK-INP-PFK-QRTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QRTM   EQUAL 'PFK03')  OR
                    WRK-INP-PFK-QRTM       EQUAL '3'
                    PERFORM 1210-RETORNAR-CHAMADOR


             WHEN  (WRK-INP-PFK-QRTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QRTM   EQUAL 'PFK05')  OR
                    WRK-INP-PFK-QRTM       EQUAL '5'
                    PERFORM 1220-TRATAR-PF5

             WHEN  (WRK-INP-PFK-QRTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QRTM   EQUAL 'PFK10')  OR
                    WRK-INP-PFK-QRTM       EQUAL 'A'
                    PERFORM 1230-TRATAR-PF10

             WHEN  (WRK-INP-PFK-QRTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QRTM   EQUAL 'PFK11')  OR
                    WRK-INP-PFK-QRTM       EQUAL 'B'
                    PERFORM 1250-TRATAR-PF11

             WHEN  (WRK-INP-PFK-QRTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QRTM   EQUAL SPACES)
                    MOVE WRK-INP-MENSA-QRTM TO WRK-OUT-MENSA-QRTM

             WHEN   OTHER
                    MOVE WRK-MENSA-01       TO WRK-OUT-MENSA-QRTM

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
           MOVE WRK-INP-CHAMADOR-QRTM  TO WRK-TELA.
           MOVE 'DCOM1718'             TO WRK-COMU-TRANSACAO.
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


           MOVE WRK-INP-OP-MENU-QRTM   TO WRK-COMU-OP-MENU.
           MOVE WRK-INP-OPCAO-QRTM     TO WRK-COMU-OPCAO-N.
           MOVE WRK-INP-TPMANUT-QRTM   TO WRK-COMU-TP-SELECAO.
           MOVE WRK-INP-TIMES-QRTM     TO WRK-COMU-TIMESTAMP.
           MOVE WRK-INP-PARCELA-QRTM   TO WRK-COMU-PARCELA.
           MOVE WRK-INP-BCO-QRTM       TO WRK-COMU-BANCO.
           MOVE WRK-INP-AGENOFF-QRTM   TO WRK-COMU-AGENCIA.

           MOVE WRK-INP-CONTA-QRTM     TO WRK-COMU-CONTA.
           MOVE WRK-INP-CNPJ-QRTM      TO WRK-COMU-CCNPJ-CPF.
           MOVE WRK-INP-FILIAL-QRTM    TO WRK-COMU-CFLIAL-CNPJ.
           MOVE WRK-INP-CTRL-QRTM      TO WRK-COMU-CCTRL-CNPJCPF.
           MOVE WRK-INP-PAGINA-QRTM    TO WRK-COMU-PAGINA-1.
           MOVE WRK-INP-OPER1-QRTM     TO WRK-COMU-DANO-OPER.
           MOVE WRK-INP-OPER2-QRTM     TO WRK-COMU-NSEQ-OPER.


      *----------------------------------------------------------------*
       1211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA O SENHAS - MENU DE ROTINAS                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-TRATAR-PF5                 SECTION.
      *----------------------------------------------------------------*



           MOVE WRK-CHNG                TO WRK-FUNCAO.
           MOVE 'DCOM1718'              TO WRK-TELA
                                           WRK-COMU-TRANSACAO.
           MOVE '5'                     TO WRK-COMU-PFK.
           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL.
           MOVE WRK-COMU-AREA           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA O SENHAS - MENU DE DCOM                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1230-TRATAR-PF10                SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-CHNG                TO WRK-FUNCAO.
           MOVE 'DCOM1718'              TO WRK-TELA

                                           WRK-COMU-TRANSACAO.
           MOVE 'A'                     TO WRK-COMU-PFK.
           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL.
           MOVE WRK-COMU-AREA           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PASSAR CONTROLE PARA O PROGRAMA DE RESUMO DAS PARCELAS         *
      ******************************************************************
      *----------------------------------------------------------------*
       1250-TRATAR-PF11                SECTION.
      *----------------------------------------------------------------*


           PERFORM 1211-FORMATAR-COMU-AREA.

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-INP-CHAMADOR-QRTM  TO WRK-COMU-CHAMADOR.

           MOVE 'DCOM1718'             TO WRK-COMU-TRANSACAO.
           MOVE 'DCOM1716'             TO WRK-TELA.
           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
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


           MOVE WRK-INP-OCULTOS-QRTM   TO WRK-OUT-OCULTOS-QRTM.
           MOVE WRK-INP-NOMECLI-QRTM   TO WRK-OUT-NOMECLI-QRTM.
           MOVE WRK-INP-CHAMADOR-QRTM  TO WRK-OUT-CHAMADOR-QRTM.

           MOVE WRK-INP-CNPJCPF-QRTM   TO WRK-OUT-CNPJCPF-QRTM.
           MOVE WRK-INP-DADOSBC-QRTM   TO WRK-OUT-DADOSBC-QRTM.
           MOVE WRK-INP-CODPROD-QRTM   TO WRK-OUT-CODPROD-QRTM.
           MOVE WRK-INP-DSPROD-QRTM    TO WRK-OUT-DSPROD-QRTM.
           MOVE WRK-INP-SUBPROD-QRTM   TO WRK-OUT-SUBPROD-QRTM.
           MOVE WRK-INP-DSSBPROD-QRTM  TO WRK-OUT-DSSBPROD-QRTM.
           MOVE WRK-INP-OPERACAO-QRTM  TO WRK-OUT-OPERACAO-QRTM.
           MOVE WRK-INP-SITUA1-QRTM    TO WRK-OUT-SITUA1-QRTM.
           MOVE WRK-INP-SITUA2-QRTM    TO WRK-OUT-SITUA2-QRTM.
           MOVE WRK-INP-SITUA3-QRTM    TO WRK-OUT-SITUA3-QRTM.
           MOVE WRK-INP-SITUA4-QRTM    TO WRK-OUT-SITUA4-QRTM.
           MOVE WRK-INP-SITUA5-QRTM    TO WRK-OUT-SITUA5-QRTM.
           MOVE WRK-INP-QTPAR-QRTM     TO WRK-OUT-QTPAR-QRTM.
           MOVE WRK-INP-VLRPAR-QRTM    TO WRK-OUT-VLRPAR-QRTM.
           MOVE WRK-INP-VLRATU-QRTM    TO WRK-OUT-VLRATU-QRTM.
           MOVE WRK-INP-VLRBAIX-QRTM   TO WRK-OUT-VLRBAIX-QRTM.
           MOVE WRK-INP-TPBAIXA-QRTM   TO WRK-OUT-TPBAIXA-QRTM.
           MOVE WRK-INP-DSBAIXA-QRTM   TO WRK-OUT-DSBAIXA-QRTM.
           MOVE WRK-INP-DEBEM-QRTM     TO WRK-OUT-DEBEM-QRTM.
           MOVE WRK-INP-LANCAM-QRTM    TO WRK-OUT-LANCAM-QRTM.
           MOVE WRK-INP-AGENCIA-QRTM   TO WRK-OUT-AGENCIA-QRTM.

           MOVE WRK-INP-CTADEB-QRTM    TO WRK-OUT-CTADEB-QRTM.
           MOVE WRK-INP-BAITIT-QRTM    TO WRK-OUT-BAITIT-QRTM.
BSI001     MOVE WRK-INP-LMOTVO-QRTM    TO WRK-OUT-LMOTVO-QRTM.
BSI001     MOVE WRK-INP-CMOTVO-QRTM    TO WRK-OUT-CMOTVO-QRTM.
BSI001     MOVE WRK-INP-RMOTVO-QRTM    TO WRK-OUT-RMOTVO-QRTM.
BSI001     MOVE WRK-INP-LAUTRZ-QRTM    TO WRK-OUT-LAUTRZ-QRTM.
BSI001     MOVE WRK-INP-CAUTRZ-QRTM    TO WRK-OUT-CAUTRZ-QRTM.
BSI001     MOVE WRK-INP-NAUTRZ-QRTM    TO WRK-OUT-NAUTRZ-QRTM.
BSI001     MOVE WRK-INP-JUSTIF-QRTM    TO WRK-OUT-JUSTIF-QRTM.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSA BRAD660 COMPACTA DADOS PARA DCOMQRTM                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*



           MOVE LENGTH                 OF WRK-OUTPUT-QRTM
                                       TO WRK-OUT-LL-QRTM
                                          WRK-660-LL-MENSAGEM-QRTM.
           MOVE LENGTH                 OF WRK-660-DCOMQRTM
                                       TO WRK-660-LL-AREA-QRTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-QRTM
                                             WRK-660-DCOMQRTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-COD-RET
               MOVE WRK-COD-RET-N      TO WRK-RETURN-CODE
               MOVE 010                TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-QRTM        TO WRK-MENSAGEM.


      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA A BRAD7100 - TRATAMENTO DE ERRO                        *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES OR LOW-VALUES
               MOVE 'SENH7020'         TO ERR-PGM
               MOVE 'DCOM1718'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM1718'         TO ERR-PGM
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
