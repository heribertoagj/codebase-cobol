      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1717.
       AUTHOR.     FABRICA-ALTRAN.
      *================================================================*
      *                   A L T R A N                                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  DCOM1717                                     *
      *    ANALISTA....:  FERNANDO LUIZ DE SANTI                       *
      *    DATA........:  11/01/2008                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  EXCLUIR ESTORNO DA BAIXA DA PARCELA POR      *
      *      RATEIO - DETALHES.                                        *
      *----------------------------------------------------------------*
      *    TELA........:  DCOMQQTM - EXCLUIR ESTORNO DA BAIXA DA       *
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
      *    DCOM5312 - EXCLUIR MOVIMENTO DE ESTORNO BAIXA RATEIO.       *
      *    POOL5005 - ENVIA / RECEBE MENSAGENS DO IMS.                 *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.:   DCOM1702                  CHAMA: DCOM1716   *
      *                    DCOM1703                                    *
      *                    DCOM1704                                    *
      *================================================================*
BSI001*                     BSI TECNOLOGIA - ALTERACAO                 *
BSI001*----------------------------------------------------------------*
BSI001*    PROGRAMADOR.:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI001*    ANALISTA BSI:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI001*    ANALISTA DDS:  RICARDO JAMMAL     - GRUPO 39                *
BSI001*    DATA........:  01/08/2016                                   *

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
           'TECLE <PF6> PARA CONFIRMAR OU <PF11> PARA VISUALIZAR O RESUM
      -    'O'.
       77  WRK-MENSA-03                PIC  X(079)         VALUE
           'EXCLUSAO EFETUADA COM SUCESSO'.


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
           '*** AREA DE COMUNICACAO - DCOM5312 ***'.
      *----------------------------------------------------------------*

       01  WRK-5312-ENTRADA.

           05 WRK-5312E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5312E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5312E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5312E-FLAG           PIC  9(001)         VALUE ZEROS.
           05 WRK-5312E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5312E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5312E-OPERACAO       PIC  9(013)         VALUE ZEROS.
           05 FILLER REDEFINES         WRK-5312E-OPERACAO.
             10 WRK-5312E-DANO-OPER    PIC  9(004).
             10 WRK-5312E-NSEQ-OPER    PIC  9(009).
           05 WRK-5312E-AGENCIA-FUNC   PIC  9(005)         VALUE ZEROS.
           05 WRK-5312E-CAG-BCRIA-BX   PIC  9(005)         VALUE ZEROS.
           05 WRK-5312E-CCTA-BCRIA-CLI-BX
                                       PIC  9(013)         VALUE ZEROS.
           05 WRK-5312E-CINDCD-LOC-DEB-BX
                                       PIC  X(001)         VALUE SPACES.

       01  WRK-5312-SAIDA.
           05 WRK-5312S-COD-RETORNO    PIC  9(004)         VALUE ZEROS.
           05 WRK-5312S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.


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

           '*** TELA DCOMQQTM - INPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-INPUT-QQTM.
           05 WRK-INP-LL-QQTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-INP-ZZ-QQTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-INP-TRANCODE-QQTM       PIC  X(008)      VALUE SPACES.
           05 FILLER                      PIC  X(006)      VALUE SPACES.
           05 WRK-INP-DADOS-QQTM.
             10 WRK-INP-PFK-QQTM          PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SENHAS-QQTM       PIC  X(037)      VALUE SPACES.
             10 WRK-INP-COMANDO-QQTM      PIC  X(068)      VALUE SPACES.
             10 WRK-INP-OCULTOS-QQTM.
               15 WRK-INP-OP-MENU-QQTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-INP-OPCAO-QQTM      PIC  9(001)      VALUE ZEROS.
               15 WRK-INP-TPMANUT-QQTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-INP-PAGINA-QQTM     PIC  9(003)      VALUE ZEROS.
               15 WRK-INP-PARCELA-QQTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-INP-BCO-QQTM        PIC  9(003)      VALUE ZEROS.
               15 WRK-INP-AGENOFF-QQTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-INP-CONTA-QQTM      PIC  9(013)      VALUE ZEROS.
4S2511*        15 WRK-INP-CNPJ-QQTM       PIC  9(009)      VALUE ZEROS.
4S2511         15 WRK-INP-CNPJ-QQTM       PIC  X(009)      VALUE SPACES.
4S2511*        15 WRK-INP-FILIAL-QQTM     PIC  9(004)      VALUE ZEROS.
4S2511         15 WRK-INP-FILIAL-QQTM     PIC  X(004)      VALUE SPACES.
               15 WRK-INP-CTRL-QQTM       PIC  9(002)      VALUE ZEROS.
               15 WRK-INP-TIMES-QQTM      PIC  X(026)      VALUE SPACES.
               15 FILLER                  PIC  X(006)      VALUE SPACES.
             10 WRK-INP-NOMECLI-QQTM      PIC  X(040)      VALUE SPACES.
             10 WRK-INP-CHAMADOR-QQTM     PIC  X(008)      VALUE SPACES.
             10 WRK-INP-CNPJCPF-QQTM      PIC  X(019)      VALUE SPACES.
             10 WRK-INP-DADOSBC-QQTM.
               15 WRK-INP-BANCO-QQTM      PIC  9(003)      VALUE ZEROS.
               15 WRK-INP-BARRA2-QQTM     PIC  X(001)      VALUE SPACES.
               15 WRK-INP-AGEN-QQTM       PIC  9(005)      VALUE ZEROS.
               15 WRK-INP-BARRA3-QQTM     PIC  X(001)      VALUE SPACES.
               15 WRK-INP-CTA-QQTM        PIC  9(013)      VALUE ZEROS.
             10 WRK-INP-CODPROD-QQTM.
               15 WRK-INP-CODPROD-QQTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-DSPROD-QQTM       PIC  X(012)      VALUE SPACES.
             10 WRK-INP-SUBPROD-QQTM.
               15 WRK-INP-SUBPROD-QQTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-DSSBPROD-QQTM     PIC  X(012)      VALUE SPACES.
             10 WRK-INP-OPERACAO-QQTM.

               15 WRK-INP-OPER1-QQTM      PIC  9(004)      VALUE ZEROS.
               15 WRK-INP-OPER2-QQTM      PIC  9(009)      VALUE ZEROS.
             10 WRK-INP-SITUA1-QQTM       PIC  X(015)      VALUE SPACES.
             10 WRK-INP-SITUA2-QQTM       PIC  X(013)      VALUE SPACES.
             10 WRK-INP-SITUA3-QQTM.
               15 WRK-INP-SITUA3-QQTM-N   PIC  9(009)      VALUE ZEROS.
             10 WRK-INP-SITUA4-QQTM       PIC  X(013)      VALUE SPACES.
             10 WRK-INP-SITUA5-QQTM       PIC  X(009)      VALUE SPACES.
             10 WRK-INP-QTPAR-QQTM        PIC  9(005)      VALUE ZEROS.
             10 WRK-INP-VLRPAR-QQTM       PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-INP-VLRATU-QQTM       PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-INP-VLRBAIX-QQTM.
               15 WRK-INP-VLRBAIX-QQTM-N  PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-INP-TPBAIXA-QQTM.
               15 WRK-INP-TPBAIXA-QQTM-N  PIC  9(001)      VALUE ZEROS.
             10 WRK-INP-DSBAIXA-QQTM      PIC  X(010)      VALUE SPACES.
             10 WRK-INP-DEBEM-QQTM        PIC  X(001)      VALUE SPACES.
             10 WRK-INP-LANCAM-QQTM       PIC  X(001)      VALUE SPACES.

             10 WRK-INP-AGENCIA-QQTM.
               15 WRK-INP-AGENCIA-QQTM-N  PIC  9(005)      VALUE ZEROS.
             10 WRK-INP-CTADEB-QQTM.
               15 WRK-INP-CTADEB-QQTM-N   PIC  9(013)      VALUE ZEROS.
             10 WRK-INP-BAITIT-QQTM       PIC  X(001)      VALUE SPACES.
BSI001       10 WRK-INP-LMOTVO-QQTM       PIC  X(019)      VALUE SPACES.
BSI001       10 WRK-INP-CMOTVO-QQTM.
BSI001          15 WRK-INP-CMOTVO-QQTM-N
BSI001                                    PIC  9(003)      VALUE ZEROS.
BSI001       10 WRK-INP-RMOTVO-QQTM       PIC  X(015)      VALUE SPACES.
BSI001       10 WRK-INP-LAUTRZ-QQTM       PIC  X(010)      VALUE SPACES.
BSI001       10 WRK-INP-CAUTRZ-QQTM.
BSI001          15 WRK-INP-CAUTRZ-QQTM-N  PIC  9(009)      VALUE ZEROS.
BSI001       10 WRK-INP-NAUTRZ-QQTM       PIC  X(017)      VALUE SPACES.
BSI001       10 WRK-INP-JUSTIF-QQTM       PIC  X(059)      VALUE SPACES.
             10 WRK-INP-MENSA-QQTM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQQTM - OUTPUT ***'.
      *---------------------------------------------------------------*


       01  WRK-OUTPUT-QQTM.
           05 WRK-OUT-LL-QQTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-OUT-ZZ-QQTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-OUT-DADOS-QQTM.
             10 WRK-OUT-SENHAS-QQTM       PIC  X(037)      VALUE SPACES.
             10 WRK-OUT-COMANDO-QQTM      PIC  X(068)      VALUE SPACES.
             10 WRK-OUT-OCULTOS-QQTM.
               15 WRK-OUT-OP-MENU-QQTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-OUT-OPCAO-QQTM      PIC  9(001)      VALUE ZEROS.
               15 WRK-OUT-TPMANUT-QQTM    PIC  9(001)      VALUE ZEROS.
               15 WRK-OUT-PAGINA-QQTM     PIC  9(003)      VALUE ZEROS.
               15 WRK-OUT-PARCELA-QQTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-OUT-BCO-QQTM        PIC  9(003)      VALUE ZEROS.
               15 WRK-OUT-AGENOFF-QQTM    PIC  9(005)      VALUE ZEROS.
               15 WRK-OUT-CONTA-QQTM      PIC  9(013)      VALUE ZEROS.
4S2511*        15 WRK-OUT-CNPJ-QQTM       PIC  9(009)      VALUE ZEROS.
4S2511         15 WRK-OUT-CNPJ-QQTM       PIC  X(009)      VALUE SPACES.
4S2511*        15 WRK-OUT-FILIAL-QQTM     PIC  9(004)      VALUE ZEROS.
4S2511         15 WRK-OUT-FILIAL-QQTM     PIC  X(004)      VALUE SPACES.
               15 WRK-OUT-CTRL-QQTM       PIC  9(002)      VALUE ZEROS.
               15 WRK-OUT-TIMES-QQTM      PIC  X(026)      VALUE SPACES.
               15 FILLER                  PIC  X(006)      VALUE SPACES.

             10 WRK-OUT-NOMECLI-QQTM      PIC  X(040)      VALUE SPACES.
             10 WRK-OUT-CHAMADOR-QQTM     PIC  X(008)      VALUE SPACES.
             10 WRK-OUT-CNPJCPF-QQTM      PIC  X(019)      VALUE SPACES.
             10 WRK-OUT-DADOSBC-QQTM.
               15 WRK-OUT-BANCO-QQTM      PIC  9(003)      VALUE ZEROS.
               15 WRK-OUT-BARRA2-QQTM     PIC  X(001)      VALUE SPACES.
               15 WRK-OUT-AGEN-QQTM       PIC  9(005)      VALUE ZEROS.
               15 WRK-OUT-BARRA3-QQTM     PIC  X(001)      VALUE SPACES.
               15 WRK-OUT-CTA-QQTM        PIC  9(013)      VALUE ZEROS.
             10 WRK-OUT-CODPROD-QQTM.
               15 WRK-OUT-CODPROD-QQTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-OUT-DSPROD-QQTM       PIC  X(012)      VALUE SPACES.
             10 WRK-OUT-SUBPROD-QQTM.
               15 WRK-OUT-SUBPROD-QQTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-OUT-DSSBPROD-QQTM     PIC  X(012)      VALUE SPACES.
             10 WRK-OUT-OPERACAO-QQTM.
               15 WRK-OUT-OPER1-QQTM      PIC  9(004)      VALUE ZEROS.
               15 WRK-OUT-OPER2-QQTM      PIC  9(009)      VALUE ZEROS.
             10 WRK-OUT-SITUA1-QQTM       PIC  X(015)      VALUE SPACES.
             10 WRK-OUT-SITUA2-QQTM       PIC  X(013)      VALUE SPACES.
             10 WRK-OUT-SITUA3-QQTM.

               15 WRK-OUT-SITUA3-QQTM-N   PIC  9(009)      VALUE ZEROS.
             10 WRK-OUT-SITUA4-QQTM       PIC  X(013)      VALUE SPACES.
             10 WRK-OUT-SITUA5-QQTM       PIC  X(009)      VALUE SPACES.
             10 WRK-OUT-QTPAR-QQTM.
               15 WRK-OUT-QTPAR-QQTM-N    PIC  9(005)      VALUE ZEROS.
             10 WRK-OUT-VLRPAR-QQTM.
               15 WRK-OUT-VLRPAR-QQTM-N   PIC ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-VLRATU-QQTM.
               15 WRK-OUT-VLRATU-QQTM-N   PIC ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-VLRBAIX-QQTM.
               15 WRK-OUT-VLRBAIX-QQTM-N  PIC ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-TPBAIXA-QQTM.
               15 WRK-OUT-TPBAIXA-QQTM-N  PIC  9(001)      VALUE ZEROS.
             10 WRK-OUT-DSBAIXA-QQTM      PIC  X(010)      VALUE SPACES.
             10 WRK-OUT-DEBEM-QQTM        PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-LANCAM-QQTM       PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-AGENCIA-QQTM.
               15 WRK-OUT-AGENCIA-QQTM-N  PIC  9(005)      VALUE ZEROS.

             10 WRK-OUT-CTADEB-QQTM.
               15 WRK-OUT-CTADEB-QQTM-N   PIC  9(013)      VALUE ZEROS.
             10 WRK-OUT-BAITIT-QQTM       PIC  X(001)      VALUE SPACES.
BSI001       10 WRK-OUT-LMOTVO-QQTM       PIC  X(019)      VALUE SPACES.
BSI001       10 WRK-OUT-CMOTVO-QQTM.
BSI001          15 WRK-OUT-CMOTVO-QQTM-N
BSI001                                    PIC  9(003)      VALUE ZEROS.
BSI001       10 WRK-OUT-RMOTVO-QQTM       PIC  X(015)      VALUE SPACES.
BSI001       10 WRK-OUT-LAUTRZ-QQTM       PIC  X(010)      VALUE SPACES.
BSI001       10 WRK-OUT-CAUTRZ-QQTM.
BSI001          15 WRK-OUT-CAUTRZ-QQTM-N  PIC  9(009)      VALUE ZEROS.
BSI001       10 WRK-OUT-NAUTRZ-QQTM       PIC  X(017)      VALUE SPACES.
BSI001       10 WRK-OUT-JUSTIF-QQTM       PIC  X(059)      VALUE SPACES.
             10 WRK-OUT-MENSA-QQTM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                         PIC X(070)       VALUE
           '*** TELA DCOMQQTM - BRAD0660 - SEG. 00 ***'.
      *---------------------------------------------------------------*

       01  WRK-660-DCOMQQTM.

           05 WRK-660-LL-AREA-QQTM        PIC  9(004) COMP VALUE  0000.
           05 WRK-660-LL-MENSAGEM-QQTM    PIC  9(004) COMP VALUE  0000.
           05 WRK-660-SENHAS-QQTM         PIC  9(004) COMP VALUE  0037.
           05 WRK-660-COMANDO-QQTM        PIC  9(004) COMP VALUE  0068.
           05 WRK-660-OCULTOS-QQTM        PIC  9(004) COMP VALUE  0079.
           05 WRK-660-NOMECLI-QQTM        PIC  9(004) COMP VALUE  0040.
           05 WRK-660-CHAMADOR-QQTM       PIC  9(004) COMP VALUE  0008.
           05 WRK-660-CNPJCPF-QQTM        PIC  9(004) COMP VALUE  0019.
           05 WRK-660-DADOSBC-QQTM        PIC  9(004) COMP VALUE  0023.
           05 WRK-660-CODPROD-QQTM        PIC  9(004) COMP VALUE  0003.
           05 WRK-660-DSPROD-QQTM         PIC  9(004) COMP VALUE  0012.
           05 WRK-660-SUBPROD-QQTM        PIC  9(004) COMP VALUE  0003.
           05 WRK-660-DSSBPROD-QQTM       PIC  9(004) COMP VALUE  0012.
           05 WRK-660-OPERACAO-QQTM       PIC  9(004) COMP VALUE  0013.
           05 WRK-660-SITUA1-QQTM         PIC  9(004) COMP VALUE  0015.
           05 WRK-660-SITUA2-QQTM         PIC  9(004) COMP VALUE  0013.
           05 WRK-660-SITUA3-QQTM         PIC  9(004) COMP VALUE  0009.
           05 WRK-660-SITUA4-QQTM         PIC  9(004) COMP VALUE  0013.
           05 WRK-660-SITUA5-QQTM         PIC  9(004) COMP VALUE  0009.
           05 WRK-660-QTPAR-QQTM          PIC  9(004) COMP VALUE  0005.
           05 WRK-660-VLRPAR-QQTM         PIC  9(004) COMP VALUE  0017.

           05 WRK-660-VLRATU-QQTM         PIC  9(004) COMP VALUE  0017.
           05 WRK-660-VLRBAIX-QQTM        PIC  9(004) COMP VALUE  0017.
           05 WRK-660-TPBAIXA-QQTM        PIC  9(004) COMP VALUE  0001.
           05 WRK-660-DSBAIXA-QQTM        PIC  9(004) COMP VALUE  0010.
           05 WRK-660-DEBEM-QQTM          PIC  9(004) COMP VALUE  0001.
           05 WRK-660-LANCAM-QQTM         PIC  9(004) COMP VALUE  0001.
           05 WRK-660-AGENCIA-QQTM        PIC  9(004) COMP VALUE  0005.
           05 WRK-660-CTADEB-QQTM         PIC  9(004) COMP VALUE  0013.
           05 WRK-660-BAITIT-QQTM         PIC  9(004) COMP VALUE  0001.
BSI001     05 WRK-660-LMOTVO-QQTM         PIC  9(004) COMP VALUE  0019.
BSI001     05 WRK-660-CMOTVO-QQTM         PIC  9(004) COMP VALUE  0003.
BSI001     05 WRK-660-RMOTVO-QQTM         PIC  9(004) COMP VALUE  0015.
BSI001     05 WRK-660-LAUTRZ-QQTM         PIC  9(004) COMP VALUE  0010.
BSI001     05 WRK-660-CAUTRZ-QQTM         PIC  9(004) COMP VALUE  0009.
BSI001     05 WRK-660-NAUTRZ-QQTM         PIC  9(004) COMP VALUE  0017.
BSI001     05 WRK-660-JUSTIF-QQTM         PIC  9(004) COMP VALUE  0059.
           05 WRK-660-MENSA-QQTM          PIC  9(004) COMP VALUE  0079.

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


           MOVE SPACES                 TO WRK-OUT-DADOS-QQTM.

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

           MOVE 'DCOMQQTM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1717'
               PERFORM 1100-RECEBER-CONTROLE

           ELSE
               PERFORM 1200-PROCESSAR-DCOMQQTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMQQTM'
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

           MOVE WRK-COMU-OP-MENU       TO WRK-INP-OP-MENU-QQTM
                                          WRK-OUT-OP-MENU-QQTM.
           MOVE WRK-COMU-OPCAO-N       TO WRK-INP-OPCAO-QQTM
                                          WRK-OUT-OPCAO-QQTM.
           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPMANUT-QQTM
                                          WRK-OUT-TPMANUT-QQTM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TIMES-QQTM
                                          WRK-OUT-TIMES-QQTM.
           MOVE WRK-COMU-PARCELA       TO WRK-INP-PARCELA-QQTM
                                          WRK-OUT-PARCELA-QQTM.
           MOVE WRK-COMU-BANCO         TO WRK-INP-BCO-QQTM
                                          WRK-OUT-BCO-QQTM.
           MOVE WRK-COMU-AGENCIA       TO WRK-INP-AGENOFF-QQTM
                                          WRK-OUT-AGENOFF-QQTM.
           MOVE WRK-COMU-CONTA         TO WRK-INP-CONTA-QQTM
                                          WRK-OUT-CONTA-QQTM.
           MOVE WRK-COMU-CCNPJ-CPF     TO WRK-INP-CNPJ-QQTM
                                          WRK-OUT-CNPJ-QQTM.
           MOVE WRK-COMU-CFLIAL-CNPJ   TO WRK-INP-FILIAL-QQTM
                                          WRK-OUT-FILIAL-QQTM.
           MOVE WRK-COMU-CCTRL-CNPJCPF TO WRK-INP-CTRL-QQTM

                                          WRK-OUT-CTRL-QQTM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGINA-QQTM
                                          WRK-OUT-PAGINA-QQTM.
           MOVE WRK-COMU-DANO-OPER     TO WRK-INP-OPER1-QQTM
                                          WRK-OUT-OPER1-QQTM.
           MOVE WRK-COMU-NSEQ-OPER     TO WRK-INP-OPER2-QQTM
                                          WRK-OUT-OPER2-QQTM.


           IF  WRK-COMU-TRANSACAO      NOT EQUAL 'DCOM1716'
               MOVE WRK-COMU-TRANSACAO TO WRK-INP-CHAMADOR-QQTM
                                          WRK-OUT-CHAMADOR-QQTM
           ELSE
               MOVE WRK-COMU-CHAMADOR  TO WRK-INP-CHAMADOR-QQTM
                                          WRK-OUT-CHAMADOR-QQTM
           END-IF.


           PERFORM 1110-ACESSAR-DCOM4174.

           IF  WRK-INCONSIS            NOT EQUAL '*'

               PERFORM 1120-ACESSAR-DCOM5307
               IF  WRK-INCONSIS        NOT EQUAL '*'
                   IF  WRK-COMU-MENSAGEM   EQUAL SPACES OR LOW-VALUES
                       MOVE WRK-MENSA-02   TO WRK-OUT-MENSA-QQTM
                   ELSE
                       MOVE WRK-COMU-MENSAGEM
                                           TO WRK-OUT-MENSA-QQTM
                   END-IF
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



           MOVE 4174-SAI-IPSSOA-DESC-COML  TO WRK-OUT-NOMECLI-QQTM.

           STRING 4174-SAI-CCNPJ-CPF(1:3) '.'
                  4174-SAI-CCNPJ-CPF(4:3) '.'
                  4174-SAI-CCNPJ-CPF(7:3) '/'
                  4174-SAI-CFLIAL-CNPJ    '-'
                  4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE               INTO WRK-OUT-CNPJCPF-QQTM.

           STRING 4174-SAI-CBCO      '/'
                  4174-SAI-CAG-BCRIA '/'
                  4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE               INTO WRK-OUT-DADOSBC-QQTM.

           MOVE 4174-SAI-CPRODT            TO WRK-OUT-CODPROD-QQTM.
           MOVE 4174-SAI-IABREV-PRODT      TO WRK-OUT-DSPROD-QQTM.
           MOVE 4174-SAI-CSPROD-DESC-COML  TO WRK-OUT-SUBPROD-QQTM.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC TO WRK-OUT-DSSBPROD-QQTM.
           MOVE 4174-SAI-DANO-OPER-DESC    TO WRK-OUT-OPER1-QQTM.
           MOVE 4174-SAI-NSEQ-OPER-DESC    TO WRK-OUT-OPER2-QQTM.

           MOVE 4174-SAI-IRSUMO-SIT-DESC   TO WRK-OUT-SITUA1-QQTM.

           EVALUATE TRUE
               WHEN 4174-SAI-CCONTR-LIM-DESC      GREATER ZEROS
                    MOVE 'CONTR LIMITE:'          TO WRK-OUT-SITUA2-QQTM
                    MOVE 4174-SAI-CCONTR-LIM-DESC TO WRK-OUT-SITUA3-QQTM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 8
                    MOVE 'CONV GERAL..:'          TO WRK-OUT-SITUA2-QQTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITUA3-QQTM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 9
                    MOVE 'CONV CLIENTE:'          TO WRK-OUT-SITUA2-QQTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITUA3-QQTM
           END-EVALUATE.

           IF  4174-SAI-NSMULA-OPER-DESC   GREATER ZEROS

               IF  WRK-OUT-SITUA3-QQTM     EQUAL SPACES
                   MOVE 'SIMULACAO...:'    TO WRK-OUT-SITUA2-QQTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-SITUA3-QQTM
               ELSE
                   MOVE 'SIMULACAO...:'    TO WRK-OUT-SITUA4-QQTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-SITUA5-QQTM
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
           MOVE WRK-INP-OPER1-QQTM     TO WRK-5307E-DANO-OPER.
           MOVE WRK-INP-OPER2-QQTM     TO WRK-5307E-NSEQ-OPER.
           MOVE WRK-COD-DEPTO-N        TO WRK-5307E-AGENCIA-FUNC.
           MOVE 'E'                    TO WRK-5307E-FLAG-FUNCAO.
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



           MOVE WRK-5307S-QTDE-PARCELAS  TO WRK-OUT-QTPAR-QQTM-N.

           MOVE WRK-5307S-VPRINC-PCELA   TO WRK-OUT-VLRPAR-QQTM-N.
           MOVE WRK-5307S-VATUAL-PCELA   TO WRK-OUT-VLRATU-QQTM-N.
           MOVE WRK-5307S-VEFETV-BAIXA   TO WRK-OUT-VLRBAIX-QQTM-N.
           MOVE WRK-5307S-CTPO-BX-PCELA  TO WRK-OUT-TPBAIXA-QQTM.
           MOVE WRK-5307S-IRSUMO-TPO-BX  TO WRK-OUT-DSBAIXA-QQTM.
           MOVE WRK-5307S-CINDCD-LOC-DEB TO WRK-OUT-DEBEM-QQTM.
           MOVE WRK-5307S-CINDCD-CON-EXT TO WRK-OUT-LANCAM-QQTM.
           MOVE WRK-5307S-CINDCD-BX-COBR TO WRK-OUT-BAITIT-QQTM.

           IF  WRK-5307S-CINDCD-LOC-DEB  EQUAL 2
               MOVE SPACES               TO WRK-OUT-AGENCIA-QQTM
                                            WRK-OUT-CTADEB-QQTM
           ELSE
               MOVE WRK-5307S-CAG-BCRIA  TO WRK-OUT-AGENCIA-QQTM-N
               MOVE WRK-5307S-CCTA-BCRIA TO WRK-OUT-CTADEB-QQTM-N
           END-IF.


BSI001     IF  WRK-5307S-CMOTVO-BAIXA-PREJ GREATER ZEROS
BSI001         MOVE 'MOTIVO BAIXA......:'  TO WRK-OUT-LMOTVO-QQTM
BSI001         MOVE WRK-5307S-CMOTVO-BAIXA-PREJ
BSI001                                     TO WRK-OUT-CMOTVO-QQTM-N
BSI001         STRING '- ' WRK-5307S-RMOTVO-BAIXA-PREJ(1:13)
BSI001         DELIMITED BY SIZE         INTO WRK-OUT-RMOTVO-QQTM
BSI001     ELSE
BSI001         MOVE SPACES                 TO WRK-OUT-LMOTVO-QQTM
BSI001                                        WRK-OUT-CMOTVO-QQTM
BSI001                                        WRK-OUT-RMOTVO-QQTM
BSI001     END-IF.
BSI001
BSI001     IF  WRK-5307S-CFUNC-BDSCO-AUTRZ GREATER ZEROS
BSI001         MOVE 'COD. AUT.:'           TO WRK-OUT-LAUTRZ-QQTM
BSI001         MOVE WRK-5307S-CFUNC-BDSCO-AUTRZ
BSI001                                     TO WRK-OUT-CAUTRZ-QQTM-N
BSI001         STRING '- ' WRK-5307S-NFUNC-BDSCO-AUTRZ(1:15)
BSI001         DELIMITED BY SIZE         INTO WRK-OUT-NAUTRZ-QQTM
BSI001     ELSE
BSI001         MOVE SPACES                 TO WRK-OUT-LAUTRZ-QQTM
BSI001                                        WRK-OUT-CAUTRZ-QQTM

BSI001                                        WRK-OUT-NAUTRZ-QQTM
BSI001     END-IF.
BSI001
BSI001     MOVE WRK-5307S-RJUSTF-BX-PARC(1:59)
BSI001                                     TO WRK-OUT-JUSTIF-QQTM.
BSI001
      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMQQTM         SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-MENSAGEM           TO WRK-INPUT-QQTM.

           PERFORM 1260-DEVOLVER-TELA


           EVALUATE TRUE
             WHEN  (WRK-INP-PFK-QQTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QQTM   EQUAL 'PFK01')  OR
                    WRK-INP-PFK-QQTM       EQUAL 'H'
                    MOVE WRK-INP-MENSA-QQTM TO WRK-OUT-MENSA-QQTM

             WHEN  (WRK-INP-PFK-QQTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QQTM   EQUAL 'PFK03')  OR
                    WRK-INP-PFK-QQTM       EQUAL '3'
                    PERFORM 1210-RETORNAR-CHAMADOR

             WHEN  (WRK-INP-PFK-QQTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QQTM   EQUAL 'PFK05')  OR
                    WRK-INP-PFK-QQTM       EQUAL '5'
                    PERFORM 1220-TRATAR-PF5

             WHEN  (WRK-INP-PFK-QQTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QQTM   EQUAL 'PFK10')  OR
                    WRK-INP-PFK-QQTM       EQUAL 'A'
                    PERFORM 1230-TRATAR-PF10


             WHEN  (WRK-INP-PFK-QQTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QQTM   EQUAL 'PFK06')   OR
                    WRK-INP-PFK-QQTM       EQUAL '6'
                    PERFORM 1235-ACESSAR-DCOM5335
                    IF  WRK-INCONSIS       EQUAL SPACES
                        PERFORM 1240-TRATAR-PF6
                    END-IF

             WHEN  (WRK-INP-PFK-QQTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QQTM   EQUAL 'PFK11')  OR
                    WRK-INP-PFK-QQTM       EQUAL 'B'
                    PERFORM 1250-TRATAR-PF11

             WHEN  (WRK-INP-PFK-QQTM       EQUAL '.'       AND
                    WRK-INP-COMANDO-QQTM   EQUAL SPACES)
                    MOVE WRK-INP-MENSA-QQTM TO WRK-OUT-MENSA-QQTM

             WHEN   OTHER
                    MOVE WRK-MENSA-01       TO WRK-OUT-MENSA-QQTM
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
           MOVE WRK-INP-CHAMADOR-QQTM  TO WRK-TELA.
           MOVE 'DCOM1717'             TO WRK-COMU-TRANSACAO.
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


           MOVE WRK-INP-OP-MENU-QQTM   TO WRK-COMU-OP-MENU.
           MOVE WRK-INP-OPCAO-QQTM     TO WRK-COMU-OPCAO-N.
           MOVE WRK-INP-TPMANUT-QQTM   TO WRK-COMU-TP-SELECAO.
           MOVE WRK-INP-TIMES-QQTM     TO WRK-COMU-TIMESTAMP.
           MOVE WRK-INP-PARCELA-QQTM   TO WRK-COMU-PARCELA.
           MOVE WRK-INP-BCO-QQTM       TO WRK-COMU-BANCO.
           MOVE WRK-INP-AGENOFF-QQTM   TO WRK-COMU-AGENCIA.
           MOVE WRK-INP-CONTA-QQTM     TO WRK-COMU-CONTA.

           MOVE WRK-INP-CNPJ-QQTM      TO WRK-COMU-CCNPJ-CPF.
           MOVE WRK-INP-FILIAL-QQTM    TO WRK-COMU-CFLIAL-CNPJ.
           MOVE WRK-INP-CTRL-QQTM      TO WRK-COMU-CCTRL-CNPJCPF.
           MOVE WRK-INP-PAGINA-QQTM    TO WRK-COMU-PAGINA-1.
           MOVE WRK-INP-OPER1-QQTM     TO WRK-COMU-DANO-OPER.
           MOVE WRK-INP-OPER2-QQTM     TO WRK-COMU-NSEQ-OPER.

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
           MOVE 'DCOM1717'              TO WRK-TELA

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
           MOVE 'DCOM1717'              TO WRK-TELA
                                           WRK-COMU-TRANSACAO.
           MOVE 'A'                     TO WRK-COMU-PFK.

           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL.
           MOVE WRK-COMU-AREA           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1235-ACESSAR-DCOM5335           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5335-ENTRADA
                      5335-SAIDA.

           MOVE WRK-INP-OPERACAO-QQTM  TO 5335E-OPERACAO.
           MOVE 007                    TO 5335E-CELMTO-DESC-COML.
           MOVE 089                    TO 5335E-CEVNTO-DESC-COML.
           MOVE 'E'                    TO 5335E-ACAO.
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
                                       TO WRK-OUT-MENSA-QQTM
                     MOVE '*'          TO WRK-INCONSIS
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


           INITIALIZE WRK-5312-ENTRADA
                      WRK-5312-SAIDA
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE WRK-COD-USER-N         TO WRK-5312E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5312E-CTERM.
           MOVE WRK-COD-DEPTO-N        TO WRK-5312E-AGENCIA-FUNC.
           MOVE WRK-INP-OPER1-QQTM     TO WRK-5312E-DANO-OPER.
           MOVE WRK-INP-OPER2-QQTM     TO WRK-5312E-NSEQ-OPER.
           MOVE WRK-INP-DEBEM-QQTM     TO WRK-5312E-CINDCD-LOC-DEB-BX.


           IF  WRK-INP-DEBEM-QQTM      EQUAL '1'
               MOVE WRK-INP-AGENCIA-QQTM-N
                                       TO WRK-5312E-CAG-BCRIA-BX
               MOVE WRK-INP-CTADEB-QQTM-N
                                       TO WRK-5312E-CCTA-BCRIA-CLI-BX
           END-IF.

           MOVE 'DCOM5312'             TO WRK-MODULO.


           CALL WRK-MODULO             USING WRK-5312-ENTRADA
                                             WRK-5312-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE WRK-5312S-COD-RETORNO
               WHEN '0000'
                    PERFORM 1210-RETORNAR-CHAMADOR
                    MOVE WRK-5312S-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM

                    MOVE WRK-COMU-AREA TO WRK-MENSAGEM

               WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
BSI001              PERFORM 9998-EXECUTAR-ROLLBACK
                    MOVE WRK-5312S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-QQTM
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

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-INP-CHAMADOR-QQTM  TO WRK-COMU-CHAMADOR.
           MOVE 'DCOM1717'             TO WRK-COMU-TRANSACAO.
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


           MOVE WRK-INP-OCULTOS-QQTM   TO WRK-OUT-OCULTOS-QQTM.
           MOVE WRK-INP-NOMECLI-QQTM   TO WRK-OUT-NOMECLI-QQTM.
           MOVE WRK-INP-CHAMADOR-QQTM  TO WRK-OUT-CHAMADOR-QQTM.
           MOVE WRK-INP-CNPJCPF-QQTM   TO WRK-OUT-CNPJCPF-QQTM.
           MOVE WRK-INP-DADOSBC-QQTM   TO WRK-OUT-DADOSBC-QQTM.
           MOVE WRK-INP-CODPROD-QQTM   TO WRK-OUT-CODPROD-QQTM.
           MOVE WRK-INP-DSPROD-QQTM    TO WRK-OUT-DSPROD-QQTM.
           MOVE WRK-INP-SUBPROD-QQTM   TO WRK-OUT-SUBPROD-QQTM.
           MOVE WRK-INP-DSSBPROD-QQTM  TO WRK-OUT-DSSBPROD-QQTM.
           MOVE WRK-INP-OPERACAO-QQTM  TO WRK-OUT-OPERACAO-QQTM.
           MOVE WRK-INP-SITUA1-QQTM    TO WRK-OUT-SITUA1-QQTM.
           MOVE WRK-INP-SITUA2-QQTM    TO WRK-OUT-SITUA2-QQTM.
           MOVE WRK-INP-SITUA3-QQTM    TO WRK-OUT-SITUA3-QQTM.
           MOVE WRK-INP-SITUA4-QQTM    TO WRK-OUT-SITUA4-QQTM.
           MOVE WRK-INP-SITUA5-QQTM    TO WRK-OUT-SITUA5-QQTM.
           MOVE WRK-INP-QTPAR-QQTM     TO WRK-OUT-QTPAR-QQTM.
           MOVE WRK-INP-VLRPAR-QQTM    TO WRK-OUT-VLRPAR-QQTM.

           MOVE WRK-INP-VLRATU-QQTM    TO WRK-OUT-VLRATU-QQTM.
           MOVE WRK-INP-VLRBAIX-QQTM   TO WRK-OUT-VLRBAIX-QQTM.
           MOVE WRK-INP-TPBAIXA-QQTM   TO WRK-OUT-TPBAIXA-QQTM.
           MOVE WRK-INP-DSBAIXA-QQTM   TO WRK-OUT-DSBAIXA-QQTM.
           MOVE WRK-INP-DEBEM-QQTM     TO WRK-OUT-DEBEM-QQTM.
           MOVE WRK-INP-LANCAM-QQTM    TO WRK-OUT-LANCAM-QQTM.
           MOVE WRK-INP-AGENCIA-QQTM   TO WRK-OUT-AGENCIA-QQTM.
           MOVE WRK-INP-CTADEB-QQTM    TO WRK-OUT-CTADEB-QQTM.
           MOVE WRK-INP-BAITIT-QQTM    TO WRK-OUT-BAITIT-QQTM.
BSI001     MOVE WRK-INP-LMOTVO-QQTM    TO WRK-OUT-LMOTVO-QQTM.
BSI001     MOVE WRK-INP-CMOTVO-QQTM    TO WRK-OUT-CMOTVO-QQTM.
BSI001     MOVE WRK-INP-RMOTVO-QQTM    TO WRK-OUT-RMOTVO-QQTM.
BSI001     MOVE WRK-INP-LAUTRZ-QQTM    TO WRK-OUT-LAUTRZ-QQTM.
BSI001     MOVE WRK-INP-CAUTRZ-QQTM    TO WRK-OUT-CAUTRZ-QQTM.
BSI001     MOVE WRK-INP-NAUTRZ-QQTM    TO WRK-OUT-NAUTRZ-QQTM.
BSI001     MOVE WRK-INP-JUSTIF-QQTM    TO WRK-OUT-JUSTIF-QQTM.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      * PROCESSA BRAD660 COMPACTA DADOS PARA DCOMQQTM                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*


           MOVE LENGTH                 OF WRK-OUTPUT-QQTM
                                       TO WRK-OUT-LL-QQTM
                                          WRK-660-LL-MENSAGEM-QQTM.
           MOVE LENGTH                 OF WRK-660-DCOMQQTM
                                       TO WRK-660-LL-AREA-QQTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-QQTM
                                             WRK-660-DCOMQQTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-COD-RET
               MOVE WRK-COD-RET-N      TO WRK-RETURN-CODE

               MOVE 010                TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-QQTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM1717'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM1717'         TO ERR-PGM
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
