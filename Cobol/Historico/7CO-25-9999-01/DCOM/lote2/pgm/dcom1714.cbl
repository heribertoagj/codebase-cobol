      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1714.
       AUTHOR.     DAVI BRAZ.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  DCOM1714                                     *
      *    PROGRAMADOR.:  DAVI BRAZ                                    *
      *    DATA........:  28/01/2007                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  CONSULTA ESTORNO DA BAIXA DA PARCELA - CHEQUE*
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMQNTM - CONSULTAR ESTORNO DA BAIXA DA PARCELA - CHEQUES  *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *

      *    I#DCOMSY - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM1709           CHAMA: DCOM1701            *
      *                  DCOM1711                                      *
      *                  DCOM1713                                      *
      *================================================================*
DTS001*                   ALTERACAO - DTS CONSULTING                   *
DTS001*----------------------------------------------------------------*
DTS001*    PROGRAMADORA:  LUCIANDRA SILVEIRA - DTS CONSULTING          *
DTS001*    ANALISTA DDS:  VERA MARIA POLLINI - GRUPO 70                *
DTS001*    DATA........:  29/10/2009                                   *
DTS001*----------------------------------------------------------------*
DTS001*    OBJETIVO....:  CORRIGIR DESCRICAO DO SUBPRODUTO NO CABECALHO*
DTS001*================================================================*


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
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM1714 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*

      *--- NUMERICO - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-241                     PIC S9(008) COMP    VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS CURSOR - NUM  ---*
       01  WRK-49361                   PIC S9(008) COMP    VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS CURSOR - NUM  ---*
       01  WRK-49369                   PIC S9(008) COMP    VALUE +49369.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.
      *---  NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49345                   PIC S9(008) COMP    VALUE +49345.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE

           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-IND                 PIC  9(002)         VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.

       01  WRK-MENSAGENS.
           05  WRK-MSG-01              PIC  X(079)         VALUE
               'CONSULTA EFETUADA COM SUCESSO. TECLE <PF3> PARA RETORNAR
      -        ' '.
           05  WRK-MSG-02              PIC  X(079)         VALUE
               'PF INVALIDA'.
           05  WRK-MSG-03              PIC  X(079)         VALUE
               'AMOSTRAGEM CONTINUA'.
           05  WRK-MSG-04              PIC  X(079)         VALUE
               'FIM DE AMOSTRAGEM'.
           05  WRK-MSG-05              PIC  X(079)         VALUE
               'NAO EXISTEM MAIS PAGINAS PARA RETORNAR'.

           05  WRK-MSG-06              PIC  X(079)         VALUE
               'NAO EXISTEM MAIS PAGINAS PARA AVANCAR'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)         VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)         VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)         VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(004)         VALUE SPACES.
           05  WRK-TELA                PIC  X(008)         VALUE SPACES.
           05  WRK-VERSAO              PIC  X(006)         VALUE
               'VRS002'.
           05  WRK-COD-USER            PIC  X(007)         VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).

           05  WRK-COD-DEPTO.

               10  FILLER              PIC  X(002)         VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)         VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)         VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01 WRK-ERRO-BRAD0660.
           05 FILLER                   PIC  X(045)         VALUE
              '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05 FILLER                   PIC  X(012)         VALUE
              '- RET.COD ='.
           05 WRK-RETURN-CODE          PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(011)         VALUE
              ' - LOCAL ='.

           05 WRK-LOCAL-ERRO           PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(002)         VALUE ' *'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQNTM - INPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-INPUT-QNTM.
           05 FILLER                      PIC  X(018)      VALUE SPACES.
           05 WRK-INP-DADOS-QNTM.
             10 WRK-INP-PFK-QNTM          PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SENHAS-QNTM       PIC  X(037)      VALUE SPACES.
             10 WRK-INP-COMANDO-QNTM      PIC  X(068)      VALUE SPACES.
             10 WRK-INP-FIMAMOS-QNTM      PIC  X(001)      VALUE SPACES.
             10 WRK-INP-PAGINA-QNTM.
                15 WRK-INP-PAGINA-QNTM-N  PIC  9(003)      VALUE ZEROS.
           05 WRK-INP-DADOS-FIXOS.
             10 WRK-INP-OCULTO-QNTM       PIC  X(079)      VALUE SPACES.
             10 REDEFINES WRK-INP-OCULTO-QNTM.
                15 WRK-INP-OPCAO-QNTM     PIC  9(001).

                15 WRK-INP-TMSTAMP-QNTM   PIC  X(026).
                15 WRK-INP-PAGINA1-QNTM   PIC  9(004).
                15 WRK-INP-PAGINA2-QNTM   PIC  9(004).
                15 WRK-INP-OPMENU-QNTM    PIC  9(001).
                15 WRK-INP-TPSELEC-QNTM   PIC  9(001).
                15 WRK-INP-MODBAIX-QNTM   PIC  X(001).
                15 WRK-INP-CHAMADO-QNTM   PIC  X(008).
                15 WRK-INP-TPCHEQ-QNTM    PIC  X(001).
             10 WRK-INP-NOME-QNTM         PIC  X(040)      VALUE SPACES.
             10 WRK-INP-CNPJCPF-QNTM      PIC  X(019)      VALUE SPACES.
             10 WRK-INP-BCAGCTA-QNTM      PIC  X(023)      VALUE SPACES.
             10 WRK-INP-CDPRODT-QNTM      PIC  X(003)      VALUE SPACES.
             10 WRK-INP-DSPRODT-QNTM      PIC  X(012)      VALUE SPACES.
             10 WRK-INP-CDSPROD-QNTM      PIC  X(003)      VALUE SPACES.
             10 WRK-INP-DSUBPRD-QNTM      PIC  X(012)      VALUE SPACES.
             10 WRK-INP-CDOPERA-QNTM.
                15 WRK-INP-DANO-OPER-QNTM PIC  9(004)      VALUE ZEROS.
                15 WRK-INP-NSEQ-OPER-QNTM PIC  9(009)      VALUE ZEROS.
             10 WRK-INP-SITU1-QNTM        PIC  X(015)      VALUE SPACES.
             10 WRK-INP-SITU2-QNTM        PIC  X(013)      VALUE SPACES.
             10 WRK-INP-SITU3-QNTM        PIC  X(009)      VALUE SPACES.

             10 WRK-INP-SITU4-QNTM        PIC  X(013)      VALUE SPACES.
             10 WRK-INP-SITU5-QNTM        PIC  X(009)      VALUE SPACES.
             10 WRK-INP-PARCELA-QNTM.
               15 WRK-INP-PARCELA-QNTM-N  PIC  9(005)      VALUE ZEROS.
             10 WRK-INP-VENCTO-QNTM       PIC  X(010)      VALUE SPACES.
           05 WRK-INP-DADOS-VAR.
             10 WRK-INP-TABELA-QNTM       OCCURS 009  TIMES.
               15 WRK-INP-BCO-QNTM        PIC  X(003)      VALUE SPACES.
               15 WRK-INP-AGENC-QNTM      PIC  X(005)      VALUE SPACES.
               15 WRK-INP-CONTA-QNTM      PIC  X(013)      VALUE SPACES.
               15 WRK-INP-NRCHQ-QNTM      PIC  X(011)      VALUE SPACES.
               15 WRK-INP-VLCHQ-QNTM      PIC  X(014)      VALUE SPACES.
               15 WRK-INP-VLBAI-QNTM      PIC  X(014)      VALUE SPACES.
             10 WRK-INP-MENSA-QNTM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQNTM - OUTPUT ***'.
      *---------------------------------------------------------------*

       01 WRK-OUTPUT-QNTM.

          05 WRK-OUT-LL-QNTM              PIC S9(004) COMP VALUE ZEROS.
          05 WRK-OUT-ZZ-QNTM              PIC  9(004) COMP VALUE ZEROS.
          05 WRK-OUT-DADOS-QNTM.
             10 WRK-OUT-SENHAS-QNTM       PIC  X(037)      VALUE SPACES.
             10 WRK-OUT-COMANDO-QNTM      PIC  X(068)      VALUE SPACES.
             10 WRK-OUT-FIMAMOS-QNTM      PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-PAGINA-QNTM.
                15 WRK-OUT-PAGINA-QNTM-N  PIC  9(003)      VALUE ZEROS.
          05 WRK-OUT-DADOS-FIXOS.
             10 WRK-OUT-OCULTO-QNTM       PIC  X(079)      VALUE SPACES.
             10 REDEFINES                 WRK-OUT-OCULTO-QNTM.
                15 WRK-OUT-OPCAO-QNTM     PIC  9(001).
                15 WRK-OUT-TMSTAMP-QNTM   PIC  X(026).
                15 WRK-OUT-PAGINA1-QNTM   PIC  9(004).
                15 WRK-OUT-PAGINA2-QNTM   PIC  9(004).
                15 WRK-OUT-OPMENU-QNTM    PIC  9(001).
                15 WRK-OUT-TPSELEC-QNTM   PIC  9(001).
                15 WRK-OUT-MODBAIX-QNTM   PIC  X(001).
                15 WRK-OUT-CHAMADO-QNTM   PIC  X(008).
                15 WRK-OUT-TPCHEQ-QNTM    PIC  X(001).
             10 WRK-OUT-NOME-QNTM         PIC  X(040)      VALUE SPACES.

             10 WRK-OUT-CNPJCPF-QNTM      PIC  X(019)      VALUE SPACES.
             10 WRK-OUT-BCAGCTA-QNTM      PIC  X(023)      VALUE SPACES.
             10 WRK-OUT-CDPRODT-QNTM      PIC  X(003)      VALUE SPACES.
             10 WRK-OUT-DSPRODT-QNTM      PIC  X(012)      VALUE SPACES.
             10 WRK-OUT-CDSPROD-QNTM      PIC  X(003)      VALUE SPACES.
             10 WRK-OUT-DSUBPRD-QNTM      PIC  X(012)      VALUE SPACES.
             10 WRK-OUT-CDOPERA-QNTM      PIC  X(013)      VALUE SPACES.
             10 WRK-OUT-SITU1-QNTM        PIC  X(015)      VALUE SPACES.
             10 WRK-OUT-SITU2-QNTM        PIC  X(013)      VALUE SPACES.
             10 WRK-OUT-SITU3-QNTM        PIC  X(009)      VALUE SPACES.
             10 WRK-OUT-SITU4-QNTM        PIC  X(013)      VALUE SPACES.
             10 WRK-OUT-SITU5-QNTM        PIC  X(009)      VALUE SPACES.
             10 WRK-OUT-PARCELA-QNTM      PIC  99999       VALUE ZEROS.
             10 WRK-OUT-VENCTO-QNTM       PIC  X(010)      VALUE SPACES.
           05 WRK-OUT-DADOS-VAR.
             10 WRK-OUT-TABELA-QNTM       OCCURS 009  TIMES.
               15 WRK-OUT-BCO-QNTM        PIC  9(003)      VALUE ZEROS.
               15 WRK-OUT-AGENC-QNTM      PIC  9(005)      VALUE ZEROS.
               15 WRK-OUT-CONTA-QNTM      PIC  9(013)      VALUE ZEROS.
               15 WRK-OUT-NRCHQ-QNTM      PIC  X(011)      VALUE SPACES.
               15 WRK-OUT-VLCHQ-QNTM.

                  20 WRK-OUT-VLCHQ-QNTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
               15 WRK-OUT-VLBAI-QNTM.
                  20 WRK-OUT-VLBAI-QNTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-MENSA-QNTM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                         PIC X(050)       VALUE
           '*** TELA DCOMQNTM - BRAD0660 ***'.
      *---------------------------------------------------------------*

       01  WRK-660-DCOMQNTM.
           05 WRK-660-LL-AREA-QNTM     PIC  9(004) COMP    VALUE 0158.
           05 WRK-660-LL-MENSAGEM-QNTM PIC  9(004) COMP    VALUE 1014.
           05 WRK-660-SENHAS-QNTM      PIC  9(004) COMP    VALUE 0037.
           05 WRK-660-COMANDO-QNTM     PIC  9(004) COMP    VALUE 0068.
           05 WRK-660-FIMAMOS-QNTM     PIC  9(004) COMP    VALUE 0001.
           05 WRK-660-PAGINA-QNTM      PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-OCULTO-QNTM      PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-NOME-QNTM        PIC  9(004) COMP    VALUE 0040.

           05 WRK-660-CNPJCPF-QNTM     PIC  9(004) COMP    VALUE 0019.
           05 WRK-660-BCAGCTA-QNTM     PIC  9(004) COMP    VALUE 0023.
           05 WRK-660-CDPRODT-QNTM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-DSPRODT-QNTM     PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-CDSPROD-QNTM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-DSUBPRD-QNTM     PIC  9(004) COMP   VALUE 0012.
           05 WRK-660-CDOPERA-QNTM     PIC  9(004) COMP   VALUE 0013.
           05 WRK-660-SITU1-QNTM       PIC  9(004) COMP   VALUE 0015.
           05 WRK-660-SITU2-QNTM       PIC  9(004) COMP   VALUE 0013.
           05 WRK-660-SITU3-QNTM       PIC  9(004) COMP   VALUE 0009.
           05 WRK-660-SITU4-QNTM       PIC  9(004) COMP   VALUE 0013.
           05 WRK-660-SITU5-QNTM       PIC  9(004) COMP   VALUE 0009.
           05 WRK-660-PARCELA-QNTM     PIC  9(004) COMP   VALUE 0005.
           05 WRK-660-VENCTO-QNTM      PIC  9(004) COMP   VALUE 0010.
           05 WRK-660-OCORRENCIA-QNTM  OCCURS 009 TIMES.
              10 WRK-660-BCO-QNTM      PIC  9(004) COMP   VALUE 0003.
              10 WRK-660-AGENC-QNTM    PIC  9(004) COMP   VALUE 0005.
              10 WRK-660-CONTA-QNTM    PIC  9(004) COMP   VALUE 0013.
              10 WRK-660-NRCHQ-QNTM    PIC  9(004) COMP   VALUE 0011.
              10 WRK-660-VLCHQ-QNTM    PIC  9(004) COMP   VALUE 0014.
              10 WRK-660-VLBAI-QNTM    PIC  9(004) COMP   VALUE 0014.

           05 WRK-660-MENSA-QNTM       PIC  9(004) COMP   VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO ENTRE PRG IMS ***'.
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
4S2511       10 WRK-COMU-CCNPJ-CPF     PIC  X(09)          VALUE SPACES.
4S2511       10 WRK-COMU-CFLIAL-CNPJ   PIC  X(04)          VALUE SPACES.     
             10 WRK-COMU-CCTRL-CNPJCPF PIC  9(002)         VALUE ZEROS.
             10 WRK-COMU-PAGINA-1      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-PAGINA-2      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-OP-MENU       PIC  9(001)         VALUE ZEROS.
             10 WRK-COMU-TP-SELECAO    PIC  9(001)         VALUE ZEROS.
             10 WRK-COMU-MODAL-BAIXA   PIC  X(001)         VALUE SPACES.
             10 WRK-COMU-MENSAGEM      PIC  X(079)         VALUE SPACES.
             10 WRK-COMU-VENCIMENTO    PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE

           '*** AREA DE COMUNICACAO DCOM5480 ***'.
      *----------------------------------------------------------------*

       01  WRK-5480-ENTRADA.
           05 WRK-5480E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5480E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5480E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5480E-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5480E-START-COUNT    PIC  X(001)         VALUE SPACES.
           05 WRK-5480E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5480E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5480E-AGENCIA-FUNC   PIC  9(005)         VALUE ZEROS.
           05 WRK-5480E-CPRODT         PIC  9(003)         VALUE ZEROS.
           05 WRK-5480E-DANO-OPER-DESC PIC  9(004)         VALUE ZEROS.
           05 WRK-5480E-NSEQ-OPER-DESC PIC  9(009)         VALUE ZEROS.
           05 WRK-5480E-NPCELA-DESC-COML
                                       PIC  9(005)         VALUE ZEROS.
           05 WRK-5480E-FLAG-FUNCAO    PIC  X(001)         VALUE SPACES.

       01  WRK-5480-SAIDA.
           05 WRK-5480S-COD-RETORNO    PIC  X(004)         VALUE SPACES.

           05 WRK-5480S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5480S-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5480S-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5480S-QTDE-RETORNADA PIC  9(003)         VALUE ZEROS.
           05 WRK-5480S-COUNT          PIC  9(009)         VALUE ZEROS.
           05 WRK-5480S-TABELA-SAIDA   OCCURS 070 TIMES.
             10 WRK-5480S-CBCO-CHEQC   PIC  9(003)         VALUE ZEROS.
             10 WRK-5480S-CAG-BCRIA-CHEQC
                                       PIC  9(005)         VALUE ZEROS.
             10 WRK-5480S-CCCORR-CHEQC PIC  9(012)         VALUE ZEROS.
             10 WRK-5480S-CCHEQ-CHEQC  PIC  9(011)         VALUE ZEROS.
             10 WRK-5480S-VCHEQC       PIC  9(015)V99      VALUE ZEROS.
             10 WRK-5480S-VEFETV-BAIXA-PCELA
                                       PIC  9(015)V99      VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSG'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
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
           '*** DCOM1714 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01 LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*


           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL  04 OR 12

               PERFORM 2000-FINALIZAR
           END-IF.

           INITIALIZE 4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR
                      WRK-5480-ENTRADA
                      WRK-5480-SAIDA
                      WRK-COMU-MENSAGEM
                      WRK-SQLCA
                      ERRO-AREA.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROGRAMA A MENSAGEM                               *

      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMQNTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL  'DCOM1714'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMQNTM
           END-IF.

           IF  WRK-TELA                EQUAL  'DCOMQNTM'
               PERFORM 1400-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

      *  ESSA ROTINA RECEBE O CONTROLE DE OUTRAS TRANSACOES            *
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.

           MOVE WRK-COMU-SENHAS        TO WRK-INP-SENHAS-QNTM
                                          WRK-OUT-SENHAS-QNTM.
           MOVE WRK-COMU-OPCAO-N       TO WRK-INP-OPCAO-QNTM
                                          WRK-OUT-OPCAO-QNTM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TMSTAMP-QNTM
                                          WRK-OUT-TMSTAMP-QNTM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGINA1-QNTM
                                          WRK-OUT-PAGINA1-QNTM.
           MOVE WRK-COMU-PAGINA-2      TO WRK-INP-PAGINA2-QNTM
                                          WRK-OUT-PAGINA2-QNTM.
           MOVE WRK-COMU-OP-MENU       TO WRK-INP-OPMENU-QNTM
                                          WRK-OUT-OPMENU-QNTM.
           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPSELEC-QNTM
                                          WRK-OUT-TPSELEC-QNTM.

           MOVE WRK-COMU-MODAL-BAIXA   TO WRK-INP-MODBAIX-QNTM
                                          WRK-OUT-MODBAIX-QNTM.
           MOVE WRK-COMU-OPERACAO      TO WRK-INP-CDOPERA-QNTM
                                          WRK-OUT-CDOPERA-QNTM.
           MOVE WRK-COMU-PARCELA       TO WRK-INP-PARCELA-QNTM
                                          WRK-OUT-PARCELA-QNTM.
           MOVE WRK-COMU-VENCIMENTO    TO WRK-OUT-VENCTO-QNTM.
           MOVE WRK-TRANSACAO          TO WRK-INP-CHAMADO-QNTM
                                          WRK-OUT-CHAMADO-QNTM.

           PERFORM 1110-ACESSAR-DCOM4174.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 1100-99-FIM
           END-IF.

           PERFORM 1120-ACESSAR-DCOM5480.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 1100-99-FIM
           END-IF.


           MOVE WRK-MSG-01             TO WRK-OUT-MENSA-QNTM.

           MOVE 1                      TO WRK-OUT-PAGINA-QNTM-N.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-ACESSAR-DCOM4174           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-DANO-OPER-QNTM TO 4174-ENT-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ-OPER-QNTM TO 4174-ENT-NSEQ-OPER-DESC.
           MOVE 'N'                    TO 4174-ENT-FLAG.
           MOVE WRK-COD-USER-R         TO 4174-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM.
           MOVE 'DCOM4174'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR

                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO
             WHEN '0000'
                  PERFORM 1111-FORMATAR-CABECALHO
             WHEN '0099'
                  MOVE 'DB2'           TO ERR-TIPO-ACESSO
                  PERFORM 9999-PROCESSAR-ROTINA-ERRO
             WHEN OTHER
                  MOVE 4174-SAI-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                  MOVE '*'             TO WRK-INCONSIS
                  PERFORM 1220-RETORNAR-CHAMADOR
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *  ESSA ROTINA FORMATA O CABECALHO DA TELA.                      *
      *----------------------------------------------------------------*
       1111-FORMATAR-CABECALHO         SECTION.
      *----------------------------------------------------------------*
           IF  4174-SAI-CMEIO-ENTRD-DESC  EQUAL 6
               MOVE 'N'                   TO  WRK-INP-TPCHEQ-QNTM
                                              WRK-OUT-TPCHEQ-QNTM
           ELSE
               IF  4174-SAI-CMEIO-ENTRD-DESC  EQUAL 7
                    MOVE 'C'              TO  WRK-INP-TPCHEQ-QNTM
                                              WRK-OUT-TPCHEQ-QNTM
               END-IF
           END-IF

           MOVE 4174-SAI-IPSSOA-DESC-COML
                                       TO WRK-OUT-NOME-QNTM
           STRING 4174-SAI-CCNPJ-CPF(1:3)
           '.'    4174-SAI-CCNPJ-CPF(4:3)
           '.'    4174-SAI-CCNPJ-CPF(7:3)
           '/'    4174-SAI-CFLIAL-CNPJ

           '-'    4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE         INTO WRK-OUT-CNPJCPF-QNTM
           MOVE WRK-OUT-CNPJCPF-QNTM   TO WRK-INP-CNPJCPF-QNTM
           STRING 4174-SAI-CBCO
           '/'    4174-SAI-CAG-BCRIA
           '/'    4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE         INTO WRK-OUT-BCAGCTA-QNTM
           MOVE WRK-OUT-BCAGCTA-QNTM   TO WRK-INP-BCAGCTA-QNTM


           MOVE 4174-SAI-CPRODT        TO WRK-INP-CDPRODT-QNTM
                                          WRK-OUT-CDPRODT-QNTM
           MOVE 4174-SAI-IABREV-PRODT  TO WRK-OUT-DSPRODT-QNTM
           MOVE 4174-SAI-CSPROD-DESC-COML
                                       TO WRK-OUT-CDSPROD-QNTM
DTS001     MOVE 4174-SAI-IRSUMO-SPROD-DESC
                                       TO WRK-OUT-DSUBPRD-QNTM
           MOVE SPACES                 TO  WRK-OUT-SITU3-QNTM
                                           WRK-OUT-SITU5-QNTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC
                                       TO WRK-OUT-SITU1-QNTM

           IF   4174-SAI-CELMTO-DESC-COML EQUAL 08
                MOVE 'CONV. GERAL.:'   TO WRK-OUT-SITU2-QNTM
                MOVE 4174-SAI-CCONTR-CONVE-DESC
                                       TO WRK-OUT-SITU3-QNTM
           END-IF

           IF   4174-SAI-CELMTO-DESC-COML EQUAL 09
                MOVE 'CONV.CLIENTE:'   TO WRK-OUT-SITU2-QNTM
                MOVE 4174-SAI-CCONTR-CONVE-DESC
                                       TO WRK-OUT-SITU3-QNTM
           END-IF

           IF   4174-SAI-CELMTO-DESC-COML EQUAL 10
                MOVE 'CONTR.LIMITE:'   TO WRK-OUT-SITU2-QNTM
                MOVE 4174-SAI-CCONTR-LIM-DESC
                                       TO WRK-OUT-SITU3-QNTM
           END-IF

           IF  4174-SAI-NSMULA-OPER-DESC   GREATER  ZEROS
               IF  WRK-OUT-SITU2-QNTM      EQUAL SPACES OR LOW-VALUES
                   MOVE 'SIMULACAO...:'TO WRK-OUT-SITU2-QNTM

                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                       TO WRK-OUT-SITU3-QNTM
               ELSE
                   MOVE 'SIMULACAO...:'TO WRK-OUT-SITU4-QNTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                       TO WRK-OUT-SITU5-QNTM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM5480           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER-R         TO WRK-5480E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-5480E-CTERM
           MOVE WRK-COD-DEPTO-N        TO WRK-5480E-AGENCIA-FUNC.
           MOVE WRK-INP-CDPRODT-QNTM   TO WRK-5480E-CPRODT.

           MOVE WRK-INP-DANO-OPER-QNTM TO WRK-5480E-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ-OPER-QNTM TO WRK-5480E-NSEQ-OPER-DESC.
           MOVE WRK-INP-PARCELA-QNTM-N TO WRK-5480E-NPCELA-DESC-COML.
           MOVE 'DCOM5480'             TO WRK-MODULO

           EVALUATE WRK-INP-OPCAO-QNTM
               WHEN 1
                    MOVE 'I'           TO WRK-5480E-FLAG-FUNCAO
               WHEN 2
                    MOVE 'E'           TO WRK-5480E-FLAG-FUNCAO
               WHEN 3
                    MOVE 'C'           TO WRK-5480E-FLAG-FUNCAO
           END-EVALUATE.

           CALL WRK-MODULO             USING WRK-5480-ENTRADA
                                             WRK-5480-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-5480S-COD-RETORNO
               WHEN '0000'

                   PERFORM 1121-MONTAR-TELA
                   VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 9

                   IF  WRK-5480S-CBCO-CHEQC(10) EQUAL ZEROS
                       MOVE '*'        TO WRK-OUT-FIMAMOS-QNTM
                   ELSE
                       MOVE SPACE      TO WRK-OUT-FIMAMOS-QNTM
                   END-IF

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE '*'            TO WRK-INCONSIS
                   IF  WRK-INP-PAGINA-QNTM-N EQUAL 1
                   OR  WRK-TRANSACAO   NOT EQUAL 'DCOM1714'
                       MOVE WRK-5480S-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                       PERFORM 1220-RETORNAR-CHAMADOR

                   ELSE
                       MOVE WRK-5480S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-QNTM
                   END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1121-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF  WRK-5480S-CBCO-CHEQC(WRK-IND) EQUAL ZEROS
               MOVE SPACES             TO WRK-OUT-TABELA-QNTM (WRK-IND)
           ELSE
               MOVE WRK-5480S-CBCO-CHEQC        (WRK-IND)
                                       TO WRK-OUT-BCO-QNTM    (WRK-IND)
               MOVE WRK-5480S-CAG-BCRIA-CHEQC   (WRK-IND)
                                       TO WRK-OUT-AGENC-QNTM  (WRK-IND)

               MOVE WRK-5480S-CCCORR-CHEQC      (WRK-IND)
                                       TO WRK-OUT-CONTA-QNTM  (WRK-IND)
               MOVE WRK-5480S-CCHEQ-CHEQC       (WRK-IND)
                                       TO WRK-OUT-NRCHQ-QNTM  (WRK-IND)
               MOVE WRK-5480S-VCHEQC            (WRK-IND)
                                       TO WRK-OUT-VLCHQ-QNTM-N(WRK-IND)
               MOVE WRK-5480S-VEFETV-BAIXA-PCELA(WRK-IND)
                                       TO WRK-OUT-VLBAI-QNTM-N(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMQNTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMQNTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-QNTM.


           PERFORM 1210-DEVOLVER-TELA

           EVALUATE TRUE

               WHEN WRK-INP-PFK-QNTM     EQUAL 'H' OR
                   (WRK-INP-PFK-QNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QNTM EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-QNTM TO WRK-OUT-MENSA-QNTM

               WHEN WRK-INP-PFK-QNTM     EQUAL '3' OR
                   (WRK-INP-PFK-QNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QNTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-QNTM     EQUAL 'X' OR
                   (WRK-INP-PFK-QNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QNTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-QNTM     EQUAL 'Z' OR

                   (WRK-INP-PFK-QNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QNTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-QNTM     EQUAL '7' OR
                   (WRK-INP-PFK-QNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QNTM EQUAL 'PFK7')
                    PERFORM 1270-TRATAR-PFK07

               WHEN WRK-INP-PFK-QNTM     EQUAL '8' OR
                   (WRK-INP-PFK-QNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QNTM EQUAL 'PFK8')
                    PERFORM 1280-TRATAR-PFK08

               WHEN WRK-INP-PFK-QNTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QNTM EQUAL SPACES
                    MOVE WRK-MSG-01      TO WRK-OUT-MENSA-QNTM

               WHEN OTHER
                    MOVE WRK-MSG-02      TO WRK-OUT-MENSA-QNTM


           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-QNTM    TO WRK-OUT-SENHAS-QNTM.
           MOVE WRK-INP-FIMAMOS-QNTM   TO WRK-OUT-FIMAMOS-QNTM.
           MOVE WRK-INP-PAGINA-QNTM    TO WRK-OUT-PAGINA-QNTM.
           MOVE WRK-INP-DADOS-FIXOS    TO WRK-OUT-DADOS-FIXOS.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND       GREATER 9
               IF  WRK-INP-BCO-QNTM(WRK-IND) NOT EQUAL SPACES
                   MOVE WRK-INP-BCO-QNTM(WRK-IND)
                                       TO WRK-OUT-BCO-QNTM(WRK-IND)
                   MOVE WRK-INP-AGENC-QNTM(WRK-IND)

                                       TO WRK-OUT-AGENC-QNTM(WRK-IND)
                   MOVE WRK-INP-CONTA-QNTM(WRK-IND)
                                       TO WRK-OUT-CONTA-QNTM(WRK-IND)
                   MOVE WRK-INP-NRCHQ-QNTM(WRK-IND)
                                       TO WRK-OUT-NRCHQ-QNTM(WRK-IND)
                   MOVE WRK-INP-VLCHQ-QNTM(WRK-IND)
                                       TO WRK-OUT-VLCHQ-QNTM(WRK-IND)
                   MOVE WRK-INP-VLBAI-QNTM(WRK-IND)
                                       TO WRK-OUT-VLBAI-QNTM(WRK-IND)
               ELSE
                   MOVE SPACES         TO WRK-OUT-TABELA-QNTM(WRK-IND)
               END-IF
           END-PERFORM.

           MOVE WRK-INP-MENSA-QNTM     TO WRK-OUT-MENSA-QNTM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-COMU-AREA  TO WRK-COMU-LL.

           MOVE WRK-INP-CHAMADO-QNTM   TO WRK-TELA
           MOVE WRK-INP-TMSTAMP-QNTM   TO WRK-COMU-TIMESTAMP
           MOVE WRK-INP-PAGINA1-QNTM   TO WRK-COMU-PAGINA-1
           MOVE WRK-INP-PAGINA2-QNTM   TO WRK-COMU-PAGINA-2
           MOVE WRK-INP-OPCAO-QNTM     TO WRK-COMU-OPCAO-N
           MOVE WRK-INP-OPMENU-QNTM    TO WRK-COMU-OP-MENU
           MOVE WRK-INP-TPSELEC-QNTM   TO WRK-COMU-TP-SELECAO
           MOVE WRK-INP-MODBAIX-QNTM   TO WRK-COMU-MODAL-BAIXA
           MOVE WRK-INP-CDOPERA-QNTM   TO WRK-COMU-OPERACAO.
           MOVE WRK-INP-PARCELA-QNTM-N TO WRK-COMU-PARCELA.
           MOVE WRK-INP-PFK-QNTM       TO WRK-COMU-PFK
           MOVE 'DCOM1714'             TO WRK-COMU-TRANSACAO
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*

       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL
           MOVE WRK-CHNG                TO WRK-FUNCAO.
           MOVE 'DCOM1714'              TO WRK-TELA
                                           WRK-COMU-TRANSACAO.
           MOVE '5'                     TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL
           MOVE WRK-CHNG                TO WRK-FUNCAO.
           MOVE 'DCOM1714'              TO WRK-TELA
                                           WRK-COMU-TRANSACAO.
           MOVE 'A'                     TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK07              *
      *----------------------------------------------------------------*
       1270-TRATAR-PFK07               SECTION.
      *----------------------------------------------------------------*


           IF  WRK-INP-PAGINA-QNTM-N   EQUAL  1
               MOVE WRK-MSG-05         TO WRK-OUT-MENSA-QNTM
               GO TO 1270-99-FIM
           END-IF.

           COMPUTE WRK-5480E-RESTART = ( WRK-INP-PAGINA-QNTM-N - 2 ) * 9

           PERFORM 1120-ACESSAR-DCOM5480.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 1270-99-FIM
           END-IF.

           MOVE WRK-MSG-03             TO WRK-OUT-MENSA-QNTM.

           SUBTRACT 1                  FROM WRK-OUT-PAGINA-QNTM-N.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK08              *
      *----------------------------------------------------------------*
       1280-TRATAR-PFK08               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-FIMAMOS-QNTM    EQUAL  '*'
               MOVE WRK-MSG-06         TO WRK-OUT-MENSA-QNTM
               GO TO 1280-99-FIM
           END-IF.

           COMPUTE WRK-5480E-RESTART = ( WRK-INP-PAGINA-QNTM-N ) * 9.

           PERFORM 1120-ACESSAR-DCOM5480.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 1280-99-FIM
           END-IF.

           IF  WRK-OUT-FIMAMOS-QNTM    EQUAL '*'

               MOVE WRK-MSG-04         TO WRK-OUT-MENSA-QNTM
           ELSE
               MOVE WRK-MSG-03         TO WRK-OUT-MENSA-QNTM
           END-IF.

           ADD 1                       TO WRK-OUT-PAGINA-QNTM-N.

      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1400-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT-QNTM
                                       TO WRK-OUT-LL-QNTM
                                          WRK-660-LL-MENSAGEM-QNTM.
           MOVE LENGTH                 OF WRK-660-DCOMQNTM

                                       TO WRK-660-LL-AREA-QNTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-QNTM
                                             WRK-660-DCOMQNTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-QNTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

      *  ESSA ROTINA FINALIZA O PROCESSAMENTO DO PROGRAMA              *
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FORMATA E EMITE A MENSAGEM DE ERRO DO PROGRAMA    *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES AND LOW-VALUES
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM1714'         TO ERR-MODULO
           ELSE

               MOVE 'DCOM1714'         TO ERR-PGM
           END-IF.


           MOVE WRK-COD-USER           TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.


           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PCB

                                             ERRO-AREA
                                             LNK-ALT-PCB
           END-IF.



           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

