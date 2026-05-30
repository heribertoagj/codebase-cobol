      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1730.
       AUTHOR.     DTS CONSULTING.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM1730                                    *
      *    PROGRAMADOR.:   DTS CONSULTING                              *
      *    ANALISTA....:   LUCIANDRA SILVEIRA                          *
      *    DATA........:   19/07/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUIR/EXCLUIR/CONSULTAR DETALHE DE BAIXA  *
      *                    DE PARCELA POR PARCELA ESPECIFICA.          *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMQ3TM - INCLUIR/EXCLUIR/CONSULTAR DETALHE DE BAIXA       *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA. *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO.    *
      *    DCOM4174 - OBTEM DADOS GERAIS DA OPERACAO.                  *
      *    DCOM5440 - OBTER DETALHES DA BAIXA.                         *
      *    DCOM5408 - INCLUIR ESTORNO DE BAIXA.                        *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC.     *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM1708 DCOM1709     CHAMA : ---             *
      *                  DCOM1710 DCOM1711                             *
      *                  DCOM1712 DCOM1713                             *
      *================================================================*
BSI001*                     BSI TECNOLOGIA - ALTERACAO                 *
BSI001*----------------------------------------------------------------*
BSI001*    PROGRAMADOR.:    LUCIANDRA SILVEIRA - BSI TECNOLOGIA        *
BSI001*    ANALISTA BSI:    LUCIANDRA SILVEIRA - BSI TECNOLOGIA        *
BSI001*    ANALISTA DDS:    RICARDO JAMMAL     - GRUPO 39              *
BSI001*    DATA........:    08/08/2016                                 *
BSI001*----------------------------------------------------------------*
BSI001*    OBJETIVO....:    INCLUIR TRATAMENTO PARA BAIXA POR PREJUIZO *
BSI001*                     BSI - 16-0116 - BAIXA AMIGAVEL E JUDICIAL  *
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
           '*** DCOM1730 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGENS ***'.
      *----------------------------------------------------------------*

       77  WRK-MSG001                  PIC  X(079)         VALUE
           'TECLE <PF6> PARA INCLUIR O ESTORNO DA BAIXA OU <PF3> PARA RE
      -    'TORNAR'.
       77  WRK-MSG002                  PIC  X(079)         VALUE
           'TECLE <PF6> PARA EXCLUIR O ESTORNO DA BAIXA OU <PF3> PARA RE
      -    'TORNAR'.
       77  WRK-MSG003                  PIC  X(079)         VALUE
           'CONSULTA EFETUADA COM SUCESSO. TECLE <PF3> PARA RETORNAR'.
       77  WRK-MSG004                  PIC  X(079)         VALUE
           'PF INVALIDA'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  IND-1                   PIC  9(003) COMP-3  VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.

           05  WRK-CONTA-DIGITO.
               10 WRK-CONTA            PIC  9(012)         VALUE ZEROS.
               10 FILLER               PIC  X(001)         VALUE '-'.
               10 WRK-DIGITO           PIC  9(001)         VALUE ZEROS.

      *---------------------------------------------------------------*
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMQ3TM  - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-Q3TM.
           05 FILLER                     PIC  X(018)       VALUE SPACES.
           05 WRK-INP-PFK-Q3TM           PIC  X(001)       VALUE SPACES.
           05 WRK-INP-DADOS-Q3TM.
             10 WRK-INP-SENHAS-Q3TM      PIC  X(037)       VALUE SPACES.
             10 WRK-INP-COMANDO-Q3TM     PIC  X(068)       VALUE SPACES.
             10 WRK-INP-TITULO-Q3TM      PIC  X(009)       VALUE SPACES.
             10 WRK-INP-OCULTOS-Q3TM.
               15 WRK-INP-OPCAO-Q3TM     PIC  9(001)       VALUE ZEROS.
               15 WRK-INP-TPSELECAO-Q3TM PIC  9(001)       VALUE ZEROS.
               15 WRK-INP-TMSTAMP-Q3TM   PIC  X(026)       VALUE SPACES.
               15 WRK-INP-PAGINA1-Q3TM   PIC  9(004)       VALUE ZEROS.
               15 WRK-INP-PAGINA2-Q3TM   PIC  9(004)       VALUE ZEROS.
               15 WRK-INP-PAGINA3-Q3TM   PIC  9(004)       VALUE ZEROS.
               15 WRK-INP-OPMENU-Q3TM    PIC  9(001)       VALUE ZEROS.
               15 WRK-INP-TPSELEC-Q3TM   PIC  9(001)       VALUE ZEROS.
               15 WRK-INP-MODBAIX-Q3TM   PIC  X(001)       VALUE SPACES.
               15 WRK-INP-CHAMADOR-Q3TM  PIC  X(008)       VALUE SPACES.
4S2511*        15 WRK-INP-NCNPJOCUL-Q3TM PIC  9(009)       VALUE ZEROS.
4S2511         15 WRK-INP-NCNPJOCUL-Q3TM PIC  X(09)       VALUE SPACES.
               15 FILLER                 PIC  X(019)       VALUE SPACES.
             10 WRK-INP-NOMECLI-Q3TM     PIC  X(040)       VALUE SPACES.
             10 WRK-INP-CNPJCPF-Q3TM.
4S2511*        15 WRK-INP-NCNPJCPF-Q3TM  PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.
4S2511         15 WRK-INP-NCNPJCPF-Q3TM  PIC  X(11)        VALUE SPACES.
               15 FILLER                 PIC  X(001)       VALUE SPACES.
4S2511*        15 WRK-INP-FILIAL-Q3TM    PIC  9(004)       VALUE ZEROS.
4S2511         15 WRK-INP-FILIAL-Q3TM    PIC  X(04)       VALUE SPACES.
               15 FILLER                 PIC  X(001)       VALUE SPACES.
               15 WRK-INP-CONTROLE-Q3TM  PIC  9(002)       VALUE ZEROS.
             10 WRK-INP-BCAGCTA-Q3TM.
               15 WRK-INP-BANCO-Q3TM     PIC  9(003)       VALUE ZEROS.
               15 WRK-INP-BARRA1-Q3TM    PIC  X(001)       VALUE SPACES.
               15 WRK-INP-AGENCIA-Q3TM   PIC  9(005)       VALUE ZEROS.
               15 WRK-INP-BARRA2-Q3TM    PIC  X(001)       VALUE SPACES.
               15 WRK-INP-CONTA-Q3TM     PIC  9(013)       VALUE ZEROS.
             10 WRK-INP-CDPRODT-Q3TM     PIC  9(003)       VALUE ZEROS.
             10 WRK-INP-DSPRODT-Q3TM     PIC  X(012)       VALUE SPACES.
             10 WRK-INP-CDSPROD-Q3TM     PIC  9(003)       VALUE ZEROS.
             10 WRK-INP-DSUBPRD-Q3TM     PIC  X(012)       VALUE SPACES.
             10 WRK-INP-OPERACAO-Q3TM    PIC  9(013)       VALUE ZEROS.
             10 FILLER REDEFINES         WRK-INP-OPERACAO-Q3TM.
               15 WRK-INP-DANO-Q3TM      PIC  9(004).
               15 WRK-INP-NSEQ-Q3TM      PIC  9(009).
             10 WRK-INP-SITU1-Q3TM       PIC  X(015)       VALUE SPACES.
             10 WRK-INP-SITU2-Q3TM       PIC  X(013)       VALUE SPACES.
             10 WRK-INP-SITU3-Q3TM       PIC  X(009)       VALUE SPACES.
             10 WRK-INP-SITU4-Q3TM       PIC  X(013)       VALUE SPACES.
             10 WRK-INP-SITU5-Q3TM       PIC  X(009)       VALUE SPACES.
             10 WRK-INP-PARCELA-Q3TM     PIC  9(005)       VALUE ZEROS.
             10 WRK-INP-DTVCTO-Q3TM      PIC  X(010)       VALUE SPACES.
             10 WRK-INP-VLBAIXA-Q3TM     PIC  X(018)       VALUE SPACES.
             10 WRK-INP-DEBITO-Q3TM      PIC  X(001)       VALUE SPACES.
             10 WRK-INP-LANCTO-Q3TM      PIC  X(001)       VALUE SPACES.
             10 WRK-INP-AGEDEB-Q3TM.
               15 WRK-INP-AGEDEB-Q3TM-N  PIC  9(005)       VALUE ZEROS.
             10 WRK-INP-CTADEB-Q3TM      PIC  X(014)       VALUE SPACES.
             10 WRK-INP-CTPOBX-Q3TM      PIC  X(001)       VALUE SPACES.
             10 WRK-INP-DTPOBX-Q3TM      PIC  X(015)       VALUE SPACES.
BSI001       10 WRK-INP-LMOTVO-Q3TM      PIC  X(015)       VALUE SPACES.
BSI001       10 WRK-INP-CMOTVO-Q3TM.
BSI001          15 WRK-INP-CMOTVO-Q3TM-N PIC  9(003)       VALUE ZEROS.
BSI001       10 WRK-INP-RMOTVO-Q3TM      PIC  X(017)       VALUE SPACES.
BSI001       10 WRK-INP-LAUTRZ-Q3TM      PIC  X(010)       VALUE SPACES.
BSI001       10 WRK-INP-CAUTRZ-Q3TM.
BSI001          15 WRK-INP-CAUTRZ-Q3TM-N PIC  9(009)        VALUE ZEROS.
BSI001       10 WRK-INP-NAUTRZ-Q3TM      PIC  X(018)       VALUE SPACES.
BSI001       10 WRK-INP-JUSTIF-Q3TM      PIC  X(060)       VALUE SPACES.
             10 WRK-INP-MENSA-Q3TM       PIC  X(079)       VALUE SPACES.
             10 WRK-INP-PFAUX1-Q3TM      PIC  X(003)       VALUE SPACES.
             10 WRK-INP-PFAUX2-Q3TM      PIC  X(009)       VALUE SPACES.
             10 WRK-INP-PFAUX3-Q3TM      PIC  X(003)       VALUE SPACES.
             10 WRK-INP-PFAUX4-Q3TM      PIC  X(009)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMQ3TM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-Q3TM.
           05 WRK-OUT-LL-Q3TM            PIC  9(004) COMP  VALUE ZEROS.
           05 WRK-OUT-ZZ-Q3TM            PIC  9(004) COMP  VALUE ZEROS.
           05 WRK-OUT-DADOS-Q3TM.
             10 WRK-OUT-SENHAS-Q3TM      PIC  X(037)       VALUE SPACES.
             10 WRK-OUT-COMANDO-Q3TM     PIC  X(068)       VALUE SPACES.
             10 WRK-OUT-TITULO-Q3TM      PIC  X(009)       VALUE SPACES.
             10 WRK-OUT-OCULTOS-Q3TM.
               15 WRK-OUT-OPCAO-Q3TM     PIC  9(001)       VALUE ZEROS.
               15 WRK-OUT-TPSELECAO-Q3TM PIC  9(001)       VALUE ZEROS.
               15 WRK-OUT-TMSTAMP-Q3TM   PIC  X(026)       VALUE SPACES.
               15 WRK-OUT-PAGINA1-Q3TM   PIC  9(004)       VALUE ZEROS.
               15 WRK-OUT-PAGINA2-Q3TM   PIC  9(004)       VALUE ZEROS.
               15 WRK-OUT-PAGINA3-Q3TM   PIC  9(004)       VALUE ZEROS.
               15 WRK-OUT-OPMENU-Q3TM    PIC  9(001)       VALUE ZEROS.
               15 WRK-OUT-TPSELEC-Q3TM   PIC  9(001)       VALUE ZEROS.
               15 WRK-OUT-MODBAIX-Q3TM   PIC  X(001)       VALUE SPACES.
               15 WRK-OUT-CHAMADOR-Q3TM  PIC  X(008)       VALUE SPACES.
4S2511*        15 WRK-OUT-NCNPJOCUL-Q3TM PIC  9(009)       VALUE ZEROS.
4S2511         15 WRK-OUT-NCNPJOCUL-Q3TM PIC  X(09)       VALUE SPACES.
               15 FILLER                 PIC  X(019)       VALUE SPACES.
             10 WRK-OUT-NOMECLI-Q3TM     PIC  X(040)       VALUE SPACES.
             10 WRK-OUT-CNPJCPF-Q3TM.
4S2511*        15 WRK-OUT-NCNPJCPF-Q3TM  PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.
4S2511         15 WRK-OUT-NCNPJCPF-Q3TM  PIC  X(11)        VALUE SPACES.
               15 FILLER                 PIC  X(001)       VALUE SPACES.
4S2511*        15 WRK-OUT-FILIAL-Q3TM    PIC  9(004)       VALUE ZEROS.
4S2511         15 WRK-OUT-FILIAL-Q3TM    PIC  X(04)       VALUE SPACES.
               15 FILLER                 PIC  X(001)       VALUE SPACES.
               15 WRK-OUT-CONTROLE-Q3TM  PIC  9(002)       VALUE ZEROS.
             10 WRK-OUT-BCAGCTA-Q3TM.
               15 WRK-OUT-BANCO-Q3TM     PIC  9(003)       VALUE ZEROS.
               15 WRK-OUT-BARRA1-Q3TM    PIC  X(001)       VALUE SPACES.
               15 WRK-OUT-AGENCIA-Q3TM   PIC  9(005)       VALUE ZEROS.
               15 WRK-OUT-BARRA2-Q3TM    PIC  X(001)       VALUE SPACES.
               15 WRK-OUT-CONTA-Q3TM     PIC  9(013)       VALUE ZEROS.
             10 WRK-OUT-CDPRODT-Q3TM     PIC  9(003)       VALUE ZEROS.
             10 WRK-OUT-DSPRODT-Q3TM     PIC  X(012)       VALUE SPACES.
             10 WRK-OUT-CDSPROD-Q3TM     PIC  9(003)       VALUE ZEROS.
             10 WRK-OUT-DSUBPRD-Q3TM     PIC  X(012)       VALUE SPACES.
             10 WRK-OUT-OPERACAO-Q3TM    PIC  9(013)       VALUE ZEROS.
             10 FILLER REDEFINES         WRK-OUT-OPERACAO-Q3TM.
               15 WRK-OUT-DANO-Q3TM      PIC  9(004).
               15 WRK-OUT-NSEQ-Q3TM      PIC  9(009).
             10 WRK-OUT-SITU1-Q3TM       PIC  X(015)       VALUE SPACES.
             10 WRK-OUT-SITU2-Q3TM       PIC  X(013)       VALUE SPACES.
             10 WRK-OUT-SITU3-Q3TM       PIC  X(009)       VALUE SPACES.
             10 WRK-OUT-SITU4-Q3TM       PIC  X(013)       VALUE SPACES.
             10 WRK-OUT-SITU5-Q3TM       PIC  X(009)       VALUE SPACES.
             10 WRK-OUT-PARCELA-Q3TM     PIC  9(005)       VALUE ZEROS.
             10 WRK-OUT-DTVCTO-Q3TM      PIC  X(010)       VALUE SPACES.
             10 WRK-OUT-VLBAIXA-Q3TM.
               15 WRK-OUT-VLBAIXA-Q3TM-N PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-DEBITO-Q3TM      PIC  X(001)       VALUE SPACES.
             10 WRK-OUT-LANCTO-Q3TM      PIC  X(001)       VALUE SPACES.
             10 WRK-OUT-AGEDEB-Q3TM.
               15 WRK-OUT-AGEDEB-Q3TM-N  PIC  9(005)       VALUE ZEROS.
             10 WRK-OUT-CTADEB-Q3TM      PIC  X(014)       VALUE SPACES.
             10 WRK-OUT-CTPOBX-Q3TM      PIC  X(001)       VALUE SPACES.
             10 WRK-OUT-DTPOBX-Q3TM      PIC  X(015)       VALUE SPACES.
BSI001       10 WRK-OUT-LMOTVO-Q3TM      PIC  X(015)       VALUE SPACES.
BSI001       10 WRK-OUT-CMOTVO-Q3TM.
BSI001          15 WRK-OUT-CMOTVO-Q3TM-N PIC  9(003)       VALUE ZEROS.
BSI001       10 WRK-OUT-RMOTVO-Q3TM      PIC  X(017)       VALUE SPACES.
BSI001       10 WRK-OUT-LAUTRZ-Q3TM      PIC  X(010)       VALUE SPACES.
BSI001       10 WRK-OUT-CAUTRZ-Q3TM.
BSI001          15 WRK-OUT-CAUTRZ-Q3TM-N PIC  9(009)        VALUE ZEROS.
BSI001       10 WRK-OUT-NAUTRZ-Q3TM      PIC  X(018)       VALUE SPACES.
BSI001       10 WRK-OUT-JUSTIF-Q3TM      PIC  X(060)       VALUE SPACES.
             10 WRK-OUT-MENSA-Q3TM       PIC  X(079)       VALUE SPACES.
             10 WRK-OUT-PFAUX1-Q3TM      PIC  X(003)       VALUE SPACES.
             10 WRK-OUT-PFAUX2-Q3TM      PIC  X(009)       VALUE SPACES.
             10 WRK-OUT-PFAUX3-Q3TM      PIC  X(003)       VALUE SPACES.
             10 WRK-OUT-PFAUX4-Q3TM      PIC  X(009)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMQ3TM  - BRAD0660 ***'.
      *----------------------------------------------------------------

       01  WRK-660-DCOMQ3TM.
           05 WRK-660-LL-Q3TM          PIC  9(004) COMP    VALUE 0000.
           05 WRK-660-ZZ-Q3TM          PIC  9(004) COMP    VALUE 0000.
           05 WRK-660-SENHAS-Q3TM      PIC  9(004) COMP    VALUE 0037.
           05 WRK-660-COMANDO-Q3TM     PIC  9(004) COMP    VALUE 0068.
           05 WRK-660-TITULO-Q3TM      PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-OCULTOS-Q3TM     PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-NOMECLI-Q3TM     PIC  9(004) COMP    VALUE 0040.
           05 WRK-660-CNPJCPF-Q3TM     PIC  9(004) COMP    VALUE 0019.
           05 WRK-660-BCAGCTA-Q3TM     PIC  9(004) COMP    VALUE 0023.
           05 WRK-660-CDPRODT-Q3TM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-DSPRODT-Q3TM     PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-CDSPROD-Q3TM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-DSUBPRD-Q3TM     PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-OPERACAO-Q3TM    PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-SITU1-Q3TM       PIC  9(004) COMP    VALUE 0015.
           05 WRK-660-SITU2-Q3TM       PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-SITU3-Q3TM       PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-SITU4-Q3TM       PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-SITU5-Q3TM       PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-PARCELA-Q3TM     PIC  9(004) COMP    VALUE 0005.
           05 WRK-660-DTVCTO-Q3TM      PIC  9(004) COMP    VALUE 0010.
           05 WRK-660-VLBAIXA-Q3TM     PIC  9(004) COMP    VALUE 0018.
           05 WRK-660-DEBITO-Q3TM      PIC  9(004) COMP    VALUE 0001.
           05 WRK-660-LANCTO-Q3TM      PIC  9(004) COMP    VALUE 0001.
           05 WRK-660-AGEDEB-Q3TM      PIC  9(004) COMP    VALUE 0005.
           05 WRK-660-CTADEB-Q3TM      PIC  9(004) COMP    VALUE 0014.
           05 WRK-660-CTPOBX-Q3TM      PIC  9(004) COMP    VALUE 0001.
           05 WRK-660-DTPOBX-Q3TM      PIC  9(004) COMP    VALUE 0015.
BSI001     05 WRK-660-LMOTVO-Q3TM      PIC  9(004) COMP    VALUE 0015.
BSI001     05 WRK-660-CMOTVO-Q3TM      PIC  9(004) COMP    VALUE 0003.
BSI001     05 WRK-660-RMOTVO-Q3TM      PIC  9(004) COMP    VALUE 0017.
BSI001     05 WRK-660-LAUTRZ-Q3TM      PIC  9(004) COMP    VALUE 0010.
BSI001     05 WRK-660-CAUTRZ-Q3TM      PIC  9(004) COMP    VALUE 0009.
BSI001     05 WRK-660-NAUTRZ-Q3TM      PIC  9(004) COMP    VALUE 0018.
BSI001     05 WRK-660-JUSTIF-Q3TM      PIC  9(004) COMP    VALUE 0060.
           05 WRK-660-MENSA-Q3TM       PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-PFAUX1-Q3TM      PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-PFAUX2-Q3TM      PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-PFAUX3-Q3TM      PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-PFAUX4-Q3TM      PIC  9(004) COMP    VALUE 0009.

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
4S2511       10 WRK-COMU-CCNPJ-CPF     PIC  X(09)          VALUE SPACES.
4S2511       10 WRK-COMU-CFLIAL-CNPJ   PIC  X(04)          VALUE SPACES.
             10 WRK-COMU-CCTRL-CNPJCPF PIC  9(002)         VALUE ZEROS.
             10 WRK-COMU-PAGINA-1      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-PAGINA-2      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-OP-MENU       PIC  9(001)         VALUE ZEROS.
             10 WRK-COMU-TP-SELECAO    PIC  9(001)         VALUE ZEROS.
             10 WRK-COMU-MODAL-BAIXA   PIC  X(001)         VALUE SPACES.
             10 WRK-COMU-MENSAGEM      PIC  X(079)         VALUE SPACES.
             10 WRK-COMU-PAGINA-3      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-INCL-EXCL     PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM5308 ***'.
      *----------------------------------------------------------------*

       01  WRK-5308-ENTRADA.
           05 WRK-5308E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5308E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5308E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5308E-FLAG           PIC  9(001)         VALUE ZEROS.
           05 WRK-5308E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5308E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5308E-OPERACAO       PIC  9(013)         VALUE ZEROS.
           05 FILLER REDEFINES         WRK-5308E-OPERACAO.
              10 WRK-5308E-DANO-OPER   PIC  9(004).
              10 WRK-5308E-NSEQ-OPER   PIC  9(009).
           05 WRK-5308E-NPCELA-DESC    PIC  9(005)         VALUE ZEROS.
           05 WRK-5308E-CPRODT         PIC  9(003)         VALUE ZEROS.
           05 WRK-5308E-CSPROD-DESC    PIC  9(003)         VALUE ZEROS.
           05 WRK-5308E-AGENCIA-FUNC   PIC  9(005)         VALUE ZEROS.
4S2511*    05 WRK-5308E-CCNPJ-CPF      PIC  9(009)         VALUE ZEROS.
4S2511     05 WRK-5308E-CCNPJ-CPF      PIC  X(09)         VALUE SPACES.
4S2511*    05 WRK-5308E-CFLIAL-CNPJ    PIC  9(004)         VALUE ZEROS.
4S2511     05 WRK-5308E-CFLIAL-CNPJ    PIC  X(04)         VALUE SPACES.
           05 WRK-5308E-CCTRL-CNPJ-CPF PIC  9(005)         VALUE ZEROS.
           05 WRK-5308E-CAG-BCRIA      PIC  9(005)         VALUE ZEROS.
           05 WRK-5308E-CCTA-BCRIA-CLI PIC  9(013)         VALUE ZEROS.
           05 WRK-5308E-FLAG-FUNCAO    PIC  X(001)         VALUE SPACES.

       01  WRK-5308-SAIDA.
           05 WRK-5308S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5308S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM5309 ***'.
      *----------------------------------------------------------------*

       01  WRK-5309-ENTRADA.
           05 WRK-5309E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5309E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5309E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5309E-FLAG           PIC  9(001)         VALUE ZEROS.
           05 WRK-5309E-START-COUNT    PIC  X(001)         VALUE SPACES.
           05 WRK-5309E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5309E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5309E-OPERACAO       PIC  9(013)         VALUE ZEROS.
           05 FILLER REDEFINES         WRK-5309E-OPERACAO.
              10 WRK-5309E-DANO-OPER   PIC  9(004).
              10 WRK-5309E-NSEQ-OPER   PIC  9(009).
           05 WRK-5309E-NPCELA-DESC    PIC  9(005)         VALUE ZEROS.
           05 WRK-5309E-CPRODT         PIC  9(003)         VALUE ZEROS.
           05 WRK-5309E-CSPROD-DESC    PIC  9(003)         VALUE ZEROS.
           05 WRK-5309E-AGENCIA-FUNC   PIC  9(005)         VALUE ZEROS.
           05 WRK-5309E-FLAG-FUNCAO    PIC  X(001)         VALUE SPACES.

       01  WRK-5309-SAIDA.
           05 WRK-5309S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5309S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.

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
           '*** AREA DE COMUNICACAO PROGRAMA DCOM5440 ***'.
      *----------------------------------------------------------------*

       01  WRK-5440-ENTRADA.
           05 WRK-5440E-COD-RETORNO        PIC  X(004)     VALUE SPACES.
           05 WRK-5440E-MSG-RETORNO        PIC  X(079)     VALUE SPACES.
           05 WRK-5440E-RESTART            PIC  9(005)     VALUE ZEROS.
           05 WRK-5440E-FLAG               PIC  X(001)     VALUE SPACES.
           05 WRK-5440E-START-COUNT        PIC  X(001)     VALUE SPACES.
           05 WRK-5440E-CFUNC-BDSCO        PIC  9(009)     VALUE ZEROS.
           05 WRK-5440E-CTERM              PIC  X(008)     VALUE SPACES.
           05 WRK-5440E-IRSUMO-EVNTO       PIC  X(020)     VALUE SPACES.
           05 WRK-5440E-DANO-OPER-DESC     PIC  9(004)     VALUE ZEROS.
           05 WRK-5440E-NSEQ-OPER-DESC     PIC  9(009)     VALUE ZEROS.
           05 WRK-5440E-NPCELA-DESC-COML   PIC  9(005)     VALUE ZEROS.

       01  WRK-5440-SAIDA.
           05 WRK-5440S-COD-RETORNO        PIC  X(004)     VALUE SPACES.
           05 WRK-5440S-MSG-RETORNO        PIC  X(079)     VALUE SPACES.
           05 WRK-5440S-RESTART            PIC  9(005)     VALUE ZEROS.
           05 WRK-5440S-FLAG               PIC  X(001)     VALUE SPACES.
           05 WRK-5440S-QTDE-RETORNADA     PIC  9(003)     VALUE ZEROS.
           05 WRK-5440S-COUNT              PIC  9(009)     VALUE ZEROS.
           05 WRK-5440S-DVCTO-PCELA-DESC   PIC  X(010)     VALUE SPACES.
           05 WRK-5440S-VEFETV-BAIXA-PCELA PIC  9(015)V99  VALUE ZEROS.
           05 WRK-5440S-CINDCD-LOC-DEB     PIC  X(001)     VALUE SPACES.
           05 WRK-5440S-CINDCD-CONSL-EXTRT PIC  X(001)     VALUE SPACES.
           05 WRK-5440S-CAG-DEB-BCRIA      PIC  9(005)     VALUE ZEROS.
           05 WRK-5440S-CCTA-DEB-BCRIA     PIC  9(013)     VALUE ZEROS.
           05 WRK-5440S-CDIG-CTA-EXTER     PIC  9(001)     VALUE ZEROS.
           05 WRK-5440S-CTPO-BAIXA-PCELA   PIC  X(001)     VALUE SPACES.
           05 WRK-5440S-IRSUMO-TPO-BAIXA   PIC  X(010)     VALUE SPACES.
           05 WRK-5440S-RJUST-BAIXA-PCELA  PIC  X(079)     VALUE SPACES.
BSI001     05 WRK-5440S-CMOTVO-BAIXA       PIC  9(003)     VALUE ZEROS.
BSI001     05 WRK-5440S-RMOTVO-BAIXA       PIC  X(030)     VALUE SPACES.
BSI001     05 WRK-5440S-CFUNC-BDSCO-AUTRZ  PIC  9(009)     VALUE ZEROS.
BSI001     05 WRK-5440S-NFUNC-BDSCO-AUTRZ  PIC  X(080)     VALUE SPACES.
BSI001
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM1730 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-IO-PGB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01 LNK-ALT-PGB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PGB
                                             LNK-ALT-PGB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PGB
                                             LNK-ALT-PGB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL 04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-Q3TM

           INITIALIZE WRK-AUXILIARES.

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

           MOVE 'DCOMQ3TM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1730'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMQ3TM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMQ3TM'
               PERFORM 1300-PROCESSAR-BRAD0660
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
           MOVE WRK-COMU-TRANSACAO     TO WRK-INP-CHAMADOR-Q3TM
                                          WRK-OUT-CHAMADOR-Q3TM.
           MOVE WRK-COMU-OPCAO-N       TO WRK-INP-OPCAO-Q3TM
                                          WRK-OUT-OPCAO-Q3TM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TMSTAMP-Q3TM
                                          WRK-OUT-TMSTAMP-Q3TM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGINA1-Q3TM
                                          WRK-OUT-PAGINA1-Q3TM.
           MOVE WRK-COMU-PAGINA-2      TO WRK-INP-PAGINA2-Q3TM
                                          WRK-OUT-PAGINA2-Q3TM.
           MOVE WRK-COMU-PAGINA-3      TO WRK-INP-PAGINA3-Q3TM
                                          WRK-OUT-PAGINA3-Q3TM.
           MOVE WRK-COMU-OP-MENU       TO WRK-INP-OPMENU-Q3TM
                                          WRK-OUT-OPMENU-Q3TM.
           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPSELECAO-Q3TM
                                          WRK-OUT-TPSELECAO-Q3TM.
           MOVE WRK-COMU-OPERACAO      TO WRK-INP-OPERACAO-Q3TM
                                          WRK-OUT-OPERACAO-Q3TM.
           MOVE WRK-COMU-BANCO         TO WRK-INP-BANCO-Q3TM
                                          WRK-OUT-BANCO-Q3TM.
           MOVE WRK-COMU-AGENCIA       TO WRK-INP-AGENCIA-Q3TM
                                          WRK-OUT-AGENCIA-Q3TM.
           MOVE WRK-COMU-CONTA         TO WRK-INP-CONTA-Q3TM
                                          WRK-OUT-CONTA-Q3TM.
           MOVE WRK-COMU-CCNPJ-CPF     TO WRK-INP-NCNPJOCUL-Q3TM
                                          WRK-OUT-NCNPJOCUL-Q3TM.
           MOVE WRK-COMU-CFLIAL-CNPJ   TO WRK-INP-FILIAL-Q3TM
                                          WRK-OUT-FILIAL-Q3TM.
           MOVE WRK-COMU-CCTRL-CNPJCPF TO WRK-INP-CONTROLE-Q3TM
                                          WRK-OUT-CONTROLE-Q3TM.
           MOVE WRK-COMU-MODAL-BAIXA   TO WRK-INP-MODBAIX-Q3TM
                                          WRK-OUT-MODBAIX-Q3TM.
           MOVE WRK-COMU-PARCELA       TO WRK-INP-PARCELA-Q3TM
                                          WRK-OUT-PARCELA-Q3TM.

           EVALUATE WRK-COMU-OPCAO-N
               WHEN 1
                    MOVE '  INCLUIR'   TO WRK-OUT-TITULO-Q3TM
                    MOVE ' 6-'         TO WRK-OUT-PFAUX1-Q3TM
                    MOVE 'CONFIRMAR '  TO WRK-OUT-PFAUX2-Q3TM
                    MOVE '10-'         TO WRK-OUT-PFAUX3-Q3TM
                    MOVE 'MENU DCOM'   TO WRK-OUT-PFAUX4-Q3TM
                    MOVE WRK-MSG001    TO WRK-OUT-MENSA-Q3TM

               WHEN 2
                    MOVE '  EXCLUIR'   TO WRK-OUT-TITULO-Q3TM
                    MOVE ' 6-'         TO WRK-OUT-PFAUX1-Q3TM
                    MOVE 'CONFIRMAR'   TO WRK-OUT-PFAUX2-Q3TM
                    MOVE '10-'         TO WRK-OUT-PFAUX3-Q3TM
                    MOVE 'MENU DCOM'   TO WRK-OUT-PFAUX4-Q3TM
                    MOVE WRK-MSG002    TO WRK-OUT-MENSA-Q3TM

               WHEN 3
                    MOVE 'CONSULTAR'   TO WRK-OUT-TITULO-Q3TM
                    MOVE '10-'         TO WRK-OUT-PFAUX1-Q3TM
                    MOVE 'MENU DCOM'   TO WRK-OUT-PFAUX2-Q3TM
                    MOVE SPACES        TO WRK-OUT-PFAUX3-Q3TM
                                          WRK-OUT-PFAUX4-Q3TM
                    MOVE WRK-MSG003    TO WRK-OUT-MENSA-Q3TM
           END-EVALUATE.

           PERFORM 1110-OBTER-CABECALHO.

           IF  WRK-INCONSIS            EQUAL SPACES
               PERFORM 1120-MONTAR-TELA
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA MONTA A AREA RETORNADA PELO PROGRAMA DCOM4174     *
      *----------------------------------------------------------------*
       1110-OBTER-CABECALHO            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR.

           MOVE WRK-COD-USER           TO 4174-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM.
           MOVE WRK-COMU-DANO-OPER     TO 4174-ENT-DANO-OPER-DESC.
           MOVE WRK-COMU-NSEQ-OPER     TO 4174-ENT-NSEQ-OPER-DESC.
           MOVE 'DCOM4174'             TO WRK-MODULO

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO
               WHEN '0000'
                     PERFORM 1115-FORMATAR-CABECALHO

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    PERFORM 1220-RETORNAR-CHAMADOR
                    MOVE 4174-SAI-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                    MOVE WRK-COMU-AREA TO WRK-MENSAGEM
                    MOVE '*'           TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  FORMATACAO DE CABECALHO COM DADOS RETORNADOS DO DCOM4174      *
      *----------------------------------------------------------------*
       1115-FORMATAR-CABECALHO         SECTION.
      *----------------------------------------------------------------*

           MOVE 4174-SAI-IPSSOA-DESC-COML  TO WRK-OUT-NOMECLI-Q3TM.
           MOVE 4174-SAI-CCNPJ-CPF         TO WRK-INP-NCNPJOCUL-Q3TM
                                              WRK-OUT-NCNPJOCUL-Q3TM.

           STRING 4174-SAI-CCNPJ-CPF(1:3) '.'
                  4174-SAI-CCNPJ-CPF(4:3) '.'
                  4174-SAI-CCNPJ-CPF(7:3) '/'
                  4174-SAI-CFLIAL-CNPJ    '-'
                  4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE               INTO WRK-OUT-CNPJCPF-Q3TM.

           STRING 4174-SAI-CBCO      '/'
                  4174-SAI-CAG-BCRIA '/'
                  4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE               INTO WRK-OUT-BCAGCTA-Q3TM.

           MOVE 4174-SAI-CPRODT            TO WRK-OUT-CDPRODT-Q3TM.
           MOVE 4174-SAI-IABREV-PRODT      TO WRK-OUT-DSPRODT-Q3TM.
           MOVE 4174-SAI-CSPROD-DESC-COML  TO WRK-OUT-CDSPROD-Q3TM.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC TO WRK-OUT-DSUBPRD-Q3TM.
           MOVE 4174-SAI-DANO-OPER-DESC    TO WRK-OUT-DANO-Q3TM.
           MOVE 4174-SAI-NSEQ-OPER-DESC    TO WRK-OUT-NSEQ-Q3TM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC   TO WRK-OUT-SITU1-Q3TM.

           EVALUATE TRUE
               WHEN 4174-SAI-CCONTR-LIM-DESC      GREATER ZEROS
                    MOVE 'CONTR LIMITE:'          TO WRK-OUT-SITU2-Q3TM
                    MOVE 4174-SAI-CCONTR-LIM-DESC TO WRK-OUT-SITU3-Q3TM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 8
                    MOVE 'CONV GERAL..:'          TO WRK-OUT-SITU2-Q3TM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITU3-Q3TM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 9
                    MOVE 'CONV CLIENTE:'          TO WRK-OUT-SITU2-Q3TM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITU3-Q3TM
           END-EVALUATE.

           IF  4174-SAI-NSMULA-OPER-DESC   GREATER ZEROS
               IF  WRK-OUT-SITU2-Q3TM      EQUAL SPACES
                   MOVE 'SIMULACAO...:'    TO WRK-OUT-SITU2-Q3TM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-SITU3-Q3TM
               ELSE
                   MOVE 'SIMULACAO...:'    TO WRK-OUT-SITU4-Q3TM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-SITU5-Q3TM
               END-IF
           END-IF.


      *----------------------------------------------------------------*
       1115-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * MONTAR A TELA DO PROGRAMA                                      *
      *----------------------------------------------------------------*
       1120-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5440-ENTRADA
                      WRK-5440-SAIDA
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE WRK-INP-DANO-Q3TM      TO WRK-5440E-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-Q3TM      TO WRK-5440E-NSEQ-OPER-DESC
           MOVE WRK-INP-PARCELA-Q3TM   TO WRK-5440E-NPCELA-DESC-COML.
           MOVE 'ESTORNO-BAIXA'        TO WRK-5440E-IRSUMO-EVNTO
           MOVE 'DCOM5440'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5440-ENTRADA
                                             WRK-5440-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  WRK-5440S-COD-RETORNO
               WHEN '0000'
                     PERFORM 1125-FORMATAR-TELA

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    PERFORM 1220-RETORNAR-CHAMADOR
                    MOVE WRK-5440S-MSG-RETORNO
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
       1125-FORMATAR-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-5440S-DVCTO-PCELA-DESC   TO WRK-OUT-DTVCTO-Q3TM.

           INSPECT WRK-OUT-DTVCTO-Q3TM       REPLACING ALL '.' BY '/'.

           MOVE WRK-5440S-VEFETV-BAIXA-PCELA TO WRK-OUT-VLBAIXA-Q3TM-N.
           MOVE WRK-5440S-CINDCD-LOC-DEB     TO WRK-OUT-DEBITO-Q3TM
           MOVE WRK-5440S-CINDCD-CONSL-EXTRT TO WRK-OUT-LANCTO-Q3TM

           IF  WRK-5440S-CINDCD-LOC-DEB      EQUAL '2'
               MOVE SPACES                   TO WRK-OUT-AGEDEB-Q3TM
                                                WRK-OUT-CTADEB-Q3TM
           ELSE
               MOVE WRK-5440S-CAG-DEB-BCRIA  TO WRK-OUT-AGEDEB-Q3TM-N
               MOVE WRK-5440S-CCTA-DEB-BCRIA TO WRK-CONTA
               MOVE WRK-5440S-CDIG-CTA-EXTER TO WRK-DIGITO
               MOVE WRK-CONTA-DIGITO         TO WRK-OUT-CTADEB-Q3TM
           END-IF.

           MOVE WRK-5440S-CTPO-BAIXA-PCELA   TO WRK-OUT-CTPOBX-Q3TM.
           MOVE WRK-5440S-IRSUMO-TPO-BAIXA   TO WRK-OUT-DTPOBX-Q3TM.
BSI001
BSI001     IF  WRK-5440S-CMOTVO-BAIXA      GREATER ZEROS
BSI001         MOVE 'MOTIVO BAIXA..:'      TO WRK-OUT-LMOTVO-Q3TM
BSI001         MOVE WRK-5440S-CMOTVO-BAIXA TO WRK-OUT-CMOTVO-Q3TM-N
BSI001         STRING '- ' WRK-5440S-RMOTVO-BAIXA(1:15)
BSI001         DELIMITED BY SIZE         INTO WRK-OUT-RMOTVO-Q3TM
BSI001     ELSE
BSI001         MOVE SPACES                 TO WRK-OUT-LMOTVO-Q3TM
BSI001                                        WRK-OUT-CMOTVO-Q3TM
BSI001                                        WRK-OUT-RMOTVO-Q3TM
BSI001     END-IF.
BSI001
BSI001     IF  WRK-5440S-CFUNC-BDSCO-AUTRZ GREATER ZEROS
BSI001         MOVE 'COD. AUT.:'           TO WRK-OUT-LAUTRZ-Q3TM
BSI001         MOVE WRK-5440S-CFUNC-BDSCO-AUTRZ
BSI001                                     TO WRK-OUT-CAUTRZ-Q3TM-N
BSI001         STRING '- ' WRK-5440S-NFUNC-BDSCO-AUTRZ(1:16)
BSI001         DELIMITED BY SIZE         INTO WRK-OUT-NAUTRZ-Q3TM
BSI001     ELSE
BSI001         MOVE SPACES                 TO WRK-OUT-LAUTRZ-Q3TM
BSI001                                        WRK-OUT-CAUTRZ-Q3TM
BSI001                                        WRK-OUT-NAUTRZ-Q3TM
BSI001     END-IF.
BSI001
BSI001     MOVE WRK-5440S-RJUST-BAIXA-PCELA
BSI001                                 TO WRK-OUT-JUSTIF-Q3TM.
BSI001
      *----------------------------------------------------------------*
       1125-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMQ3TM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMQ3TM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-Q3TM.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-Q3TM     EQUAL 'H' OR
                   (WRK-INP-PFK-Q3TM     EQUAL '.' AND
                    WRK-INP-COMANDO-Q3TM EQUAL 'PFK01')
                    PERFORM 1210-DEVOLVER-TELA
                    MOVE WRK-INP-MENSA-Q3TM TO WRK-OUT-MENSA-Q3TM

               WHEN WRK-INP-PFK-Q3TM     EQUAL '3' OR
                   (WRK-INP-PFK-Q3TM     EQUAL '.' AND
                    WRK-INP-COMANDO-Q3TM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-Q3TM     EQUAL '5' OR
                   (WRK-INP-PFK-Q3TM     EQUAL '.' AND
                    WRK-INP-COMANDO-Q3TM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-Q3TM     EQUAL '6' OR
                   (WRK-INP-PFK-Q3TM     EQUAL '.' AND
                    WRK-INP-COMANDO-Q3TM EQUAL 'PFK06')
                    PERFORM 1240-TRATAR-CONFIRMACAO

               WHEN WRK-INP-PFK-Q3TM     EQUAL 'A' OR
                   (WRK-INP-PFK-Q3TM     EQUAL '.' AND
                    WRK-INP-COMANDO-Q3TM EQUAL 'PFK10')
                    PERFORM 1250-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-Q3TM     EQUAL '.' AND
                    WRK-INP-COMANDO-Q3TM EQUAL  SPACES OR LOW-VALUES
                    PERFORM 1210-DEVOLVER-TELA
                    EVALUATE WRK-INP-OPCAO-Q3TM
                        WHEN 1
                            MOVE WRK-MSG001 TO WRK-OUT-MENSA-Q3TM
                        WHEN 2
                            MOVE WRK-MSG002 TO WRK-OUT-MENSA-Q3TM
                        WHEN 3
                            MOVE WRK-MSG003 TO WRK-OUT-MENSA-Q3TM
                    END-EVALUATE

               WHEN OTHER
                    PERFORM 1210-DEVOLVER-TELA
                    MOVE WRK-MSG004      TO WRK-OUT-MENSA-Q3TM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TITULO-Q3TM    TO WRK-OUT-TITULO-Q3TM.
           MOVE WRK-INP-OCULTOS-Q3TM   TO WRK-OUT-OCULTOS-Q3TM.
           MOVE WRK-INP-NOMECLI-Q3TM   TO WRK-OUT-NOMECLI-Q3TM.
           MOVE WRK-INP-CNPJCPF-Q3TM   TO WRK-OUT-CNPJCPF-Q3TM.
           MOVE WRK-INP-BCAGCTA-Q3TM   TO WRK-OUT-BCAGCTA-Q3TM.
           MOVE WRK-INP-CDPRODT-Q3TM   TO WRK-OUT-CDPRODT-Q3TM.
           MOVE WRK-INP-DSPRODT-Q3TM   TO WRK-OUT-DSPRODT-Q3TM.
           MOVE WRK-INP-CDSPROD-Q3TM   TO WRK-OUT-CDSPROD-Q3TM.
           MOVE WRK-INP-DSUBPRD-Q3TM   TO WRK-OUT-DSUBPRD-Q3TM.
           MOVE WRK-INP-OPERACAO-Q3TM  TO WRK-OUT-OPERACAO-Q3TM.
           MOVE WRK-INP-SITU1-Q3TM     TO WRK-OUT-SITU1-Q3TM.
           MOVE WRK-INP-SITU2-Q3TM     TO WRK-OUT-SITU2-Q3TM.
           MOVE WRK-INP-SITU3-Q3TM     TO WRK-OUT-SITU3-Q3TM.
           MOVE WRK-INP-SITU4-Q3TM     TO WRK-OUT-SITU4-Q3TM.
           MOVE WRK-INP-SITU5-Q3TM     TO WRK-OUT-SITU5-Q3TM.
           MOVE WRK-INP-PARCELA-Q3TM   TO WRK-OUT-PARCELA-Q3TM.
           MOVE WRK-INP-DTVCTO-Q3TM    TO WRK-OUT-DTVCTO-Q3TM.
           MOVE WRK-INP-VLBAIXA-Q3TM   TO WRK-OUT-VLBAIXA-Q3TM.
           MOVE WRK-INP-DEBITO-Q3TM    TO WRK-OUT-DEBITO-Q3TM.
           MOVE WRK-INP-LANCTO-Q3TM    TO WRK-OUT-LANCTO-Q3TM.
           MOVE WRK-INP-AGEDEB-Q3TM    TO WRK-OUT-AGEDEB-Q3TM.
           MOVE WRK-INP-CTADEB-Q3TM    TO WRK-OUT-CTADEB-Q3TM.
           MOVE WRK-INP-CTPOBX-Q3TM    TO WRK-OUT-CTPOBX-Q3TM.
           MOVE WRK-INP-DTPOBX-Q3TM    TO WRK-OUT-DTPOBX-Q3TM.
BSI001     MOVE WRK-INP-LMOTVO-Q3TM    TO WRK-OUT-LMOTVO-Q3TM.
BSI001     MOVE WRK-INP-CMOTVO-Q3TM    TO WRK-OUT-CMOTVO-Q3TM.
BSI001     MOVE WRK-INP-RMOTVO-Q3TM    TO WRK-OUT-RMOTVO-Q3TM.
BSI001     MOVE WRK-INP-LAUTRZ-Q3TM    TO WRK-OUT-LAUTRZ-Q3TM.
BSI001     MOVE WRK-INP-CAUTRZ-Q3TM    TO WRK-OUT-CAUTRZ-Q3TM.
BSI001     MOVE WRK-INP-NAUTRZ-Q3TM    TO WRK-OUT-NAUTRZ-Q3TM.
BSI001     MOVE WRK-INP-JUSTIF-Q3TM    TO WRK-OUT-JUSTIF-Q3TM.
           MOVE WRK-INP-PFAUX1-Q3TM    TO WRK-OUT-PFAUX1-Q3TM.
           MOVE WRK-INP-PFAUX2-Q3TM    TO WRK-OUT-PFAUX2-Q3TM.
           MOVE WRK-INP-PFAUX3-Q3TM    TO WRK-OUT-PFAUX3-Q3TM.
           MOVE WRK-INP-PFAUX4-Q3TM    TO WRK-OUT-PFAUX4-Q3TM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           PERFORM 1225-FORMATAR-COMU-AREA.

           MOVE WRK-INP-CHAMADOR-Q3TM  TO WRK-TELA.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  FORMATACAO DA AREA DE COMUNICACAO                             *
      *----------------------------------------------------------------*
       1225-FORMATAR-COMU-AREA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                     OF WRK-COMU-AREA
                                           TO WRK-COMU-LL.
           MOVE 'DCOM1730'                 TO WRK-COMU-TRANSACAO.
           MOVE WRK-INP-OPCAO-Q3TM         TO WRK-COMU-OPCAO-N.
           MOVE WRK-INP-OPMENU-Q3TM        TO WRK-COMU-OP-MENU.
           MOVE WRK-INP-TPSELECAO-Q3TM     TO WRK-COMU-TP-SELECAO.
           MOVE WRK-INP-TMSTAMP-Q3TM       TO WRK-COMU-TIMESTAMP.
           MOVE WRK-INP-PAGINA1-Q3TM       TO WRK-COMU-PAGINA-1.
           MOVE WRK-INP-PAGINA2-Q3TM       TO WRK-COMU-PAGINA-2.
           MOVE WRK-INP-PAGINA3-Q3TM       TO WRK-COMU-PAGINA-3.
           MOVE WRK-INP-OPERACAO-Q3TM      TO WRK-COMU-OPERACAO.
           MOVE WRK-INP-MODBAIX-Q3TM       TO WRK-COMU-MODAL-BAIXA.
           MOVE WRK-INP-PARCELA-Q3TM       TO WRK-COMU-PARCELA.

           IF  WRK-INP-TPSELECAO-Q3TM      EQUAL 5
               MOVE WRK-INP-NCNPJOCUL-Q3TM TO WRK-COMU-CCNPJ-CPF
               MOVE WRK-INP-FILIAL-Q3TM    TO WRK-COMU-CFLIAL-CNPJ
               MOVE WRK-INP-CONTROLE-Q3TM  TO WRK-COMU-CCTRL-CNPJCPF
           ELSE
               MOVE WRK-INP-BANCO-Q3TM     TO WRK-COMU-BANCO
               MOVE WRK-INP-AGENCIA-Q3TM   TO WRK-COMU-AGENCIA
               MOVE WRK-INP-CONTA-Q3TM     TO WRK-COMU-CONTA
           END-IF.

      *----------------------------------------------------------------*
       1225-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1730'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE '5'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  TRATAMENTO PARA CONFIRMACAO DE INCLUSAO/EXCLUSAO DE ESTORNO   *
      *----------------------------------------------------------------*
       1240-TRATAR-CONFIRMACAO         SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-INP-OPCAO-Q3TM
               WHEN 1
                    PERFORM 1243-ACESSAR-DCOM5335
                    IF  WRK-INCONSIS   EQUAL SPACES
                        PERFORM 1241-TRATAR-INCLUSAO
                    END-IF

               WHEN 2
                    PERFORM 1243-ACESSAR-DCOM5335
                    IF  WRK-INCONSIS   EQUAL SPACES
                        PERFORM 1242-TRATAR-EXCLUSAO
                    END-IF

               WHEN 3
                    PERFORM 1210-DEVOLVER-TELA
                    MOVE WRK-MSG004    TO WRK-OUT-MENSA-Q3TM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1241-TRATAR-INCLUSAO            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5308-ENTRADA
                      WRK-5308-SAIDA
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE WRK-COD-USER-R         TO WRK-5308E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5308E-CTERM.
           MOVE WRK-INP-OPERACAO-Q3TM  TO WRK-5308E-OPERACAO.
           MOVE WRK-INP-PARCELA-Q3TM   TO WRK-5308E-NPCELA-DESC
           MOVE WRK-INP-CDPRODT-Q3TM   TO WRK-5308E-CPRODT.
           MOVE WRK-INP-CDSPROD-Q3TM   TO WRK-5308E-CSPROD-DESC
           MOVE WRK-COD-DEPTO-N        TO WRK-5308E-AGENCIA-FUNC.
           MOVE WRK-INP-NCNPJOCUL-Q3TM TO WRK-5308E-CCNPJ-CPF
           MOVE WRK-INP-FILIAL-Q3TM    TO WRK-5308E-CFLIAL-CNPJ
           MOVE WRK-INP-CONTROLE-Q3TM  TO WRK-5308E-CCTRL-CNPJ-CPF
           MOVE WRK-INP-AGENCIA-Q3TM   TO WRK-5308E-CAG-BCRIA
           MOVE WRK-INP-CONTA-Q3TM     TO WRK-5308E-CCTA-BCRIA-CLI
           MOVE 'I'                    TO WRK-5308E-FLAG-FUNCAO.
           MOVE 'DCOM5308'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5308-ENTRADA
                                             WRK-5308-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  WRK-5308S-COD-RETORNO
               WHEN '0000'
                     PERFORM 1220-RETORNAR-CHAMADOR
                     MOVE WRK-5308S-MSG-RETORNO
                                        TO WRK-COMU-MENSAGEM
                     MOVE 'S'           TO WRK-COMU-INCL-EXCL
                     MOVE WRK-COMU-AREA TO WRK-MENSAGEM

               WHEN '0099'
                     MOVE 'DB2'         TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     PERFORM 1210-DEVOLVER-TELA
                     MOVE WRK-5308S-MSG-RETORNO
                                        TO WRK-OUT-MENSA-Q3TM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1241-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1242-TRATAR-EXCLUSAO            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5309-ENTRADA
                      WRK-5309-SAIDA
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE WRK-COD-USER-R         TO WRK-5309E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5309E-CTERM.
           MOVE WRK-INP-OPERACAO-Q3TM  TO WRK-5309E-OPERACAO.
           MOVE WRK-INP-PARCELA-Q3TM   TO WRK-5309E-NPCELA-DESC
           MOVE WRK-INP-CDPRODT-Q3TM   TO WRK-5309E-CPRODT.
           MOVE WRK-INP-CDSPROD-Q3TM   TO WRK-5309E-CSPROD-DESC
           MOVE WRK-COD-DEPTO-N        TO WRK-5309E-AGENCIA-FUNC.
           MOVE 'DCOM5309'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5309-ENTRADA
                                             WRK-5309-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  WRK-5309S-COD-RETORNO
               WHEN '0000'
                     PERFORM 1220-RETORNAR-CHAMADOR
                     MOVE WRK-5309S-MSG-RETORNO
                                        TO WRK-COMU-MENSAGEM
                     MOVE 'S'           TO WRK-COMU-INCL-EXCL
                     MOVE WRK-COMU-AREA TO WRK-MENSAGEM

               WHEN '0099'
                     MOVE 'DB2'         TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     PERFORM 1210-DEVOLVER-TELA
                     MOVE WRK-5309S-MSG-RETORNO
                                        TO WRK-OUT-MENSA-Q3TM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1242-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  CONSISTIR PERMICAO DE INCLUSAO/EXCLUSAO DE ESTORNO DE BAIXA   *
      *----------------------------------------------------------------*
       1243-ACESSAR-DCOM5335           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5335-ENTRADA
                      5335-SAIDA.

           MOVE WRK-INP-OPERACAO-Q3TM  TO 5335E-OPERACAO.
           MOVE WRK-INP-PARCELA-Q3TM   TO 5335E-NPCELA-DESC-COML.
           MOVE 007                    TO 5335E-CELMTO-DESC-COML.
           MOVE 089                    TO 5335E-CEVNTO-DESC-COML.

           IF  WRK-INP-OPCAO-Q3TM      EQUAL 1
               MOVE 'I'                TO 5335E-ACAO
           ELSE
               MOVE 'E'                TO 5335E-ACAO
           END-IF.

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
                                       TO WRK-OUT-MENSA-Q3TM
                     MOVE '*'          TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       1243-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1250-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1730'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE 'A'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT-Q3TM
                                       TO WRK-OUT-LL-Q3TM
                                          WRK-660-ZZ-Q3TM.
           MOVE LENGTH                 OF WRK-660-DCOMQ3TM
                                       TO WRK-660-LL-Q3TM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-Q3TM
                                             WRK-660-DCOMQ3TM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-Q3TM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
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
               MOVE 'DCOM1730'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM1730'         TO ERR-PGM
           END-IF.

           MOVE WRK-COD-USER           TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.


           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PGB
                                             ERRO-AREA
                                             LNK-ALT-PGB
                                             WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PGB
                                             ERRO-AREA
                                             LNK-ALT-PGB
           END-IF.

           PERFORM 2000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
