
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1712.
       AUTHOR.     ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM1712                                    *
      *    PROGRAMADOR.:   DAVI BRAZ                                   *
      *    ANALISTA....:   EMERSON ARTUR FRANCO                        *
      *    DATA........:   10/01/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTAR ESTORNO DA BAIXA DA PARCELA -     *
      *                    TITULOS                                     *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMQLTM - CONSULTAR EXTORNO DA BAIXA DA PARCELA - TITULOS  *
      *----------------------------------------------------------------*

      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSG - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS DE APLICACAO                 *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM1702          CHAMA: DCOM4174             *
      *                  DCOM1703                 DCOM
      *                  DCOM1704                 DCOM5469             *
      *================================================================*
BSI001*                    ALTERACAO - BSI TECNOLOGIA                  *
BSI001*----------------------------------------------------------------*
BSI001*    ANALISTA....:   LUCIANDRA SILVEIRA - BSI TECNOLOGIA         *
BSI001*    ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO GP. 39        *
BSI001*    DATA........:   06/01/2016                                  *
BSI001*----------------------------------------------------------------*
BSI001*    OBJETIVO....:   INCLUIR CODIGO E DESCRICAO DA BANDEIRA PARA *

BSI001*                    ORPAG LISTADA.                              *
BSI001*                    BSI - 15/0214 -  UNIFICACAO DE BANDEIRAS.   *
BSI001*================================================================*
BSI002*                    BSI TECNOLOGIA - ALTERACAO                  *
BSI002*----------------------------------------------------------------*
BSI002*    PROGRAMADOR.:   LUCIANDRA SILVEIRA - BSI TECNOLOGIA         *
BSI002*    ANALISTA BSI:   LUCIANDRA SILVEIRA - BSI TECNOLOGIA         *
BSI002*    ANALISTA DDS:   RICARDO JAMMAL     - GRUPO 39               *
BSI002*    DATA........:   21/07/2017                                  *
BSI002*----------------------------------------------------------------*
BSI002*    OBJETIVO....:   PERMITIR DETALHAMENTO DOS JUROS DA OPERA-   *
BSI002*                    CAO INCLUINDO <PFK12> PARA ACESSO AOS JU-   *
BSI002*                    ROS MORATORIOS DAS PARCELAS DA OPERACAO.    *
BSI002*                    BSI - 17-0097 - ENCARGOS MORATORIOS.        *
BSI002*----------------------------------------------------------------*
BSI002*    NAVEGACAO...:                                               *
BSI002*    CHAMADO POR.:          -         CHAMA:  DCOM3928           *
BSI002*================================================================*

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
           '*** DCOM1712 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *----------------------------------------------------------------*

      *--- ALPHA    - NORMAL    - DESPROTEGIDO - POS. CURSOR    ---*
       77  WRK-49345                   PIC S9(008) COMP    VALUE +49345.

      *--- PROTEGIDO
       77  WRK-225                     PIC S9(008) COMP    VALUE +225.

      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       77  WRK-49353                   PIC S9(008) COMP    VALUE +49353.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.

           05  WRK-IND                 PIC  9(002) COMP-3  VALUE ZEROS.
           05  ACU-SELECIONADOS        PIC  9(002) COMP-3  VALUE ZEROS.
BSI002     05  ACU-SELECIONADOS-M      PIC  9(002) COMP-3  VALUE ZEROS.
           05  WRK-RESTART             PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.

BSI001 01  WRK-CPF.
BSI001     05 WRK-NRO-CPF              PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
BSI001     05 FILLER                   PIC  X(001)         VALUE '-'.
BSI001     05 WRK-CTRL-CPF             PIC  9(002)         VALUE ZEROS.
BSI001     05 FILLER                   PIC  X(004)         VALUE SPACES.
BSI001
BSI001 01  WRK-CNPJ.
BSI001     05 WRK-NRO-CNPJ             PIC  ZZ.ZZZ.ZZ9     VALUE ZEROS.
BSI001     05 FILLER                   PIC  X(001)         VALUE '/'.
4S2511*    05 WRK-FLIAL-CNPJ           PIC  9(004)         VALUE ZEROS.
4S2511     05 WRK-FLIAL-CNPJ           PIC  X(04)          VALUE SPACES.
BSI001     05 FILLER                   PIC  X(001)         VALUE '-'.
BSI001     05 WRK-CTRL-CNPJ            PIC  9(002)         VALUE ZEROS.
BSI001

BSI001 01  WRK-TITULO-ORPAG.
BSI001     05 FILLER                   PIC  X(008)         VALUE
BSI001        'BANDEIRA'.
BSI001     05 FILLER                   PIC  X(068)         VALUE SPACES.
BSI001
BSI001 01  WRK-TITULO-OUTROS.
BSI001     05 FILLER                   PIC  X(045)         VALUE
BSI001        'NOSSO NUMERO       CNPJ/CPF            SACADO'.
BSI001     05 FILLER                   PIC  X(031)         VALUE SPACES.
BSI001
BSI001 01  WRK-DETALHE-ORPAG.
BSI001     05 WRK-COD-BANDEIRA         PIC  9(002)         VALUE ZEROS.
BSI001     05 FILLER                   PIC  X(003)         VALUE ' - '.
BSI001     05 WRK-DES-BANDEIRA         PIC  X(025)         VALUE SPACES.
BSI001     05 FILLER                   PIC  X(046)         VALUE SPACES.
BSI001
BSI001 01  WRK-DETALHE-OUTROS.
BSI001     05 WRK-NOSSO-NRO            PIC  9(018)         VALUE ZEROS.
BSI001     05 FILLER                   PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-CNPJ-CPF             PIC  X(018)         VALUE SPACES.
BSI001     05 FILLER                   PIC  X(001)         VALUE SPACES.

BSI001     05 WRK-SACADO               PIC  X(038)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG-01                  PIC  X(079)         VALUE
BSI002     'SELECIONE PARCELA COM "D" P/DETALHAR,"M" P/MORATORIA OU TECL
BSI002-    'E <PF3> P/RETORNAR'.
       01  WRK-MSG-02                  PIC  X(079)         VALUE
           'PF INVALIDA'.
       01  WRK-MSG-03                  PIC  X(079)         VALUE
BSI002     'SELECIONE PARCELA APENAS COM "D" OU "M" E TECLE <ENTER>'.
       01  WRK-MSG-04                  PIC  X(079)         VALUE

           'SELECIONE APENAS UMA PARCELA COM "D" E TECLE <ENTER>'.
BSI002 01  WRK-MSG-09                  PIC  X(079)         VALUE
BSI002     'SELECIONE APENAS UMA PARCELA COM "M" E TECLE <ENTER>'.

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
           '*** TELA DCOMQLTM - INPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-INPUT-QLTM.
           05 FILLER                      PIC  X(018)      VALUE SPACES.
           05 WRK-INP-DADOS-QLTM.
             10 WRK-INP-PFK-QLTM          PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SENHAS-QLTM       PIC  X(037)      VALUE SPACES.
             10 WRK-INP-FIMAMOS-QLTM      PIC  X(001)      VALUE SPACES.
             10 WRK-INP-PAGINA-QLTM.
                15 WRK-INP-PAGINA-QLTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-COMANDO-QLTM      PIC  X(068)      VALUE SPACES.
           05 WRK-INP-DADOS-FIXOS1-QLTM.
             10 WRK-INP-OCULTO-QLTM.
                15 WRK-INP-OPCAO-QLTM     PIC  9(001)      VALUE ZEROS.
                15 WRK-INP-TMSTAMP-QLTM   PIC  X(026)      VALUE SPACES.

                15 WRK-INP-PAGINA1-QLTM   PIC  9(004)      VALUE ZEROS.
                15 WRK-INP-PAGINA2-QLTM   PIC  9(004)      VALUE ZEROS.
                15 WRK-INP-OPMENU-QLTM    PIC  9(001)      VALUE ZEROS.
                15 WRK-INP-TPSELEC-QLTM   PIC  9(001)      VALUE ZEROS.
                15 WRK-INP-MODBAIX-QLTM   PIC  X(001)      VALUE SPACES.
                15 WRK-INP-FASE-QLTM      PIC  X(001)      VALUE SPACES.
4S2511*         15 WRK-INP-CPFCNPJOC-QLTM PIC  9(009)      VALUE ZEROS.
4S2511          15 WRK-INP-CPFCNPJOC-QLTM PIC  X(09)       VALUE SPACES.
BSI002          15 FILLER                 PIC  X(031)      VALUE SPACES.
             10 WRK-INP-NOME-QLTM         PIC  X(040)      VALUE SPACES.
             10 WRK-INP-CPFCNPJ-QLTM      PIC  X(019)      VALUE SPACES.
             10 REDEFINES                 WRK-INP-CPFCNPJ-QLTM.
4S2511*         15 WRK-INP-CCNPJ-CPF-QLTM PIC  999.999.999.
4S2511          15 WRK-INP-CCNPJ-CPF-QLTM PIC  X(11).
                15 FILLER                 PIC  X(001).
4S2511*              15 WRK-INP-FILIAL-QLTM    PIC  9(004).
4S2511               15 WRK-INP-FILIAL-QLTM    PIC  X(04).
                15 FILLER                 PIC  X(001).
                15 WRK-INP-CTRL-QLTM      PIC  9(002).
             10 WRK-INP-BCAGCTA-QLTM      PIC  X(023)      VALUE SPACES.
             10 REDEFINES                 WRK-INP-BCAGCTA-QLTM.
                15 WRK-INP-BCO-QLTM       PIC  9(003).
                15 FILLER                 PIC  X(001).
                15 WRK-INP-AGE-QLTM       PIC  9(005).

                15 FILLER                 PIC  X(001).
                15 WRK-INP-CTA-QLTM       PIC  9(013).
             10 WRK-INP-CDPRODT-QLTM.
               15 WRK-INP-CDPRODT-QLTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-DSPRODT-QLTM      PIC  X(012)      VALUE SPACES.
             10 WRK-INP-CDSPROD-QLTM.
               15 WRK-INP-CDSPROD-QLTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-DSUBPRD-QLTM      PIC  X(012)      VALUE SPACES.
             10 WRK-INP-CDOPERA-QLTM.
                15 WRK-INP-DANO-OPER-QLTM PIC  9(004)      VALUE ZEROS.
                15 WRK-INP-NSEQ-OPER-QLTM PIC  9(009)      VALUE ZEROS.
             10 WRK-INP-SITU1-QLTM        PIC  X(015)      VALUE SPACES.
             10 WRK-INP-SITU2-QLTM        PIC  X(013)      VALUE SPACES.
             10 WRK-INP-SITU3-QLTM        PIC  X(009)      VALUE SPACES.
             10 WRK-INP-SITU4-QLTM        PIC  X(013)      VALUE SPACES.
             10 WRK-INP-SITU5-QLTM        PIC  X(009)      VALUE SPACES.
BSI001       10 WRK-INP-TITULO-QLTM       PIC  X(076)      VALUE SPACES.
           05 WRK-INP-DADOS-VAR-QLTM.
             10 WRK-INP-TABELA-QLTM       OCCURS 003  TIMES.
               15 WRK-INP-SELEC-QLTM      PIC  X(001)      VALUE SPACES.
               15 WRK-INP-PAR-QLTM.

                 20 WRK-INP-PAR-QLTM-N    PIC  9(005)      VALUE ZEROS.
               15 WRK-INP-DTVENC-QLTM     PIC  X(010)      VALUE SPACES.
               15 WRK-INP-VLPARC-QLTM     PIC  X(014)      VALUE SPACES.
               15 WRK-INP-VLATUA-QLTM     PIC  X(014)      VALUE SPACES.
               15 WRK-INP-VLBAIX-QLTM     PIC  X(014)      VALUE SPACES.
               15 WRK-INP-NRDOCT-QLTM     PIC  X(012)      VALUE SPACES.
BSI001         15 WRK-INP-DETALHE-QLTM    PIC  X(076)      VALUE SPACES.
           05 WRK-INP-DADOS-FIXOS2-QLTM.
             10 WRK-INP-TOTPRCB-QLTM      PIC  X(005)      VALUE SPACES.
             10 WRK-INP-VALORBX-QLTM      PIC  X(018)      VALUE SPACES.
           05 WRK-INP-MENSA-QLTM          PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQLTM - OUTPUT ***'.
      *---------------------------------------------------------------*

       01 WRK-OUTPUT-QLTM.
          05 WRK-OUT-LL-QLTM                PIC S9(004)    COMP
                                                           VALUE ZEROS.
          05 WRK-OUT-ZZ-QLTM                PIC S9(004)    COMP

                                                           VALUE ZEROS.
          05 WRK-OUT-DADOS-QLTM.
             10 WRK-OUT-SENHAS-QLTM         PIC  X(037)    VALUE SPACES.
             10 WRK-OUT-FIMAMOS-QLTM        PIC  X(001)    VALUE SPACES.
             10 WRK-OUT-PAGINA-QLTM.
               15 WRK-OUT-PAGINA-QLTM-N     PIC  9(003)    VALUE ZEROS.
             10 WRK-OUT-COMANDO-ATTR-QLTM   PIC S9(004)    COMP
                                                           VALUE ZEROS.
             10 WRK-OUT-COMANDO-QLTM        PIC  X(068)    VALUE SPACES.
             10 WRK-OUT-DADOS-FIXOS1-QLTM.
               15 WRK-OUT-OCULTO-QLTM.
                 20 WRK-OUT-OPCAO-QLTM      PIC  9(001)    VALUE ZEROS.
                 20 WRK-OUT-TMSTAMP-QLTM    PIC  X(026)    VALUE SPACES.
                 20 WRK-OUT-PAGINA1-QLTM    PIC  9(004)    VALUE ZEROS.
                 20 WRK-OUT-PAGINA2-QLTM    PIC  9(004)    VALUE ZEROS.
                 20 WRK-OUT-OPMENU-QLTM     PIC  9(001)    VALUE ZEROS.
                 20 WRK-OUT-TPSELEC-QLTM    PIC  9(001)    VALUE ZEROS.
                 20 WRK-OUT-MODBAIX-QLTM    PIC  X(001)    VALUE SPACES.
                 20 WRK-OUT-FASE-QLTM       PIC  X(001)    VALUE SPACES.
4S2511*          20 WRK-OUT-CPFCNPJOC-QLTM  PIC  9(009)    VALUE ZEROS.
4S2511           20 WRK-OUT-CPFCNPJOC-QLTM  PIC  X(09)     VALUE SPACES.
BSI002           20 FILLER                  PIC  X(031)    VALUE SPACES.

               15 WRK-OUT-NOME-QLTM         PIC  X(040)    VALUE SPACES.
               15 WRK-OUT-CPFCNPJ-QLTM      PIC  X(019)    VALUE SPACES.
               15 REDEFINES                 WRK-OUT-CPFCNPJ-QLTM.
4S2511*              20 WRK-OUT-CCNPJ-CPF-QLTM  PIC  999.999.999.
4S2511               20 WRK-OUT-CCNPJ-CPF-QLTM  PIC  X(11).
                  20 FILLER                  PIC  X(001).
4S2511*                 20 WRK-OUT-FILIAL-QLTM     PIC  9(004).
4S2511                  20 WRK-OUT-FILIAL-QLTM     PIC  X(04).
                 20 FILLER                  PIC  X(001).
                 20 WRK-OUT-CTRL-QLTM       PIC  9(002).
               15 WRK-OUT-BCAGCTA-QLTM      PIC  X(023)   VALUE SPACES.
               15 REDEFINES                 WRK-OUT-BCAGCTA-QLTM.
                20 WRK-OUT-BCO-QLTM         PIC  9(003).
                20 FILLER                   PIC  X(001).
                20 WRK-OUT-AGE-QLTM         PIC  9(005).
                20 FILLER                   PIC  X(001).
                20 WRK-OUT-CTA-QLTM         PIC  9(013).
               15 WRK-OUT-CDPRODT-QLTM.
                 20 WRK-OUT-CDPRODT-QLTM-N  PIC  9(003)    VALUE ZEROS.
               15 WRK-OUT-DSPRODT-QLTM      PIC  X(012)    VALUE SPACES.
               15 WRK-OUT-CDSPROD-QLTM.
                 20 WRK-OUT-CDSPROD-QLTM-N  PIC  9(003)    VALUE ZEROS.
               15 WRK-OUT-DSUBPRD-QLTM      PIC  X(012)    VALUE SPACES.

               15 WRK-OUT-CDOPERA-QLTM.
                 20 WRK-OUT-DANO-OPER-QLTM  PIC  9(004)    VALUE ZEROS.
                 20 WRK-OUT-NSEQ-OPER-QLTM  PIC  9(009)    VALUE ZEROS.
               15 WRK-OUT-SITU1-QLTM        PIC  X(015)    VALUE SPACES.
               15 WRK-OUT-SITU2-QLTM        PIC  X(013)    VALUE SPACES.
               15 WRK-OUT-SITU3-QLTM.
                 20 WRK-OUT-SITU3-QLTM-N    PIC  9(009)    VALUE ZEROS.
               15 WRK-OUT-SITU4-QLTM        PIC  X(013)    VALUE SPACES.
               15 WRK-OUT-SITU5-QLTM.
                 20 WRK-OUT-SITU5-QLTM-N    PIC  9(009)    VALUE ZEROS.
BSI001         15 WRK-OUT-TITULO-QLTM       PIC  X(076)    VALUE SPACES.
             10 WRK-OUT-DADOS-VAR-QLTM.
               15 WRK-OUT-TABELA-QLTM       OCCURS 003 TIMES.
                20 WRK-OUT-SELEC-ATTR-QLTM  PIC S9(004)    COMP
                                                           VALUE ZEROS.
                20 WRK-OUT-SELEC-QLTM       PIC  X(001)    VALUE SPACES.
                20 WRK-OUT-PAR-QLTM.
                  25 WRK-OUT-PAR-QLTM-N     PIC  99999     VALUE ZEROS.
                20 WRK-OUT-DTVENC-QLTM      PIC  X(010)    VALUE SPACES.
                20 WRK-OUT-VLPARC-QLTM.
                  25 WRK-OUT-VLPARC-QLTM-N  PIC  ZZZ.ZZZ.ZZ9,99

                                                           VALUE ZEROS.
                20 WRK-OUT-VLATUA-QLTM.
                  25 WRK-OUT-VLATUA-QLTM-N  PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                20 WRK-OUT-VLBAIX-QLTM.
                  25 WRK-OUT-VLBAIX-QLTM-N  PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                20 WRK-OUT-NRDOCT-QLTM      PIC  X(012)    VALUE SPACES.
BSI001          20 WRK-OUT-DETALHE-QLTM     PIC  X(076)    VALUE SPACES.
             10 WRK-OUT-DADOS-FIXOS2-QLTM.
               15 WRK-OUT-TOTPRCB-QLTM      PIC  X(005)    VALUE SPACES.
               15 WRK-OUT-VALORBX-QLTM.
                 20 WRK-OUT-VALORBX-QLTM-N  PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-MENSA-QLTM          PIC  X(079)    VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQLTM - BRAD0660 ***'.
      *---------------------------------------------------------------*


       01  WRK-660-DCOMQLTM.
           05 WRK-660-LL-AREA-QLTM     PIC  9(004) COMP    VALUE  0000.
           05 WRK-660-LL-MENSAGEM-QLTM PIC  9(004) COMP    VALUE  0000.
           05 WRK-660-SENHAS-QLTM      PIC  9(004) COMP    VALUE  0037.
           05 WRK-660-FIMAMOS-QLTM     PIC  9(004) COMP    VALUE  0001.
           05 WRK-660-PAGINA-QLTM      PIC  9(004) COMP    VALUE  0003.
           05 WRK-660-COMANDO-QLTM     PIC  9(004) COMP    VALUE  2070.
           05 WRK-660-OCULTO-QLTM      PIC  9(004) COMP    VALUE  0079.
           05 WRK-660-NOME-QLTM        PIC  9(004) COMP    VALUE  0040.
           05 WRK-660-CPFCNPJ-QLTM     PIC  9(004) COMP    VALUE  0019.
           05 WRK-660-BCAGCTA-QLTM     PIC  9(004) COMP    VALUE  0023.
           05 WRK-660-CDPRODT-QLTM     PIC  9(004) COMP    VALUE  0003.
           05 WRK-660-DSPRODT-QLTM     PIC  9(004) COMP    VALUE  0012.
           05 WRK-660-CDSPROD-QLTM     PIC  9(004) COMP    VALUE  0003.
           05 WRK-660-DSUBPRD-QLTM     PIC  9(004) COMP    VALUE  0012.
           05 WRK-660-CDOPERA-QLTM     PIC  9(004) COMP    VALUE  0013.
           05 WRK-660-SITU1-QLTM       PIC  9(004) COMP    VALUE  0015.
           05 WRK-660-SITU2-QLTM       PIC  9(004) COMP    VALUE  0013.
           05 WRK-660-SITU3-QLTM       PIC  9(004) COMP    VALUE  0009.
           05 WRK-660-SITU4-QLTM       PIC  9(004) COMP    VALUE  0013.
           05 WRK-660-SITU5-QLTM       PIC  9(004) COMP    VALUE  0009.

BSI001     05 WRK-660-TITULO-QLTM      PIC  9(004) COMP    VALUE  0076.
           05 WRK-660-SELEC-0001-QHTM  PIC  9(004) COMP    VALUE  2003.
           05 WRK-660-PAR-0001-QLTM    PIC  9(004) COMP    VALUE  0005.
           05 WRK-660-DTVENC-0001-QLTM PIC  9(004) COMP    VALUE  0010.
           05 WRK-660-VLPARC-0001-QLTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLATUA-0001-QLTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLBAIX-0001-QLTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-NRDOCT-0001-QLTM PIC  9(004) COMP    VALUE  0012.
BSI001     05 WRK-660-DETALHE-001-QLTM PIC  9(004) COMP    VALUE  0076.
           05 WRK-660-SELEC-0002-QHTM  PIC  9(004) COMP    VALUE  2003.
           05 WRK-660-PAR-0002-QLTM    PIC  9(004) COMP    VALUE  0005.
           05 WRK-660-DTVENC-0002-QLTM PIC  9(004) COMP    VALUE  0010.
           05 WRK-660-VLPARC-0002-QLTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLATUA-0002-QLTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLBAIX-0002-QLTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-NRDOCT-0002-QLTM PIC  9(004) COMP    VALUE  0012.
BSI001     05 WRK-660-DETALHE-002-QLTM PIC  9(004) COMP    VALUE  0076.
           05 WRK-660-SELEC-0003-QHTM  PIC  9(004) COMP    VALUE  2003.
           05 WRK-660-PAR-0003-QLTM    PIC  9(004) COMP    VALUE  0005.
           05 WRK-660-DTVENC-0003-QLTM PIC  9(004) COMP    VALUE  0010.
           05 WRK-660-VLPARC-0003-QLTM PIC  9(004) COMP    VALUE  0014.

           05 WRK-660-VLATUA-0003-QLTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLBAIX-0003-QLTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-NRDOCT-0003-QLTM PIC  9(004) COMP    VALUE  0012.
BSI001     05 WRK-660-DETALHE-003-QLTM PIC  9(004) COMP    VALUE  0076.
           05 WRK-660-TOTPRCB-QLTM     PIC  9(004) COMP    VALUE  0005.
           05 WRK-660-VALORBX-QLTM     PIC  9(004) COMP    VALUE  0018.
           05 WRK-660-MENSA-QLTM       PIC  9(004) COMP    VALUE  0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO DO DCOM1702/DCOM1703/DCOM1704 ***'.
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
4S2511 10 WRK-COMU-CCNPJ-CPF     PIC  X(09)          VALUE SPACES.
4S2511 10 WRK-COMU-CFLIAL-CNPJ   PIC  X(04)          VALUE SPACES.
             10 WRK-COMU-CCTRL-CNPJCPF PIC  9(002)         VALUE ZEROS.
             10 WRK-COMU-PAGINA-1      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-PAGINA-2      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-OP-MENU       PIC  9(001)         VALUE ZEROS.
             10 WRK-COMU-TP-SELECAO    PIC  9(001)         VALUE ZEROS.

             10 WRK-COMU-MODAL-BAIXA   PIC  X(001)         VALUE SPACES.
             10 WRK-COMU-MENSAGEM      PIC  X(079)         VALUE SPACES.
             10 WRK-COMU-PAGINA-3      PIC  9(004)         VALUE ZEROS.
BSI002       10 WRK-COMU-INCL-EXCL     PIC  X(001)         VALUE SPACES.
BSI002       10 WRK-COMU-CHAMADOR      PIC  X(008)         VALUE SPACES.

BSI001*----------------------------------------------------------------*
BSI001 01  FILLER                      PIC  X(050)         VALUE
BSI001     '*** AREA AUXILIAR PARA HEXAVISION ***'.
BSI001*----------------------------------------------------------------*
BSI001
BSI001 COPY 'I#DCOMHX'.
BSI001
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO DO DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

      *----------------------------------------------------------------*

       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO COM DCOM5310 ***'.
      *----------------------------------------------------------------*

       01  5310-ENTRADA-ROTEADOR.
           05 5310E-COD-RETORNO          PIC  X(004)       VALUE SPACES.
           05 5310E-MSG-RETORNO          PIC  X(079)       VALUE SPACES.
           05 5310E-RESTART              PIC  9(005)       VALUE ZEROS.
           05 5310E-FLAG                 PIC  X(001)       VALUE SPACES.
           05 5310E-START-COUNT          PIC  X(001)       VALUE SPACES.
           05 5310E-CFUNC-BDSCO          PIC  9(009)       VALUE ZEROS.
           05 5310E-CTERM                PIC  X(008)       VALUE SPACES.
           05 5310E-AGENCIA-FUNC         PIC  9(005)       VALUE ZEROS.
           05 5310E-CPRODT               PIC  9(003)       VALUE ZEROS.
           05 5310E-DANO-OPER-DESC       PIC  9(004)       VALUE ZEROS.
           05 5310E-NSEQ-OPER-DESC       PIC  9(009)       VALUE ZEROS.
           05 5310E-NPCELA-OPER-DESC     PIC  9(005)       VALUE ZEROS.

       01 5310-SAIDA-ROTEADOR.
           05 5310S-COD-RETORNO          PIC  X(004)       VALUE SPACES.
           05 5310S-MSG-RETORNO          PIC  X(079)       VALUE SPACES.

           05 5310S-RESTART              PIC  9(005)       VALUE ZEROS.
           05 5310S-FLAG                 PIC  X(001)       VALUE SPACES.
           05 5310S-QTDE-RETORNADA       PIC  9(003)       VALUE ZEROS.
           05 5310S-COUNT                PIC  9(009)       VALUE ZEROS.
           05 5310S-QTDE-BAIXA           PIC  9(005)       VALUE ZEROS.
           05 5310S-VBAIXA-TOTAL         PIC  9(015)V99    VALUE ZEROS.
           05 5310S-TABELA-SAIDA         OCCURS 070 TIMES.
              10 5310S-NPCELA-DESC-COML  PIC  9(004)       VALUE ZEROS.
              10 5310S-DVCTO-PCELA-DESC  PIC  X(010)       VALUE SPACES.
              10 5310S-VPRINC-PCELA-DESC PIC  9(015)V99    VALUE ZEROS.
              10 5310S-VATUAL-PCELA-DESC PIC  9(015)V99    VALUE ZEROS.
              10 5310S-VEFETV-BAIXA-PCELA
                                         PIC  9(015)V99    VALUE ZEROS.
              10 5310S-CIND-BAIXA-COBR   PIC  X(001)       VALUE SPACES.
              10 5310S-CTITLO-COBR-BCO   PIC  X(012)       VALUE SPACES.
              10 5310S-CSEU-NRO-TITLO    PIC  X(015)       VALUE SPACES.
4S2511*       10 5310S-CCTRL-CNPJ-CPF    PIC  9(009)       VALUE ZEROS.
4S2511        10 5310S-CCTRL-CNPJ-CPF    PIC  9(002)       VALUE ZEROS.
              10 5310S-IPSSOA-SACDO      PIC  X(040)       VALUE SPACES.
              10 5310S-QTDE-CHEQUES      PIC  9(009)       VALUE ZEROS.
              10 5310S-CNPJ              PIC  9(015)       VALUE ZEROS.
              10 FILLER REDEFINES        5310S-CNPJ.

4S2511*            15 5310S-CNPJ-RAIZ      PIC  9(009).
4S2511             15 5310S-CNPJ-RAIZ      PIC  X(09).
4S2511*            15 5310S-CNPJ-FLIAL     PIC  9(004).
4S2511             15 5310S-CNPJ-FLIAL     PIC  X(04).
                  15 5310S-CNPJ-CTRL     PIC  9(002).
              10 5310S-NOSSO-NRO         PIC  9(018)       VALUE ZEROS.
BSI001        10 WRK-5310S-CADM-CATAO-CREDT
BSI001                                   PIC  9(009)       VALUE ZEROS.
BSI001        10 WRK-5310S-IADM-CATAO-CREDT
BSI001                                   PIC  X(025)       VALUE SPACES.
BSI001        10 WRK-5310S-CINDCD-VDA-RCBVL
BSI001                                   PIC  X(001)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM1712 - FIM DA AREA DE WORKING ***'.

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
               GOBACK
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-QLTM
                                          WRK-INCONSIS.

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

           MOVE 'DCOMQLTM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1712'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMQLTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMQLTM'
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
           MOVE WRK-COMU-SENHAS        TO WRK-INP-SENHAS-QLTM
                                          WRK-OUT-SENHAS-QLTM.
           MOVE WRK-COMU-OPCAO-N       TO WRK-INP-OPCAO-QLTM
                                          WRK-OUT-OPCAO-QLTM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TMSTAMP-QLTM
                                          WRK-OUT-TMSTAMP-QLTM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGINA1-QLTM
                                          WRK-OUT-PAGINA1-QLTM.
           MOVE WRK-COMU-PAGINA-2      TO WRK-INP-PAGINA2-QLTM
                                          WRK-OUT-PAGINA2-QLTM.
           MOVE WRK-COMU-OP-MENU       TO WRK-INP-OPMENU-QLTM
                                          WRK-OUT-OPMENU-QLTM.
           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPSELEC-QLTM
                                          WRK-OUT-TPSELEC-QLTM.
           MOVE WRK-COMU-MODAL-BAIXA   TO WRK-INP-MODBAIX-QLTM
                                          WRK-OUT-MODBAIX-QLTM.
           MOVE WRK-COMU-OPERACAO      TO WRK-INP-CDOPERA-QLTM

                                          WRK-OUT-CDOPERA-QLTM.
           MOVE WRK-COMU-CCNPJ-CPF     TO WRK-INP-CCNPJ-CPF-QLTM
                                          WRK-OUT-CCNPJ-CPF-QLTM
BSI002                                    WRK-INP-CPFCNPJOC-QLTM
BSI002                                    WRK-OUT-CPFCNPJOC-QLTM.
           MOVE WRK-COMU-CFLIAL-CNPJ   TO WRK-INP-FILIAL-QLTM
                                          WRK-OUT-FILIAL-QLTM
           MOVE WRK-COMU-CCTRL-CNPJCPF TO WRK-INP-CTRL-QLTM
                                          WRK-OUT-CTRL-QLTM
           MOVE WRK-COMU-BANCO         TO WRK-INP-BCO-QLTM
                                          WRK-OUT-BCO-QLTM
           MOVE WRK-COMU-AGENCIA       TO WRK-INP-AGE-QLTM
                                          WRK-OUT-AGE-QLTM
           MOVE WRK-COMU-CONTA         TO WRK-INP-CTA-QLTM
                                          WRK-OUT-CTA-QLTM

           PERFORM 1110-ACESSAR-DCOM4174.

           IF  WRK-INCONSIS               EQUAL SPACES
               IF  WRK-TRANSACAO          EQUAL 'DCOM1730'
                   MOVE WRK-COMU-PAGINA-3 TO WRK-INP-PAGINA-QLTM-N

                                             WRK-OUT-PAGINA-QLTM-N

                   COMPUTE WRK-RESTART    = (WRK-COMU-PAGINA-3 - 1) * 3
               ELSE
                   MOVE ZEROS             TO WRK-RESTART
                   MOVE 1                 TO WRK-INP-PAGINA-QLTM-N
                                             WRK-OUT-PAGINA-QLTM-N
               END-IF

               PERFORM 1120-ACESSAR-DCOM5310

               IF WRK-COMU-MENSAGEM       EQUAL SPACES
                   MOVE WRK-MSG-01        TO WRK-OUT-MENSA-QLTM
               ELSE
                   MOVE WRK-COMU-MENSAGEM TO WRK-OUT-MENSA-QLTM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA DADOS NA TELA.                            *
      *----------------------------------------------------------------*
       1110-ACESSAR-DCOM4174           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR.

           MOVE WRK-INP-DANO-OPER-QLTM TO 4174-ENT-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ-OPER-QLTM TO 4174-ENT-NSEQ-OPER-DESC.
           MOVE 'N'                    TO 4174-ENT-FLAG.
           MOVE WRK-COD-USER-R         TO 4174-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM.
           MOVE 'DCOM4174'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE  4174-SAI-COD-RETORNO
               WHEN '0000'
                     PERFORM 1111-FORMATAR-CABECALHO

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE 4174-SAI-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                     MOVE '*'          TO WRK-INCONSIS
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

           MOVE 4174-SAI-IPSSOA-DESC-COML
                                       TO WRK-OUT-NOME-QLTM
           STRING 4174-SAI-CCNPJ-CPF(1:3)
           '.'    4174-SAI-CCNPJ-CPF(4:3)
           '.'    4174-SAI-CCNPJ-CPF(7:3)
           '/'    4174-SAI-CFLIAL-CNPJ
           '-'    4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE         INTO WRK-OUT-CPFCNPJ-QLTM

           MOVE WRK-OUT-CPFCNPJ-QLTM   TO WRK-INP-CPFCNPJ-QLTM

           STRING 4174-SAI-CBCO
           '/'    4174-SAI-CAG-BCRIA
           '/'    4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE         INTO WRK-OUT-BCAGCTA-QLTM


           MOVE WRK-OUT-BCAGCTA-QLTM   TO WRK-INP-BCAGCTA-QLTM
           MOVE 4174-SAI-CPRODT        TO WRK-OUT-CDPRODT-QLTM-N
                                          WRK-INP-CDPRODT-QLTM-N
           MOVE 4174-SAI-IABREV-PRODT  TO WRK-OUT-DSPRODT-QLTM
           MOVE 4174-SAI-CSPROD-DESC-COML
                                       TO WRK-OUT-CDSPROD-QLTM-N
           MOVE 4174-SAI-IRSUMO-SPROD-DESC
                                       TO WRK-OUT-DSUBPRD-QLTM
           MOVE SPACES                 TO WRK-OUT-SITU3-QLTM
                                          WRK-OUT-SITU5-QLTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC
                                       TO WRK-OUT-SITU1-QLTM

           EVALUATE TRUE
               WHEN 4174-SAI-CCONTR-LIM-DESC      GREATER ZEROS
                    MOVE 'CONTR LIMITE:'          TO WRK-OUT-SITU2-QLTM
                    MOVE 4174-SAI-CCONTR-LIM-DESC TO WRK-OUT-SITU3-QLTM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 8
                    MOVE 'CONV GERAL..:'          TO WRK-OUT-SITU2-QLTM

                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITU3-QLTM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 9
                    MOVE 'CONV CLIENTE:'          TO WRK-OUT-SITU2-QLTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITU3-QLTM
           END-EVALUATE.

           IF  4174-SAI-NSMULA-OPER-DESC GREATER  ZEROS
               IF  WRK-OUT-SITU2-QLTM    EQUAL SPACES OR LOW-VALUES
                   MOVE 'SIMULACAO...:'  TO WRK-OUT-SITU2-QLTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                         TO WRK-OUT-SITU3-QLTM
               ELSE
                   MOVE 'SIMULACAO...:'  TO WRK-OUT-SITU4-QLTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                         TO WRK-OUT-SITU5-QLTM
               END-IF
           END-IF.


BSI001     IF  4174-SAI-CTPO-DESC-COML EQUAL 004
BSI001         MOVE WRK-TITULO-ORPAG   TO WRK-INP-TITULO-QLTM
BSI001                                    WRK-OUT-TITULO-QLTM
BSI001     ELSE
BSI001         MOVE WRK-TITULO-OUTROS  TO WRK-INP-TITULO-QLTM
BSI001                                    WRK-OUT-TITULO-QLTM
BSI001     END-IF.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA DADOS NA TELA.                            *
      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM5310           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5310-ENTRADA-ROTEADOR
                      5310-SAIDA-ROTEADOR.


           MOVE WRK-COD-USER-R         TO 5310E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 5310E-CTERM
           MOVE WRK-COD-DEPTO-N        TO 5310E-AGENCIA-FUNC.
           MOVE WRK-INP-CDPRODT-QLTM-N TO 5310E-CPRODT.
           MOVE WRK-INP-DANO-OPER-QLTM TO 5310E-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-OPER-QLTM TO 5310E-NSEQ-OPER-DESC

           IF  WRK-COMU-TP-SELECAO     EQUAL 2
               MOVE WRK-COMU-PARCELA   TO 5310E-NPCELA-OPER-DESC
           END-IF.

           MOVE WRK-RESTART            TO 5310E-RESTART.
           MOVE 'DCOM5310'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 5310-ENTRADA-ROTEADOR
                                             5310-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  5310S-COD-RETORNO

               WHEN '0000'
                     PERFORM 1121-FORMATAR-TOT-BAIXAS
                     IF 5310S-NPCELA-DESC-COML(4) EQUAL ZEROS
                        MOVE '*'       TO WRK-OUT-FIMAMOS-QLTM
                     ELSE
                        MOVE SPACES    TO WRK-OUT-FIMAMOS-QLTM
                     END-IF

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE 5310S-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                  MOVE '*'             TO WRK-INCONSIS
                  PERFORM 1220-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*

       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FORMATA TOTAL DE BAIXAS DA TELA.                  *
      *----------------------------------------------------------------*
       1121-FORMATAR-TOT-BAIXAS        SECTION.
      *----------------------------------------------------------------*

           PERFORM
           VARYING WRK-IND             FROM 1 BY 1
             UNTIL WRK-IND             GREATER 3
                IF 5310S-NPCELA-DESC-COML(WRK-IND) EQUAL ZEROS
                   MOVE SPACES         TO WRK-OUT-TABELA-QLTM (WRK-IND)
                   MOVE WRK-225        TO
                                       WRK-OUT-SELEC-ATTR-QLTM(WRK-IND)
               ELSE
                   MOVE WRK-49345      TO
                                       WRK-OUT-SELEC-ATTR-QLTM (WRK-IND)
                   MOVE 5310S-NPCELA-DESC-COML                 (WRK-IND)
                                       TO WRK-OUT-PAR-QLTM-N   (WRK-IND)

                   MOVE 5310S-DVCTO-PCELA-DESC                 (WRK-IND)
                                       TO WRK-OUT-DTVENC-QLTM  (WRK-IND)

                   INSPECT WRK-OUT-DTVENC-QLTM                 (WRK-IND)
                                       REPLACING ALL '.' BY '/'

                   MOVE 5310S-VPRINC-PCELA-DESC                (WRK-IND)
                                       TO WRK-OUT-VLPARC-QLTM-N(WRK-IND)
                   MOVE 5310S-VATUAL-PCELA-DESC                (WRK-IND)
                                       TO WRK-OUT-VLATUA-QLTM-N(WRK-IND)
                   MOVE 5310S-VEFETV-BAIXA-PCELA               (WRK-IND)
                                       TO WRK-OUT-VLBAIX-QLTM-N(WRK-IND)
BSI001
BSI001             IF  WRK-INP-TITULO-QLTM
BSI001                                  EQUAL WRK-TITULO-ORPAG
BSI001                 MOVE WRK-5310S-CADM-CATAO-CREDT(WRK-IND)
BSI001                                 TO HX-NRO-09
BSI001                 MOVE HX-NRO-02  TO WRK-COD-BANDEIRA
BSI001                 MOVE WRK-5310S-IADM-CATAO-CREDT(WRK-IND)
BSI001                                 TO WRK-DES-BANDEIRA
BSI001                 MOVE WRK-DETALHE-ORPAG

BSI001                                 TO WRK-OUT-DETALHE-QLTM(WRK-IND)
BSI001             ELSE
                   IF  5310S-CSEU-NRO-TITLO(WRK-IND) EQUAL SPACES
                                                        OR ZEROS
BSI001                 MOVE SPACES     TO WRK-OUT-TITULO-QLTM
                                          WRK-OUT-NRDOCT-QLTM (WRK-IND)
BSI001                                    WRK-OUT-DETALHE-QLTM(WRK-IND)
BSI001             ELSE
                       MOVE 5310S-CSEU-NRO-TITLO               (WRK-IND)
                                      TO WRK-OUT-NRDOCT-QLTM   (WRK-IND)
                       MOVE 5310S-CTITLO-COBR-BCO              (WRK-IND)
BSI001                                 TO WRK-NOSSO-NRO
BSI001
4S2511*                IF  5310S-CNPJ-FLIAL     (WRK-IND) EQUAL ZEROS
4S2511                 IF  5310S-CNPJ-FLIAL     (WRK-IND) EQUAL SPACES
4S2511                 OR 5310S-CNPJ-FLIAL  (WRK-IND) EQUAL LOW-VALUES
BSI001                     MOVE 5310S-CNPJ-RAIZ (WRK-IND)
BSI001                       TO WRK-NRO-CPF
BSI001                     MOVE 5310S-CNPJ-CTRL (WRK-IND)
BSI001                       TO WRK-CTRL-CPF
BSI001                     MOVE WRK-CPF
BSI001                       TO WRK-CNPJ-CPF
BSI001                 ELSE

BSI001                     MOVE 5310S-CNPJ-RAIZ (WRK-IND)
BSI001                       TO HX-NRO-09
BSI001                     MOVE HX-NRO-08
BSI001                       TO WRK-NRO-CNPJ
BSI001                     MOVE 5310S-CNPJ-FLIAL(WRK-IND)
BSI001                       TO WRK-FLIAL-CNPJ
BSI001                     MOVE 5310S-CNPJ-CTRL (WRK-IND)
BSI001                       TO WRK-CTRL-CNPJ
BSI001                     MOVE WRK-CNPJ
BSI001                       TO WRK-CNPJ-CPF
BSI001                 END-IF
BSI001
BSI001                 MOVE 5310S-IPSSOA-SACDO   (WRK-IND)(1:37)
BSI001                   TO WRK-SACADO
BSI001                 MOVE WRK-DETALHE-OUTROS
BSI001                   TO WRK-OUT-DETALHE-QLTM (WRK-IND)
BSI001             END-IF
BSI001         END-IF
               END-IF
           END-PERFORM.


           MOVE 5310S-QTDE-BAIXA       TO WRK-OUT-TOTPRCB-QLTM.
           MOVE 5310S-VBAIXA-TOTAL     TO WRK-OUT-VALORBX-QLTM-N.

      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMQLTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMQLTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-QLTM.

           PERFORM 1210-DEVOLVER-TELA.

           EVALUATE TRUE

               WHEN WRK-INP-PFK-QLTM     EQUAL 'H' OR
                   (WRK-INP-PFK-QLTM     EQUAL '.' AND

                    WRK-INP-COMANDO-QLTM EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-QLTM
                                         TO WRK-OUT-MENSA-QLTM

               WHEN WRK-INP-PFK-QLTM     EQUAL '3' OR
                   (WRK-INP-PFK-QLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QLTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-QLTM     EQUAL '5' OR
                   (WRK-INP-PFK-QLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QLTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-QLTM     EQUAL 'A' OR
                   (WRK-INP-PFK-QLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QLTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-QLTM     EQUAL '7' OR
                   (WRK-INP-PFK-QLTM     EQUAL '.' AND

                    WRK-INP-COMANDO-QLTM EQUAL 'PFK7')
                    PERFORM 1270-TRATAR-PFK07

               WHEN WRK-INP-PFK-QLTM     EQUAL '8' OR
                   (WRK-INP-PFK-QLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QLTM EQUAL 'PFK8')
                    PERFORM 1280-TRATAR-PFK08

               WHEN WRK-INP-PFK-QLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QLTM EQUAL SPACES
                    PERFORM 1290-TRATAR-ENTER

               WHEN OTHER
                    MOVE WRK-MSG-02      TO WRK-OUT-MENSA-QLTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *  ESSA DEVOLVE A TELA INTEIRA PARA O USUARIO.                   *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-FIMAMOS-QLTM   TO WRK-OUT-FIMAMOS-QLTM
           MOVE WRK-INP-PAGINA-QLTM    TO WRK-OUT-PAGINA-QLTM
           MOVE WRK-INP-DADOS-FIXOS1-QLTM
                                       TO WRK-OUT-DADOS-FIXOS1-QLTM
           MOVE WRK-INP-DADOS-FIXOS2-QLTM
                                       TO WRK-OUT-DADOS-FIXOS2-QLTM.

           PERFORM
           VARYING WRK-IND             FROM 1 BY 1
             UNTIL WRK-IND             GREATER 3
                IF WRK-INP-PAR-QLTM   (WRK-IND)
                                       EQUAL SPACES
                   MOVE SPACES         TO WRK-OUT-TABELA-QLTM  (WRK-IND)
                   MOVE WRK-225        TO
                                       WRK-OUT-SELEC-ATTR-QLTM (WRK-IND)

              ELSE
BSI002            MOVE WRK-INP-SELEC-QLTM(WRK-IND)
BSI002                                 TO WRK-OUT-SELEC-QLTM   (WRK-IND)
                  MOVE WRK-INP-PAR-QLTM(WRK-IND)
                                       TO WRK-OUT-PAR-QLTM     (WRK-IND)
                  MOVE WRK-INP-DTVENC-QLTM(WRK-IND)
                                       TO WRK-OUT-DTVENC-QLTM  (WRK-IND)
                  MOVE WRK-INP-VLPARC-QLTM(WRK-IND)
                                       TO WRK-OUT-VLPARC-QLTM  (WRK-IND)
                  MOVE WRK-INP-VLATUA-QLTM(WRK-IND)
                                       TO WRK-OUT-VLATUA-QLTM  (WRK-IND)
                  MOVE WRK-INP-VLBAIX-QLTM(WRK-IND)
                                       TO WRK-OUT-VLBAIX-QLTM  (WRK-IND)
                  MOVE WRK-INP-NRDOCT-QLTM(WRK-IND)
                                       TO WRK-OUT-NRDOCT-QLTM  (WRK-IND)
BSI001            MOVE WRK-INP-DETALHE-QLTM(WRK-IND)
BSI001                                 TO WRK-OUT-DETALHE-QLTM (WRK-IND)
              END-IF
           END-PERFORM.

      *----------------------------------------------------------------*

       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-COMU-AREA  TO WRK-COMU-LL.

           EVALUATE WRK-INP-TPSELEC-QLTM
               WHEN '3'
                   MOVE 'DCOM1703'     TO WRK-TELA
                   MOVE WRK-INP-BCO-QLTM
                                       TO WRK-COMU-BANCO
                   MOVE WRK-INP-AGE-QLTM
                                       TO WRK-COMU-AGENCIA

               WHEN '4'
                   MOVE 'DCOM1704'     TO WRK-TELA

                   MOVE WRK-INP-BCO-QLTM
                                       TO WRK-COMU-BANCO
                   MOVE WRK-INP-AGE-QLTM
                                       TO WRK-COMU-AGENCIA
                   MOVE WRK-INP-CTA-QLTM
                                       TO WRK-COMU-CONTA

               WHEN '5'
                   MOVE 'DCOM1704'     TO WRK-TELA
                   STRING WRK-INP-CPFCNPJ-QLTM(1:3)
                          WRK-INP-CPFCNPJ-QLTM(5:3)
                          WRK-INP-CPFCNPJ-QLTM(9:3)
                   DELIMITED BY SIZE INTO WRK-COMU-CCNPJ-CPF
                   MOVE WRK-INP-FILIAL-QLTM
                                       TO WRK-COMU-CFLIAL-CNPJ
                   MOVE WRK-INP-CTRL-QLTM
                                       TO WRK-COMU-CCTRL-CNPJCPF
               WHEN OTHER
                   MOVE 'DCOM1702'     TO WRK-TELA

           END-EVALUATE.


           MOVE WRK-INP-SENHAS-QLTM    TO WRK-COMU-SENHAS
           MOVE WRK-INP-OPCAO-QLTM     TO WRK-COMU-OPCAO-N
           MOVE WRK-INP-TMSTAMP-QLTM   TO WRK-COMU-TIMESTAMP
           MOVE WRK-INP-PAGINA1-QLTM   TO WRK-COMU-PAGINA-1
           MOVE WRK-INP-PAGINA2-QLTM   TO WRK-COMU-PAGINA-2
           MOVE WRK-INP-OPMENU-QLTM    TO WRK-COMU-OP-MENU
           MOVE WRK-INP-TPSELEC-QLTM   TO WRK-COMU-TP-SELECAO
           MOVE WRK-INP-MODBAIX-QLTM   TO WRK-COMU-MODAL-BAIXA
           MOVE WRK-INP-CDOPERA-QLTM   TO WRK-COMU-OPERACAO.
           MOVE WRK-INP-PFK-QLTM       TO WRK-COMU-PFK
           MOVE 'DCOM1712'             TO WRK-COMU-TRANSACAO
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *

      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL
           MOVE WRK-CHNG                TO WRK-FUNCAO.
           MOVE 'DCOM1712'              TO WRK-TELA
                                           WRK-COMU-TRANSACAO.
           MOVE 5                       TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.

      *----------------------------------------------------------------*

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1712'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE 'A'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1270-TRATAR-PFK07               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-QLTM-N   EQUAL  1
               MOVE 'NAO EXISTEM MAIS PAGINAS PARA RETORNAR'
                                       TO WRK-OUT-MENSA-QLTM
               GO TO 1270-99-FIM
           END-IF.


           COMPUTE WRK-RESTART = ( WRK-INP-PAGINA-QLTM-N - 2 ) * 2.

           PERFORM 1120-ACESSAR-DCOM5310.

           MOVE 'AMOSTRAGEM CONTINUA'  TO WRK-OUT-MENSA-QLTM.

           SUBTRACT 1                  FROM WRK-OUT-PAGINA-QLTM-N.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK08              *
      *----------------------------------------------------------------*
       1280-TRATAR-PFK08               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-FIMAMOS-QLTM    EQUAL  '*'
               MOVE 'NAO EXISTEM PAGINAS PARA AVANCAR'

                                       TO WRK-OUT-MENSA-QLTM
               GO TO 1280-99-FIM
           END-IF.

           COMPUTE WRK-RESTART         = ( WRK-INP-PAGINA-QLTM-N ) * 2

           PERFORM 1120-ACESSAR-DCOM5310.

           IF  WRK-OUT-FIMAMOS-QLTM    EQUAL '*'
               MOVE 'FIM DE AMOSTRAGEM'TO WRK-OUT-MENSA-QLTM
           ELSE
               MOVE 'AMOSTRAGEM CONTINUA'
                                       TO WRK-OUT-MENSA-QLTM
           END-IF.

           ADD 1                       TO WRK-OUT-PAGINA-QLTM-N.

      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       1290-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                TO ACU-SELECIONADOS
BSI002                                  ACU-SELECIONADOS-M.

           PERFORM VARYING WRK-IND   FROM 1 BY 1
                     UNTIL WRK-IND   GREATER 3
                        OR WRK-INP-PAR-QLTM(WRK-IND) EQUAL SPACES
                   IF WRK-INP-SELEC-QLTM   (WRK-IND)
BSI002                               NOT EQUAL 'D' AND 'M' AND ' '
                      MOVE WRK-49353 TO WRK-OUT-SELEC-ATTR-QLTM(WRK-IND)
                      MOVE '*'       TO WRK-INCONSIS
                   END-IF
                   IF  WRK-INP-SELEC-QLTM(WRK-IND) EQUAL 'D'
                       ADD 1         TO ACU-SELECIONADOS
                   END-IF
BSI002             IF  WRK-INP-SELEC-QLTM(WRK-IND) EQUAL 'M'
BSI002                 ADD 1         TO ACU-SELECIONADOS-M
                   END-IF

           END-PERFORM.

           IF  WRK-INCONSIS          EQUAL '*'
               MOVE WRK-MSG-03       TO WRK-OUT-MENSA-QLTM
               GO TO 1290-99-FIM
           END-IF.

BSI002     IF (ACU-SELECIONADOS        EQUAL ZEROS   AND
BSI002         ACU-SELECIONADOS-M      EQUAL ZEROS)  OR
BSI002        (ACU-SELECIONADOS        GREATER ZEROS AND
BSI002         ACU-SELECIONADOS-M      GREATER ZEROS)
BSI002         MOVE WRK-MSG-03         TO WRK-OUT-MENSA-QLTM
               MOVE '*'                TO WRK-INCONSIS
               PERFORM VARYING WRK-IND FROM 1 BY 1
                         UNTIL WRK-IND GREATER 3
                    IF WRK-INP-SELEC-QLTM(WRK-IND)
                                       NOT EQUAL SPACES AND LOW-VALUES
                       MOVE WRK-49353  TO
                                       WRK-OUT-SELEC-ATTR-QLTM(WRK-IND)
                    END-IF
               END-PERFORM

               GO TO 1290-99-FIM
           END-IF.

BSI002     IF  ACU-SELECIONADOS        GREATER 1
BSI002         MOVE WRK-MSG-04         TO WRK-OUT-MENSA-QLTM
BSI002         MOVE '*'                TO WRK-INCONSIS
BSI002         PERFORM VARYING WRK-IND FROM 1 BY 1
BSI002                   UNTIL WRK-IND GREATER 3
BSI002              IF WRK-INP-SELEC-QLTM(WRK-IND)
BSI002                                 NOT EQUAL SPACES AND LOW-VALUES
BSI002                 MOVE WRK-49353  TO
BSI002                                 WRK-OUT-SELEC-ATTR-QLTM(WRK-IND)
BSI002              END-IF
BSI002         END-PERFORM
BSI002         GO TO 1290-99-FIM
BSI002     END-IF.
BSI002
BSI002     IF  ACU-SELECIONADOS-M      GREATER 1
BSI002         MOVE WRK-MSG-09         TO WRK-OUT-MENSA-QLTM
BSI002         MOVE '*'                TO WRK-INCONSIS
BSI002         PERFORM VARYING WRK-IND FROM 1 BY 1

BSI002                   UNTIL WRK-IND GREATER 3
BSI002              IF WRK-INP-SELEC-QLTM(WRK-IND)
BSI002                                 NOT EQUAL SPACES AND LOW-VALUES
BSI002                 MOVE WRK-49353  TO
BSI002                                 WRK-OUT-SELEC-ATTR-QLTM(WRK-IND)
BSI002              END-IF
BSI002         END-PERFORM
BSI002         GO TO 1290-99-FIM
BSI002     END-IF.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 3
                IF WRK-INP-SELEC-QLTM(WRK-IND) EQUAL 'D'
                   PERFORM 1291-DETALHAR-PARCELA
                END-IF
BSI002          IF WRK-INP-SELEC-QLTM(WRK-IND) EQUAL 'M'
BSI002             PERFORM 1292-DETALHAR-JUROS
BSI002          END-IF
           END-PERFORM.

      *----------------------------------------------------------------*

       1290-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1291-DETALHAR-PARCELA           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-INP-OPCAO-QLTM     TO WRK-COMU-OPCAO-N.
           MOVE WRK-INP-TMSTAMP-QLTM   TO WRK-COMU-TIMESTAMP.
           MOVE WRK-INP-PAGINA1-QLTM   TO WRK-COMU-PAGINA-1.
           MOVE WRK-INP-PAGINA2-QLTM   TO WRK-COMU-PAGINA-2.
           MOVE WRK-INP-PAGINA-QLTM-N  TO WRK-COMU-PAGINA-3.
           MOVE WRK-INP-OPMENU-QLTM    TO WRK-COMU-OP-MENU.
           MOVE WRK-INP-TPSELEC-QLTM   TO WRK-COMU-TP-SELECAO.
           MOVE WRK-INP-MODBAIX-QLTM   TO WRK-COMU-MODAL-BAIXA.
           MOVE WRK-INP-CDOPERA-QLTM   TO WRK-COMU-OPERACAO.
           MOVE 'DCOM1712'             TO WRK-COMU-TRANSACAO

           MOVE WRK-INP-PAR-QLTM-N    (WRK-IND)
                                       TO WRK-COMU-PARCELA.

           MOVE 'DCOM1730'             TO WRK-TELA.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1291-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BSI002*----------------------------------------------------------------*
BSI002 1292-DETALHAR-JUROS             SECTION.
BSI002*----------------------------------------------------------------*
BSI002
BSI002     INITIALIZE WRK-COMU-AREA.
BSI002
BSI002     MOVE LENGTH                 OF WRK-COMU-AREA
BSI002                                 TO WRK-COMU-LL.
BSI002     MOVE WRK-CHNG               TO WRK-FUNCAO.
BSI002     MOVE 'DCOM3928'             TO WRK-TELA.

BSI002     MOVE 'DCOM1712'             TO WRK-COMU-TRANSACAO.
BSI002     MOVE WRK-INP-OPCAO-QLTM     TO WRK-COMU-OPCAO-N.
BSI002     MOVE WRK-INP-TMSTAMP-QLTM   TO WRK-COMU-TIMESTAMP.
BSI002     MOVE WRK-INP-DANO-OPER-QLTM TO WRK-COMU-DANO-OPER.
BSI002     MOVE WRK-INP-NSEQ-OPER-QLTM TO WRK-COMU-NSEQ-OPER.
BSI002
BSI002     PERFORM
BSI002     VARYING WRK-IND             FROM 1 BY 1
BSI002       UNTIL WRK-IND             GREATER 3
BSI002          IF WRK-INP-SELEC-QLTM (WRK-IND) EQUAL 'M'
BSI002             MOVE WRK-INP-PAR-QLTM-N
BSI002                                (WRK-IND) TO WRK-COMU-PARCELA
BSI002          END-IF
BSI002     END-PERFORM.
BSI002
BSI002     MOVE WRK-INP-BCO-QLTM       TO WRK-COMU-BANCO
BSI002     MOVE WRK-INP-AGE-QLTM       TO WRK-COMU-AGENCIA
BSI002     MOVE WRK-INP-CTA-QLTM       TO WRK-COMU-CONTA
BSI002     MOVE WRK-INP-CPFCNPJOC-QLTM TO WRK-COMU-CCNPJ-CPF
BSI002     MOVE WRK-INP-FILIAL-QLTM    TO WRK-COMU-CFLIAL-CNPJ
BSI002     MOVE WRK-INP-CTRL-QLTM      TO WRK-COMU-CCTRL-CNPJCPF

BSI002     MOVE WRK-INP-PAGINA1-QLTM   TO WRK-COMU-PAGINA-1
BSI002     MOVE WRK-INP-PAGINA2-QLTM   TO WRK-COMU-PAGINA-2
BSI002     MOVE WRK-INP-OPMENU-QLTM    TO WRK-COMU-OP-MENU.
BSI002     MOVE WRK-INP-TPSELEC-QLTM   TO WRK-COMU-TP-SELECAO.
BSI002     MOVE WRK-INP-MODBAIX-QLTM   TO WRK-COMU-MODAL-BAIXA.
BSI002     MOVE WRK-INP-PAGINA-QLTM-N  TO WRK-COMU-PAGINA-3.
BSI002     MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.
BSI002
BSI002*----------------------------------------------------------------*
BSI002 1292-99-FIM.                    EXIT.
BSI002*----------------------------------------------------------------*
BSI002
      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1400-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT-QLTM
                                       TO WRK-OUT-LL-QLTM
                                          WRK-660-LL-MENSAGEM-QLTM.

           MOVE LENGTH                 OF WRK-660-DCOMQLTM
                                       TO WRK-660-LL-AREA-QLTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-QLTM
                                             WRK-660-DCOMQLTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-QLTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *  ESSA ROTINA FORMATA E EMITE A MENSAGEM DE ERRO DO PROGRAMA    *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES AND LOW-VALUES
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM1712'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM1712'         TO ERR-PGM
           END-IF.

           MOVE WRK-COD-USER-R         TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO-N        TO ERR-COD-DEPTO.

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
