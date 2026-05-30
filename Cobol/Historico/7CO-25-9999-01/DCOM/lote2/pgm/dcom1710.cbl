      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1710.
       AUTHOR.     ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM1710                                    *
      *    PROGRAMADOR.:   DAVI BRAZ                                   *
      *    ANALISTA....:   EMERSON ARTUR FRANCO                        *
      *    DATA........:   10/01/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EXCLUIR EXTORNO DA BAIXA DA PARCELA -       *
      *                    TITULOS                                     *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMQJTM - EXCLUIR EXTORNO DA BAIXA DA PARCELA - TITULOS    *
      *----------------------------------------------------------------*

      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSG - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *    BRAD7100 - TRATAMENTO DE ERROS DE APLICACAO                 *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM1702          CHAMA: DCOM4174             *
      *                  DCOM1703                 DCOM5310             *
      *                  DCOM1704                 DCOM5309             *
      *                                           DCOM5469             *
      *================================================================*
BSI001*                    ALTERACAO - BSI TECNOLOGIA                  *
BSI001*----------------------------------------------------------------*
BSI001*    ANALISTA....:   LUCIANDRA SILVEIRA - BSI TECNOLOGIA         *
BSI001*    ANALISTA DDS:   RICARDO JAMMAL     - BRADESCO GP. 39        *
BSI001*    DATA........:   23/11/2015                                  *
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
           '*** DCOM1710 - INICIO DA AREA DE WORKING ***'.

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
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.
           05  WRK-QTDE-TELA           PIC  9(003) COMP-3  VALUE ZEROS.
           05  WRK-QTDE-EXCLUIDOS      PIC  9(003) COMP-3  VALUE ZEROS.
           05  WRK-RESTART             PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-MSG-AUX             PIC  X(079)         VALUE SPACES.

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
BSI001     05 WRK-NOSSO-NRO            PIC  X(012)         VALUE ZEROS.
BSI001     05 FILLER                   PIC  X(007)         VALUE SPACES.

BSI001     05 WRK-CNPJ-CPF             PIC  X(018)         VALUE SPACES.
BSI001     05 FILLER                   PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-SACADO               PIC  X(038)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-SELECIONADOS-D          PIC  9(002) COMP-3  VALUE ZEROS.
       01  ACU-SELECIONADOS-E          PIC  9(002) COMP-3  VALUE ZEROS.
BSI002 01  ACU-SELECIONADOS-M          PIC  9(002) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG-01                  PIC  X(079)         VALUE
BSI002     'SELECIONE COM "E" P/EXCLUIR, "D" P/DETALHAR OU "M" P/MORATOR
BSI002-    'IA E TECLE <ENTER>'.

       01  WRK-MSG-02                  PIC  X(079)         VALUE
           'PF INVALIDA'.
       01  WRK-MSG-03                  PIC  X(079)         VALUE
BSI002     'SELECIONE APENAS COM "E", "D" OU "M" E TECLE <ENTER>'.
       01  WRK-MSG-04                  PIC  X(079)         VALUE
           'TECLE <PF6> PARA CONFIRMAR OU <ENTER> PARA ALTERAR'.
       01  WRK-MSG-05                  PIC  X(079)         VALUE
           'EXCLUSAO DE ESTORNO DA BAIXA COMANDADA DE PARCELA EFETUADA C
      -    'OM SUCESSO'.
       01  WRK-MSG-06                  PIC  X(079)         VALUE
BSI002     'SELECIONE UMA PARCELA COM "D" OU "M" OU VARIAS COM "E" E TEC
BSI002-    'LE <ENTER>'.
       01  WRK-MSG-07                  PIC  X(079)         VALUE
           'SELECIONE APENAS UMA PARCELA COM "D" E TECLE <ENTER>'.
BSI002 77  WRK-MSG-09                  PIC  X(079)         VALUE
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
           '*** TELA DCOMQJTM - INPUT ***'.
      *---------------------------------------------------------------*


       01  WRK-INPUT-QJTM.
           05 FILLER                      PIC  X(018)      VALUE SPACES.
           05 WRK-INP-DADOS-QJTM.
             10 WRK-INP-PFK-QJTM          PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SENHAS-QJTM       PIC  X(037)      VALUE SPACES.
             10 WRK-INP-FIMAMOS-QJTM      PIC  X(001)      VALUE SPACES.
             10 WRK-INP-PAGINA-QJTM.
                15 WRK-INP-PAGINA-QJTM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-COMANDO-QJTM      PIC  X(068)      VALUE SPACES.
             10 WRK-INP-DADOS-FIXOS1-QJTM.
               15 WRK-INP-OCULTO-QJTM.
                20 WRK-INP-OPCAO-QJTM     PIC  9(001)      VALUE ZEROS.
                20 WRK-INP-TMSTAMP-QJTM   PIC  X(026)      VALUE SPACES.
                20 WRK-INP-PAGINA1-QJTM   PIC  9(004)      VALUE ZEROS.
                20 WRK-INP-PAGINA2-QJTM   PIC  9(004)      VALUE ZEROS.
                20 WRK-INP-OPMENU-QJTM    PIC  9(001)      VALUE ZEROS.
                20 WRK-INP-TPSELEC-QJTM   PIC  9(001)      VALUE ZEROS.
                20 WRK-INP-MODBAIX-QJTM   PIC  X(001)      VALUE SPACES.
                20 WRK-INP-FASE-QJTM      PIC  X(001)      VALUE SPACES.
4S2511*         20 WRK-INP-CPFCNPJOC-QJTM PIC  9(009)      VALUE ZEROS.
4S2511          20 WRK-INP-CPFCNPJOC-QJTM PIC  X(009)      VALUE SPACES.
BSI002          20 FILLER                 PIC  X(031)      VALUE SPACES.

              15 WRK-INP-NOME-QJTM        PIC  X(040)      VALUE SPACES.
              15 WRK-INP-CPFCNPJ-QJTM     PIC  X(019)      VALUE SPACES.
              15 REDEFINES                WRK-INP-CPFCNPJ-QJTM.
4S2511*         20 WRK-INP-CCNPJ-CPF-QJTM PIC  999.999.999.
4S2511          20 WRK-INP-CCNPJ-CPF-QJTM PIC  X(11).
                20 FILLER                 PIC  X(001).
4S2511*          20 WRK-INP-FILIAL-QJTM    PIC  9(004).
4S2511           20 WRK-INP-FILIAL-QJTM    PIC  X(04).
                20 FILLER                 PIC  X(001).
                20 WRK-INP-CTRL-QJTM      PIC  9(002).
              15 WRK-INP-BCAGCTA-QJTM     PIC  X(023)      VALUE SPACES.
              15 REDEFINES                WRK-INP-BCAGCTA-QJTM.
                20 WRK-INP-BCO-QJTM       PIC  9(003).
                20 FILLER                 PIC  X(001).
                20 WRK-INP-AGE-QJTM       PIC  9(005).
                20 FILLER                 PIC  X(001).
                20 WRK-INP-CTA-QJTM       PIC  9(013).
              15 WRK-INP-CDPRODT-QJTM.
               20 WRK-INP-CDPRODT-QJTM-N  PIC  9(003)      VALUE ZEROS.
              15 WRK-INP-DSPRODT-QJTM     PIC  X(012)      VALUE SPACES.
              15 WRK-INP-CDSPROD-QJTM.
               20 WRK-INP-CDSPROD-QJTM-N  PIC  9(003)      VALUE ZEROS.
              15 WRK-INP-DSUBPRD-QJTM     PIC  X(012)      VALUE SPACES.

              15 WRK-INP-CDOPERA-QJTM.
                20 WRK-INP-DANO-OPER-QJTM PIC  9(004)      VALUE ZEROS.
                20 WRK-INP-NSEQ-OPER-QJTM PIC  9(009)      VALUE ZEROS.
              15 WRK-INP-SITU1-QJTM       PIC  X(015)      VALUE SPACES.
              15 WRK-INP-SITU2-QJTM       PIC  X(013)      VALUE SPACES.
              15 WRK-INP-SITU3-QJTM       PIC  X(009)      VALUE SPACES.
              15 WRK-INP-SITU4-QJTM       PIC  X(013)      VALUE SPACES.
              15 WRK-INP-SITU5-QJTM       PIC  X(009)      VALUE SPACES.
BSI001        15 WRK-INP-TITULO-QJTM      PIC  X(076)      VALUE SPACES.
             10 WRK-INP-TABELA-QJTM       OCCURS 003  TIMES.
               15 WRK-INP-SELEC-QJTM      PIC  X(001)      VALUE SPACES.
               15 WRK-INP-PAR-QJTM.
                 20 WRK-INP-PAR-QJTM-N    PIC  9(005)      VALUE ZEROS.
               15 WRK-INP-DTVENC-QJTM     PIC  X(010)      VALUE SPACES.
               15 WRK-INP-VLPARC-QJTM     PIC  X(014)      VALUE SPACES.
               15 WRK-INP-VLATUA-QJTM     PIC  X(014)      VALUE SPACES.
               15 WRK-INP-VLBAIX-QJTM     PIC  X(014)      VALUE SPACES.
               15 WRK-INP-NRDOCT-QJTM     PIC  X(012)      VALUE SPACES.
BSI001         15 WRK-INP-DETALHE-QJTM    PIC  X(076)      VALUE SPACES.
             10 WRK-INP-DADOS-FIXOS2-QJTM.
               15 WRK-INP-TOTPRCB-QJTM    PIC  X(005)      VALUE SPACES.

               15 WRK-INP-VALORBX-QJTM    PIC  X(018)      VALUE SPACES.
             10 WRK-INP-MENSA-QJTM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQJTM - OUTPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-OUTPUT-QJTM.
           05 WRK-OUT-LL-QJTM             PIC S9(004) COMP VALUE +0000.
           05 WRK-OUT-ZZ-QJTM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-OUT-DADOS-QJTM.
             10 WRK-OUT-SENHAS-QJTM       PIC  X(037)      VALUE SPACES.
             10 WRK-OUT-FIMAMOS-QJTM      PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-PAGINA-QJTM.
               15 WRK-OUT-PAGINA-QJTM-N   PIC  9(003)      VALUE ZEROS.
             10 WRK-OUT-COMANDO-ATTR-QJTM PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-COMANDO-QJTM      PIC  X(068)      VALUE SPACES.
             10 WRK-OUT-DADOS-FIXOS1-QJTM.
               15 WRK-OUT-OCULTO-QJTM.
                20 WRK-OUT-OPCAO-QJTM     PIC  9(001)      VALUE ZEROS.

                20 WRK-OUT-TMSTAMP-QJTM   PIC  X(026)      VALUE SPACES.
                20 WRK-OUT-PAGINA1-QJTM   PIC  9(004)      VALUE ZEROS.
                20 WRK-OUT-PAGINA2-QJTM   PIC  9(004)      VALUE ZEROS.
                20 WRK-OUT-OPMENU-QJTM    PIC  9(001)      VALUE ZEROS.
                20 WRK-OUT-TPSELEC-QJTM   PIC  9(001)      VALUE ZEROS.
                20 WRK-OUT-MODBAIX-QJTM   PIC  X(001)      VALUE SPACES.
                20 WRK-OUT-FASE-QJTM      PIC  X(001)      VALUE SPACES.
4S2511*         20 WRK-OUT-CPFCNPJOC-QJTM PIC  9(009)      VALUE ZEROS.
4S2511          20 WRK-OUT-CPFCNPJOC-QJTM PIC  X(009)      VALUE SPACES.
BSI002          20 FILLER                 PIC  X(031)      VALUE SPACES.
             15 WRK-OUT-NOME-QJTM         PIC  X(040)      VALUE SPACES.
              15 WRK-OUT-CPFCNPJ-QJTM     PIC  X(019)      VALUE SPACES.
              15 REDEFINES                WRK-OUT-CPFCNPJ-QJTM.
4S2511*         20 WRK-OUT-CCNPJ-CPF-QJTM PIC  999.999.999.
4S2511          20 WRK-OUT-CCNPJ-CPF-QJTM PIC  X(11).
                20 FILLER                 PIC  X(001).
4S2511*          20 WRK-OUT-FILIAL-QJTM    PIC  9(004).
4S2511           20 WRK-OUT-FILIAL-QJTM    PIC  X(04).
                20 FILLER                 PIC  X(001).
                20 WRK-OUT-CTRL-QJTM      PIC  9(002).
              15 WRK-OUT-BCAGCTA-QJTM     PIC  X(023)      VALUE SPACES.
              15 REDEFINES                WRK-OUT-BCAGCTA-QJTM.
                20 WRK-OUT-BCO-QJTM       PIC  9(003).
                20 FILLER                 PIC  X(001).

                20 WRK-OUT-AGE-QJTM       PIC  9(005).
                20 FILLER                 PIC  X(001).
                20 WRK-OUT-CTA-QJTM       PIC  9(013).
             15 WRK-OUT-CDPRODT-QJTM.
               20 WRK-OUT-CDPRODT-QJTM-N  PIC  9(003)      VALUE ZEROS.
             15 WRK-OUT-DSPRODT-QJTM      PIC  X(012)      VALUE SPACES.
             15 WRK-OUT-CDSPROD-QJTM.
               20 WRK-OUT-CDSPROD-QJTM-N  PIC  9(003)      VALUE ZEROS.
             15 WRK-OUT-DSUBPRD-QJTM      PIC  X(012)      VALUE SPACES.
             15 WRK-OUT-CDOPERA-QJTM.
                20 WRK-OUT-DANO-OPER-QJTM PIC  9(004)      VALUE ZEROS.
                20 WRK-OUT-NSEQ-OPER-QJTM PIC  9(009)      VALUE ZEROS.
             15 WRK-OUT-SITU1-QJTM        PIC  X(015)      VALUE SPACES.
             15 WRK-OUT-SITU2-QJTM        PIC  X(013)      VALUE SPACES.
             15 WRK-OUT-SITU3-QJTM.
               20 WRK-OUT-SITU3-QJTM-N    PIC  9(009)      VALUE ZEROS.
             15 WRK-OUT-SITU4-QJTM        PIC  X(013)      VALUE SPACES.
             15 WRK-OUT-SITU5-QJTM.
               20 WRK-OUT-SITU5-QJTM-N    PIC  9(009)      VALUE ZEROS.
BSI001       15 WRK-OUT-TITULO-QJTM       PIC  X(076)      VALUE SPACES.
             10 WRK-OUT-DADOS-VAR-QJTM.

               15 WRK-OUT-TABELA-QJTM      OCCURS 003  TIMES.
                20 WRK-OUT-SELEC-ATTR-QJTM PIC  9(004) COMP VALUE ZEROS.
                20 WRK-OUT-SELEC-QJTM      PIC  X(001)     VALUE SPACES.
                20 WRK-OUT-PAR-QJTM.
                  25 WRK-OUT-PAR-QJTM-N    PIC  99999
                                                           VALUE ZEROS.
                20 WRK-OUT-DTVENC-QJTM     PIC  X(010)     VALUE SPACES.
                20 WRK-OUT-VLPARC-QJTM.
                  25 WRK-OUT-VLPARC-QJTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                20 WRK-OUT-VLATUA-QJTM.
                  25 WRK-OUT-VLATUA-QJTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                20 WRK-OUT-VLBAIX-QJTM.
                 25 WRK-OUT-VLBAIX-QJTM-N PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                20 WRK-OUT-NRDOCT-QJTM    PIC  X(012)      VALUE SPACES.
BSI001          20 WRK-OUT-DETALHE-QJTM   PIC  X(076)      VALUE SPACES.
             10 WRK-OUT-DADOS-FIXOS2-QJTM.
               15 WRK-OUT-TOTPRCB-QJTM    PIC  X(005)      VALUE SPACES.
               15 WRK-OUT-VALORBX-QJTM.

                20 WRK-OUT-VALORBX-QJTM-N PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-MENSA-QJTM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                         PIC X(070)       VALUE
           '*** TELA DCOMQJTM - BRAD0660 - SEG. 00 ***'.
      *---------------------------------------------------------------*

       01  WRK-660-DCOMQJTM.
           05 WRK-660-LL-AREA-QJTM     PIC  9(004) COMP    VALUE  0000.
           05 WRK-660-ZZ-MENSAGEM-QJTM PIC  9(004) COMP    VALUE  0000.
           05 WRK-660-SENHAS-QJTM      PIC  9(004) COMP    VALUE  0037.
           05 WRK-660-FIMAMOS-QJTM     PIC  9(004) COMP    VALUE  0001.
           05 WRK-660-PAGINA-QJTM      PIC  9(004) COMP    VALUE  0003.
           05 WRK-660-COMANDO-QJTM     PIC  9(004) COMP    VALUE  2070.
           05 WRK-660-OCULTO-QJTM      PIC  9(004) COMP    VALUE  0079.
           05 WRK-660-NOME-QJTM        PIC  9(004) COMP    VALUE  0040.
           05 WRK-660-CPFCNPJ-QJTM     PIC  9(004) COMP    VALUE  0019.
           05 WRK-660-BCAGCTA-QJTM     PIC  9(004) COMP    VALUE  0023.
           05 WRK-660-CDPRODT-QJTM     PIC  9(004) COMP    VALUE  0003.

           05 WRK-660-DSPRODT-QJTM     PIC  9(004) COMP    VALUE  0012.
           05 WRK-660-CDSPROD-QJTM     PIC  9(004) COMP    VALUE  0003.
           05 WRK-660-DSUBPRD-QJTM     PIC  9(004) COMP    VALUE  0012.
           05 WRK-660-CDOPERA-QJTM     PIC  9(004) COMP    VALUE  0013.
           05 WRK-660-SITU1-QJTM       PIC  9(004) COMP    VALUE  0015.
           05 WRK-660-SITU2-QJTM       PIC  9(004) COMP    VALUE  0013.
           05 WRK-660-SITU3-QJTM       PIC  9(004) COMP    VALUE  0009.
           05 WRK-660-SITU4-QJTM       PIC  9(004) COMP    VALUE  0013.
           05 WRK-660-SITU5-QJTM       PIC  9(004) COMP    VALUE  0009.
BSI001     05 WRK-660-DETALHE-QJTM     PIC  9(004) COMP    VALUE  0076.
           05 WRK-660-SELEC-0001-QJTM  PIC  9(004) COMP    VALUE  2003.
           05 WRK-660-PAR-0001-QJTM    PIC  9(004) COMP    VALUE  0005.
           05 WRK-660-DTVENC-0001-QJTM PIC  9(004) COMP    VALUE  0010.
           05 WRK-660-VLPARC-0001-QJTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLATUA-0001-QJTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLBAIX-0001-QJTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-NRDOCT-0001-QJTM PIC  9(004) COMP    VALUE  0012.
BSI001     05 WRK-660-DETALHE-001-QJTM PIC  9(004) COMP    VALUE  0076.
           05 WRK-660-SELEC-0002-QJTM  PIC  9(004) COMP    VALUE  2003.
           05 WRK-660-PAR-0002-QJTM    PIC  9(004) COMP    VALUE  0005.
           05 WRK-660-DTVENC-0002-QJTM PIC  9(004) COMP    VALUE  0010.

           05 WRK-660-VLPARC-0002-QJTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLATUA-0002-QJTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLBAIX-0002-QJTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-NRDOCT-0002-QJTM PIC  9(004) COMP    VALUE  0012.
BSI001     05 WRK-660-DETALHE-002-QJTM PIC  9(004) COMP    VALUE  0076.
           05 WRK-660-SELEC-0003-QJTM  PIC  9(004) COMP    VALUE  2003.
           05 WRK-660-PAR-0003-QJTM    PIC  9(004) COMP    VALUE  0005.
           05 WRK-660-DTVENC-0003-QJTM PIC  9(004) COMP    VALUE  0010.
           05 WRK-660-VLPARC-0003-QJTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLATUA-0003-QJTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLBAIX-0003-QJTM PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-NRDOCT-0003-QJTM PIC  9(004) COMP    VALUE  0012.
BSI001     05 WRK-660-DETALHE-003-QJTM PIC  9(004) COMP    VALUE  0076.
           05 WRK-660-TOTPRCB-QJTM     PIC  9(004) COMP    VALUE  0005.
           05 WRK-660-VALORBX-QJTM     PIC  9(004) COMP    VALUE  0018.
           05 WRK-660-MENSA-QJTM       PIC  9(004) COMP    VALUE  0079.

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

       01  5310-SAIDA-ROTEADOR.
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
              10 5310S-CCTRL-CNPJ-CPF    PIC  9(009)       VALUE ZEROS.
              10 5310S-IPSSOA-SACDO      PIC  X(040)       VALUE SPACES.
              10 5310S-QTDE-CHEQUES      PIC  9(009)       VALUE ZEROS.
              10 5310S-CNPJ              PIC  9(015)       VALUE ZEROS.
              10 FILLER REDEFINES        5310S-CNPJ.
4S2511*          15 5310S-CNPJ-RAIZ      PIC  9(009).
4S2511           15 5310S-CNPJ-RAIZ      PIC  X(009).
4S2511*          15 5310S-CNPJ-FLIAL     PIC  9(004).
4S2511           15 5310S-CNPJ-FLIAL     PIC  X(04).
                  15 5310S-CNPJ-CTRL     PIC  9(002).
              10 5310S-NOSSO-NRO         PIC  9(018)       VALUE ZEROS.
BSI001        10 WRK-5310S-CADM-CATAO-CREDT
BSI001                                   PIC  9(009)       VALUE ZEROS.
BSI001        10 WRK-5310S-IADM-CATAO-CREDT
BSI001                                   PIC  X(025)       VALUE SPACES.
BSI001        10 WRK-5310S-CINDCD-VDA-RCBVL
BSI001                                   PIC  X(001)       VALUE SPACES.

      *----------------------------------------------------------------*

       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO COM DCOM5309 ***'.
      *----------------------------------------------------------------*

       01  5309-ENTRADA-ROTEADOR.
            03 5309E-COD-RETORNO              PIC  X(04)   VALUE SPACES.
            03 5309E-MSG-RETORNO              PIC  X(79)   VALUE SPACES.
            03 5309E-RESTART                  PIC  9(05)   VALUE ZEROS.
            03 5309E-FLAG                     PIC  9(01)   VALUE ZEROS.
            03 5309E-START-COUNT              PIC  X(01)   VALUE SPACES.
            03 5309E-CFUNC-BDSCO              PIC  9(09)   VALUE ZEROS.
            03 5309E-CTERM                    PIC  X(08)   VALUE SPACES.
            03 5309E-OPERACAO                 PIC  9(13)   VALUE ZEROS.
            03 FILLER REDEFINES 5309E-OPERACAO.
               05 5309E-DANO-OPER-DESC        PIC  9(04).
               05 5309E-NSEQ-OPER-DESC        PIC  9(09).
            03 5309E-NPCELA-DESC-COML         PIC  9(05)   VALUE ZEROS.
            03 5309E-CPRODT                   PIC  9(03)   VALUE ZEROS.
            03 5309E-CSPROD-DESC-COML         PIC  9(03)   VALUE ZEROS.
            03 5309E-AGENCIA-FUNC             PIC  9(05)   VALUE ZEROS.
            03 5309E-FLAG-FUNCAO              PIC  X(001)  VALUE SPACES.

      *
       01  5309-SAIDA-ROTEADOR.
           03  5309S-HEADER.
               05  5309S-COD-RETORNO           PIC  X(004) VALUE SPACES.
               05  5309S-MSG-RETORNO           PIC  X(079) VALUE SPACES.
           03  5309S-CONSISTENCIA.
               05  5309S-TAMANHO-ERRO          PIC  9(001) VALUE ZEROS.
               05  5309S-TABELA-ERROS          OCCURS 008 TIMES.
                   07  5309S-CAMPOS-ERROS      PIC  9(001) VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM DCOM5335 ***'.
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
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE

           '*** DCOM1710 - FIM DA AREA DE WORKING ***'.
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

           INITIALIZE WRK-COMU-AREA
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE SPACES                 TO WRK-OUT-DADOS-QJTM.

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

           MOVE 'DCOMQJTM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1710'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMQJTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMQJTM'
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
           MOVE WRK-COMU-SENHAS        TO WRK-INP-SENHAS-QJTM
                                          WRK-OUT-SENHAS-QJTM.
           MOVE WRK-COMU-OPCAO-N       TO WRK-INP-OPCAO-QJTM
                                          WRK-OUT-OPCAO-QJTM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TMSTAMP-QJTM
                                          WRK-OUT-TMSTAMP-QJTM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGINA1-QJTM
                                          WRK-OUT-PAGINA1-QJTM.
           MOVE WRK-COMU-PAGINA-2      TO WRK-INP-PAGINA2-QJTM
                                          WRK-OUT-PAGINA2-QJTM.
           MOVE WRK-COMU-OP-MENU       TO WRK-INP-OPMENU-QJTM
                                          WRK-OUT-OPMENU-QJTM.
           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPSELEC-QJTM

                                          WRK-OUT-TPSELEC-QJTM.
           MOVE WRK-COMU-MODAL-BAIXA   TO WRK-INP-MODBAIX-QJTM
                                          WRK-OUT-MODBAIX-QJTM.
           MOVE WRK-COMU-OPERACAO      TO WRK-INP-CDOPERA-QJTM
                                          WRK-OUT-CDOPERA-QJTM.
           MOVE WRK-COMU-CCNPJ-CPF     TO WRK-INP-CCNPJ-CPF-QJTM
                                          WRK-OUT-CCNPJ-CPF-QJTM
BSI002                                    WRK-INP-CPFCNPJOC-QJTM
BSI002                                    WRK-OUT-CPFCNPJOC-QJTM
           MOVE WRK-COMU-CFLIAL-CNPJ   TO WRK-INP-FILIAL-QJTM
                                          WRK-OUT-FILIAL-QJTM
           MOVE WRK-COMU-CCTRL-CNPJCPF TO WRK-INP-CTRL-QJTM
                                          WRK-OUT-CTRL-QJTM
           MOVE WRK-COMU-BANCO         TO WRK-INP-BCO-QJTM
                                          WRK-OUT-BCO-QJTM
           MOVE WRK-COMU-AGENCIA       TO WRK-INP-AGE-QJTM
                                          WRK-OUT-AGE-QJTM
           MOVE WRK-COMU-CONTA         TO WRK-INP-CTA-QJTM
                                          WRK-OUT-CTA-QJTM
           MOVE WRK-COMU-MENSAGEM      TO WRK-MSG-AUX.


           PERFORM 1110-ACESSAR-DCOM4174.

           IF  WRK-INCONSIS               EQUAL SPACES
               IF  WRK-TRANSACAO          EQUAL 'DCOM1730'
                   MOVE WRK-COMU-PAGINA-3 TO WRK-INP-PAGINA-QJTM-N
                                             WRK-OUT-PAGINA-QJTM-N

                   COMPUTE WRK-RESTART    = (WRK-COMU-PAGINA-3 - 1) * 3
               ELSE
                   MOVE ZEROS             TO WRK-RESTART
                   MOVE 1                 TO WRK-INP-PAGINA-QJTM-N
                                             WRK-OUT-PAGINA-QJTM-N
               END-IF

               PERFORM 1120-ACESSAR-DCOM5310

               IF  WRK-INCONSIS          EQUAL '*'
                   IF WRK-TRANSACAO      EQUAL 'DCOM1730' AND
                      WRK-COMU-INCL-EXCL EQUAL 'S'
                      PERFORM 1220-RETORNAR-CHAMADOR
                      MOVE 'S'           TO WRK-COMU-INCL-EXCL

                      MOVE WRK-MSG-AUX   TO WRK-COMU-MENSAGEM
                   ELSE
                      PERFORM 1220-RETORNAR-CHAMADOR
                      MOVE 5310S-MSG-RETORNO
                                         TO WRK-COMU-MENSAGEM
                   END-IF
                   MOVE WRK-COMU-AREA    TO WRK-MENSAGEM
               END-IF
               MOVE '1'                  TO WRK-OUT-FASE-QJTM
           END-IF.

BSI002     IF   WRK-TRANSACAO            EQUAL 'DCOM3928'
BSI002          IF   WRK-MSG-AUX         EQUAL SPACES
BSI002               MOVE WRK-MSG-01     TO WRK-OUT-MENSA-QJTM
BSI002          ELSE
BSI002               MOVE WRK-MSG-AUX    TO WRK-OUT-MENSA-QJTM
BSI002          END-IF
BSI002     END-IF.
BSI002
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

           MOVE WRK-INP-DANO-OPER-QJTM TO 4174-ENT-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ-OPER-QJTM TO 4174-ENT-NSEQ-OPER-DESC.
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
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    PERFORM 1220-RETORNAR-CHAMADOR
                    MOVE 4174-SAI-MSG-RETORNO
                                       TO WRK-OUT-MENSA-QJTM
                    MOVE '*'           TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *  ESSA ROTINA FORMATA O CABECALHO DA TELA.                      *
      *----------------------------------------------------------------*
       1111-FORMATAR-CABECALHO         SECTION.
      *----------------------------------------------------------------*

           MOVE 4174-SAI-IPSSOA-DESC-COML TO WRK-OUT-NOME-QJTM.

           STRING 4174-SAI-CCNPJ-CPF(1:3)
              '.' 4174-SAI-CCNPJ-CPF(4:3)
              '.' 4174-SAI-CCNPJ-CPF(7:3)
              '/' 4174-SAI-CFLIAL-CNPJ
              '-' 4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE              INTO WRK-OUT-CPFCNPJ-QJTM.

           MOVE WRK-OUT-CPFCNPJ-QJTM      TO WRK-INP-CPFCNPJ-QJTM.

           STRING 4174-SAI-CBCO
              '/' 4174-SAI-CAG-BCRIA
              '/' 4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE              INTO WRK-OUT-BCAGCTA-QJTM.


           MOVE WRK-OUT-BCAGCTA-QJTM      TO WRK-INP-BCAGCTA-QJTM.
           MOVE 4174-SAI-CPRODT           TO WRK-OUT-CDPRODT-QJTM-N
                                             WRK-INP-CDPRODT-QJTM-N.
           MOVE 4174-SAI-IABREV-PRODT     TO WRK-OUT-DSPRODT-QJTM.
           MOVE 4174-SAI-CSPROD-DESC-COML TO WRK-OUT-CDSPROD-QJTM-N.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC
                                          TO WRK-OUT-DSUBPRD-QJTM.
           MOVE SPACES                    TO WRK-OUT-SITU3-QJTM
                                             WRK-OUT-SITU5-QJTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC  TO WRK-OUT-SITU1-QJTM

           IF   4174-SAI-CELMTO-DESC-COML EQUAL 08
                MOVE 'CONV. GERAL.:'      TO WRK-OUT-SITU2-QJTM
                MOVE 4174-SAI-CCONTR-CONVE-DESC
                                          TO WRK-OUT-SITU3-QJTM
           END-IF.

           IF   4174-SAI-CELMTO-DESC-COML EQUAL 09
                MOVE 'CONV.CLIENTE:'      TO WRK-OUT-SITU2-QJTM
                MOVE 4174-SAI-CCONTR-CONVE-DESC

                                          TO WRK-OUT-SITU3-QJTM
           END-IF.

           IF   4174-SAI-CELMTO-DESC-COML EQUAL 10
                MOVE 'CONTR.LIMITE:'      TO WRK-OUT-SITU2-QJTM
                MOVE 4174-SAI-CCONTR-LIM-DESC
                                          TO WRK-OUT-SITU3-QJTM
           END-IF.

           IF  4174-SAI-NSMULA-OPER-DESC  GREATER ZEROS
               IF  WRK-OUT-SITU2-QJTM     EQUAL SPACES OR LOW-VALUES
                   MOVE 'SIMULACAO...:'   TO WRK-OUT-SITU2-QJTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                          TO WRK-OUT-SITU3-QJTM
               ELSE
                   MOVE 'SIMULACAO...:'   TO WRK-OUT-SITU4-QJTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                          TO WRK-OUT-SITU5-QJTM
               END-IF
           END-IF.


BSI001     IF  4174-SAI-CTPO-DESC-COML EQUAL 004
BSI001         MOVE WRK-TITULO-ORPAG   TO WRK-INP-TITULO-QJTM
BSI001                                    WRK-OUT-TITULO-QJTM
BSI001     ELSE
BSI001         MOVE WRK-TITULO-OUTROS  TO WRK-INP-TITULO-QJTM
BSI001                                    WRK-OUT-TITULO-QJTM
BSI001     END-IF.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM5310           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5310-ENTRADA-ROTEADOR
                      5310-SAIDA-ROTEADOR.

           MOVE WRK-COD-USER-R         TO 5310E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 5310E-CTERM.

           MOVE WRK-COD-DEPTO-N        TO 5310E-AGENCIA-FUNC.
           MOVE WRK-INP-CDPRODT-QJTM-N TO 5310E-CPRODT.
           MOVE WRK-INP-DANO-OPER-QJTM TO 5310E-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ-OPER-QJTM TO 5310E-NSEQ-OPER-DESC.
           MOVE WRK-RESTART            TO 5310E-RESTART.

           IF  WRK-INP-TPSELEC-QJTM    EQUAL 2
               MOVE WRK-COMU-PARCELA   TO 5310E-NPCELA-OPER-DESC
           END-IF

           MOVE 'DCOM5310'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 5310-ENTRADA-ROTEADOR
                                             5310-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE 5310S-COD-RETORNO
               WHEN '0000'
                     PERFORM 1121-FORMATAR-TOT-BAIXAS

                     IF  5310S-NPCELA-DESC-COML(4) EQUAL ZEROS
                         MOVE '*'      TO WRK-OUT-FIMAMOS-QJTM
                     ELSE
                         MOVE SPACES   TO WRK-OUT-FIMAMOS-QJTM
                     END-IF

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE 5310S-MSG-RETORNO     TO WRK-OUT-MENSA-QJTM
                     MOVE '*'                   TO WRK-INCONSIS
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
                   MOVE SPACES         TO WRK-OUT-TABELA-QJTM (WRK-IND)
                   MOVE WRK-225        TO
                                       WRK-OUT-SELEC-ATTR-QJTM(WRK-IND)
                ELSE
                   MOVE WRK-49345      TO
                                       WRK-OUT-SELEC-ATTR-QJTM(WRK-IND)
                   MOVE SPACES         TO WRK-OUT-SELEC-QJTM  (WRK-IND)
                   MOVE 5310S-NPCELA-DESC-COML                (WRK-IND)
                                       TO WRK-OUT-PAR-QJTM-N  (WRK-IND)
                   MOVE 5310S-DVCTO-PCELA-DESC                (WRK-IND)
                                       TO WRK-OUT-DTVENC-QJTM (WRK-IND)
                   INSPECT WRK-OUT-DTVENC-QJTM                (WRK-IND)
                                       REPLACING ALL '.' BY '/'

                   MOVE 5310S-VPRINC-PCELA-DESC               (WRK-IND)
                                       TO WRK-OUT-VLPARC-QJTM-N(WRK-IND)
                   MOVE 5310S-VATUAL-PCELA-DESC(WRK-IND)
                                       TO WRK-OUT-VLATUA-QJTM-N(WRK-IND)
                   MOVE 5310S-VEFETV-BAIXA-PCELA(WRK-IND)
                                       TO WRK-OUT-VLBAIX-QJTM-N(WRK-IND)
BSI001
BSI001             IF  WRK-INP-TITULO-QJTM
BSI001                                  EQUAL WRK-TITULO-ORPAG
BSI001                 MOVE WRK-5310S-CADM-CATAO-CREDT(WRK-IND)
BSI001                                 TO HX-NRO-09
BSI001                 MOVE HX-NRO-02  TO WRK-COD-BANDEIRA
BSI001                 MOVE WRK-5310S-IADM-CATAO-CREDT(WRK-IND)
BSI001                                 TO WRK-DES-BANDEIRA
BSI001                 MOVE WRK-DETALHE-ORPAG
BSI001                                 TO WRK-OUT-DETALHE-QJTM(WRK-IND)
BSI001             ELSE
                   IF  5310S-CSEU-NRO-TITLO(WRK-IND) EQUAL SPACES
                                                        OR ZEROS
BSI001                 MOVE SPACES     TO WRK-OUT-TITULO-QJTM
                                          WRK-OUT-NRDOCT-QJTM (WRK-IND)

BSI001                                    WRK-OUT-DETALHE-QJTM(WRK-IND)
BSI001             ELSE
                       MOVE 5310S-CSEU-NRO-TITLO             (WRK-IND)
                                       TO WRK-OUT-NRDOCT-QJTM(WRK-IND)
                       MOVE 5310S-CTITLO-COBR-BCO            (WRK-IND)
BSI001                                 TO WRK-NOSSO-NRO
BSI001
4S2511*                IF  5310S-CNPJ-FLIAL     (WRK-IND) EQUAL ZEROS
4S2511                 IF  5310S-CNPJ-FLIAL     (WRK-IND) EQUAL SPACES
4S2511                                                    OR LOW-VALUES
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
BSI001                   TO WRK-OUT-DETALHE-QJTM (WRK-IND)
BSI001             END-IF
BSI001         END-IF
               END-IF
           END-PERFORM.

           MOVE 5310S-QTDE-BAIXA       TO WRK-OUT-TOTPRCB-QJTM.
           MOVE 5310S-VBAIXA-TOTAL     TO WRK-OUT-VALORBX-QJTM-N.

      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMQJTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMQJTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-QJTM.

           PERFORM 1210-DEVOLVER-TELA.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-QJTM     EQUAL 'H' OR
                   (WRK-INP-PFK-QJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QJTM EQUAL 'PFK01')
                    IF WRK-INP-FASE-QJTM EQUAL '2'
                       MOVE WRK-225      TO WRK-OUT-SELEC-ATTR-QJTM(1)
                                            WRK-OUT-SELEC-ATTR-QJTM(2)
                                            WRK-OUT-SELEC-ATTR-QJTM(3)
                       MOVE WRK-49345    TO WRK-OUT-COMANDO-ATTR-QJTM
                    END-IF

                    MOVE WRK-INP-MENSA-QJTM TO WRK-OUT-MENSA-QJTM

               WHEN WRK-INP-PFK-QJTM     EQUAL '3' OR
                   (WRK-INP-PFK-QJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QJTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-QJTM     EQUAL '5' OR
                   (WRK-INP-PFK-QJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QJTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-QJTM     EQUAL 'A' OR
                   (WRK-INP-PFK-QJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QJTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-FASE-QJTM    EQUAL '1'
                    PERFORM 1201-TRATAR-FASE-1

               WHEN WRK-INP-FASE-QJTM    EQUAL '2'

                    PERFORM 1202-TRATAR-FASE-2

           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1201-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
               WHEN WRK-INP-PFK-QJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QJTM EQUAL SPACES
                    PERFORM 1250-TRATAR-ENTER

               WHEN WRK-INP-PFK-QJTM     EQUAL '7' OR
                   (WRK-INP-PFK-QJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QJTM EQUAL 'PFK07')
                    PERFORM 1270-TRATAR-PFK07


               WHEN WRK-INP-PFK-QJTM     EQUAL '8' OR
                   (WRK-INP-PFK-QJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QJTM EQUAL 'PFK08')
                    PERFORM 1280-TRATAR-PFK08

               WHEN OTHER
                    MOVE WRK-MSG-02      TO WRK-OUT-MENSA-QJTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1201-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1202-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
               WHEN WRK-INP-PFK-QJTM     EQUAL '.' AND

                    WRK-INP-COMANDO-QJTM EQUAL SPACES
                    MOVE '1'           TO WRK-OUT-FASE-QJTM
                    MOVE WRK-MSG-01    TO WRK-OUT-MENSA-QJTM

               WHEN WRK-INP-PFK-QJTM     EQUAL '6' OR
                   (WRK-INP-PFK-QJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QJTM EQUAL 'PFK06')
                    PERFORM
                    VARYING WRK-IND      FROM 1 BY 1
                      UNTIL WRK-IND      GREATER 3
                         OR WRK-INCONSIS EQUAL '*'
                         IF WRK-INP-SELEC-QJTM(WRK-IND) EQUAL 'E'
                            PERFORM 1252-ACESSAR-DCOM5335
                            MOVE '1'     TO WRK-OUT-FASE-QJTM
                         END-IF
                    END-PERFORM
                    IF  WRK-INCONSIS     EQUAL SPACES
                        PERFORM 1260-TRATAR-CONFIRMACAO
                    END-IF

               WHEN OTHER

                    MOVE WRK-MSG-02    TO WRK-OUT-MENSA-QJTM
                    MOVE WRK-225       TO WRK-OUT-SELEC-ATTR-QJTM(1)
                                          WRK-OUT-SELEC-ATTR-QJTM(2)
                                          WRK-OUT-SELEC-ATTR-QJTM(3)
                    MOVE WRK-49345     TO WRK-OUT-COMANDO-ATTR-QJTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1202-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA DEVOLVE A TELA INTEIRA PARA O USUARIO.                   *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-QJTM    TO WRK-OUT-SENHAS-QJTM
           MOVE WRK-INP-FIMAMOS-QJTM   TO WRK-OUT-FIMAMOS-QJTM
           MOVE WRK-INP-PAGINA-QJTM    TO WRK-OUT-PAGINA-QJTM

           MOVE WRK-INP-DADOS-FIXOS1-QJTM
                                       TO WRK-OUT-DADOS-FIXOS1-QJTM
           MOVE WRK-INP-DADOS-FIXOS2-QJTM
                                       TO WRK-OUT-DADOS-FIXOS2-QJTM.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                   UNTIL WRK-IND       GREATER 3
              IF  WRK-INP-PAR-QJTM(WRK-IND)
                                       EQUAL SPACES
                  MOVE WRK-225          TO
                                       WRK-OUT-SELEC-ATTR-QJTM (WRK-IND)
              ELSE
                  MOVE WRK-INP-SELEC-QJTM(WRK-IND) TO
                                       WRK-OUT-SELEC-QJTM (WRK-IND)
                  MOVE WRK-INP-PAR-QJTM(WRK-IND)
                                       TO WRK-OUT-PAR-QJTM     (WRK-IND)
                  MOVE WRK-INP-DTVENC-QJTM(WRK-IND)
                                       TO WRK-OUT-DTVENC-QJTM  (WRK-IND)
                  MOVE WRK-INP-VLPARC-QJTM(WRK-IND)
                                       TO WRK-OUT-VLPARC-QJTM  (WRK-IND)
                  MOVE WRK-INP-VLATUA-QJTM(WRK-IND)

                                       TO WRK-OUT-VLATUA-QJTM  (WRK-IND)
                  MOVE WRK-INP-VLBAIX-QJTM(WRK-IND)
                                       TO WRK-OUT-VLBAIX-QJTM  (WRK-IND)
                  MOVE WRK-INP-NRDOCT-QJTM(WRK-IND)
                                       TO WRK-OUT-NRDOCT-QJTM  (WRK-IND)
BSI001            MOVE WRK-INP-DETALHE-QJTM(WRK-IND)
BSI001                                 TO WRK-OUT-DETALHE-QJTM (WRK-IND)
                  ADD 1                TO WRK-QTDE-TELA
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

           EVALUATE WRK-INP-TPSELEC-QJTM
               WHEN '3'
                   MOVE 'DCOM1703'     TO WRK-TELA
                   MOVE WRK-INP-BCO-QJTM
                                       TO WRK-COMU-BANCO
                   MOVE WRK-INP-AGE-QJTM
                                       TO WRK-COMU-AGENCIA
               WHEN '4'
                   MOVE 'DCOM1704'     TO WRK-TELA
                   MOVE WRK-INP-BCO-QJTM
                                       TO WRK-COMU-BANCO
                   MOVE WRK-INP-AGE-QJTM
                                       TO WRK-COMU-AGENCIA
                   MOVE WRK-INP-CTA-QJTM
                                       TO WRK-COMU-CONTA

               WHEN '5'
                   MOVE 'DCOM1704'     TO WRK-TELA
                   STRING WRK-INP-CPFCNPJ-QJTM(1:3)

                          WRK-INP-CPFCNPJ-QJTM(5:3)
                          WRK-INP-CPFCNPJ-QJTM(9:3)
                   DELIMITED BY SIZE INTO WRK-COMU-CCNPJ-CPF
                   MOVE WRK-INP-FILIAL-QJTM
                                       TO WRK-COMU-CFLIAL-CNPJ
                   MOVE WRK-INP-CTRL-QJTM
                                       TO WRK-COMU-CCTRL-CNPJCPF
               WHEN OTHER
                   MOVE 'DCOM1702'     TO WRK-TELA

           END-EVALUATE.

           MOVE WRK-INP-OPCAO-QJTM     TO WRK-COMU-OPCAO-N
           MOVE WRK-INP-TMSTAMP-QJTM   TO WRK-COMU-TIMESTAMP
           MOVE WRK-INP-PAGINA1-QJTM   TO WRK-COMU-PAGINA-1
           MOVE WRK-INP-PAGINA2-QJTM   TO WRK-COMU-PAGINA-2
           MOVE WRK-INP-OPMENU-QJTM    TO WRK-COMU-OP-MENU
           MOVE WRK-INP-TPSELEC-QJTM   TO WRK-COMU-TP-SELECAO
           MOVE WRK-INP-MODBAIX-QJTM   TO WRK-COMU-MODAL-BAIXA
           MOVE WRK-INP-CDOPERA-QJTM   TO WRK-COMU-OPERACAO.
           MOVE WRK-OUT-MENSA-QJTM     TO WRK-COMU-MENSAGEM.

           MOVE 'DCOM1710'             TO WRK-COMU-TRANSACAO
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
           MOVE 'DCOM1710'              TO WRK-TELA
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
           MOVE 'DCOM1710'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE 'A'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       1250-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                TO ACU-SELECIONADOS-D
                                        ACU-SELECIONADOS-E
BSI002                                  ACU-SELECIONADOS-M.

           PERFORM VARYING WRK-IND   FROM 1 BY 1
                     UNTIL WRK-IND   GREATER 3
                        OR WRK-INP-PAR-QJTM(WRK-IND) EQUAL SPACES
                IF WRK-INP-SELEC-QJTM(WRK-IND)
                                     NOT EQUAL 'E' AND 'D' AND SPACES
BSI002                                                     AND 'M'
                   MOVE WRK-49353    TO WRK-OUT-SELEC-ATTR-QJTM(WRK-IND)
                   MOVE '*'          TO WRK-INCONSIS
               END-IF
               IF  WRK-INP-SELEC-QJTM(WRK-IND) EQUAL 'E'
                   ADD 1             TO ACU-SELECIONADOS-E
               END-IF

               IF  WRK-INP-SELEC-QJTM(WRK-IND) EQUAL 'D'
                   ADD 1             TO ACU-SELECIONADOS-D
               END-IF
               IF  WRK-INP-SELEC-QJTM(WRK-IND) EQUAL 'M'
                   ADD 1             TO ACU-SELECIONADOS-M
               END-IF
           END-PERFORM.

           IF  WRK-INCONSIS          EQUAL '*'
               MOVE WRK-MSG-03       TO WRK-OUT-MENSA-QJTM
               GO TO 1250-99-FIM
           END-IF.

           IF (ACU-SELECIONADOS-E      EQUAL ZEROS   AND
BSI002         ACU-SELECIONADOS-D      EQUAL ZEROS   AND
BSI002         ACU-SELECIONADOS-M      EQUAL ZEROS)  OR
              (ACU-SELECIONADOS-E      GREATER ZEROS AND
BSI002         ACU-SELECIONADOS-D      GREATER ZEROS AND
BSI002         ACU-SELECIONADOS-M      GREATER ZEROS)
               MOVE WRK-MSG-06         TO WRK-OUT-MENSA-QJTM
               MOVE '*'                TO WRK-INCONSIS

               PERFORM VARYING WRK-IND FROM 1 BY 1
                         UNTIL WRK-IND GREATER 3
                    OR WRK-INP-PAR-QJTM  (WRK-IND) EQUAL SPACES
                    IF WRK-INP-SELEC-QJTM(WRK-IND)
                                       NOT EQUAL SPACES AND LOW-VALUES
                       MOVE WRK-49353  TO
                                       WRK-OUT-SELEC-ATTR-QJTM(WRK-IND)
                    END-IF
               END-PERFORM
               GO TO 1250-99-FIM
           END-IF.

BSI002     IF (ACU-SELECIONADOS-E      GREATER ZEROS  AND
BSI002         ACU-SELECIONADOS-D      GREATER ZEROS) OR
BSI002        (ACU-SELECIONADOS-E      GREATER ZEROS  AND
BSI002         ACU-SELECIONADOS-M      GREATER ZEROS) OR
BSI002        (ACU-SELECIONADOS-D      GREATER ZEROS  AND
BSI002         ACU-SELECIONADOS-M      GREATER ZEROS)
BSI002         MOVE WRK-MSG-06         TO WRK-OUT-MENSA-QJTM
BSI002         MOVE '*'                TO WRK-INCONSIS
BSI002         PERFORM VARYING WRK-IND FROM 1 BY 1

BSI002                   UNTIL WRK-IND GREATER 3
BSI002              OR WRK-INP-PAR-QJTM  (WRK-IND) EQUAL SPACES
BSI002              IF WRK-INP-SELEC-QJTM(WRK-IND)
BSI002                                 NOT EQUAL SPACES AND LOW-VALUES
BSI002                 MOVE WRK-49353  TO
BSI002                                 WRK-OUT-SELEC-ATTR-QJTM(WRK-IND)
BSI002              END-IF
BSI002         END-PERFORM
BSI002         GO TO 1250-99-FIM
BSI002     END-IF.

           IF  ACU-SELECIONADOS-D      GREATER 1
               MOVE WRK-MSG-07         TO WRK-OUT-MENSA-QJTM
               MOVE '*'                TO WRK-INCONSIS
               PERFORM VARYING WRK-IND FROM 1 BY 1
                         UNTIL WRK-IND GREATER 3
                    OR WRK-INP-PAR-QJTM  (WRK-IND) EQUAL SPACES
                    IF WRK-INP-SELEC-QJTM(WRK-IND)
                                       NOT EQUAL SPACES AND LOW-VALUES
                       MOVE WRK-49353  TO
                                       WRK-OUT-SELEC-ATTR-QJTM(WRK-IND)

                    END-IF
               END-PERFORM
               GO TO 1250-99-FIM
           END-IF.

BSI002     IF  ACU-SELECIONADOS-M      GREATER 1
BSI002         MOVE WRK-MSG-09         TO WRK-OUT-MENSA-QJTM
BSI002         MOVE '*'                TO WRK-INCONSIS
BSI002         PERFORM VARYING WRK-IND FROM 1 BY 1
BSI002                   UNTIL WRK-IND GREATER 3
BSI002              OR WRK-INP-PAR-QJTM  (WRK-IND) EQUAL SPACES
BSI002              IF WRK-INP-SELEC-QJTM(WRK-IND)
BSI002                                 NOT EQUAL SPACES AND LOW-VALUES
BSI002                 MOVE WRK-49353  TO
BSI002                                 WRK-OUT-SELEC-ATTR-QJTM(WRK-IND)
BSI002              END-IF
BSI002         END-PERFORM
BSI002         GO TO 1250-99-FIM
BSI002     END-IF.

           IF  ACU-SELECIONADOS-E      GREATER 1

               PERFORM
               VARYING WRK-IND         FROM 1 BY 1
                 UNTIL WRK-IND         GREATER 3
                    OR WRK-INCONSIS    EQUAL '*'
                    OR WRK-INP-PAR-QJTM  (WRK-IND) EQUAL SPACES
                    IF WRK-INP-SELEC-QJTM(WRK-IND) EQUAL 'E'
                       PERFORM 1252-ACESSAR-DCOM5335
                    END-IF
               END-PERFORM

               PERFORM VARYING WRK-IND FROM 1 BY 1
                         UNTIL WRK-IND GREATER 3
                    OR WRK-INCONSIS    EQUAL '*'
                    OR WRK-INP-PAR-QJTM(WRK-IND) EQUAL SPACES
                    PERFORM 1261-ACESSAR-DCOM5309

                    IF WRK-INCONSIS    EQUAL '*'
                       MOVE WRK-49353  TO
                                       WRK-OUT-SELEC-ATTR-QJTM(WRK-IND)
                   END-IF
               END-PERFORM


               IF  WRK-INCONSIS        EQUAL '*'
                   GO TO 1250-99-FIM
               END-IF
           END-IF.

           IF  ACU-SELECIONADOS-D      EQUAL 1
               PERFORM 1251-DETALHAR-PARCELA
           ELSE
BSI002     IF  ACU-SELECIONADOS-M  EQUAL 1
BSI002         PERFORM 1253-DETALHAR-JUROS
BSI002     ELSE
               MOVE '2'                TO WRK-OUT-FASE-QJTM
               MOVE WRK-225            TO WRK-OUT-SELEC-ATTR-QJTM(1)
                                          WRK-OUT-SELEC-ATTR-QJTM(2)
                                          WRK-OUT-SELEC-ATTR-QJTM(3)
               MOVE WRK-49345          TO WRK-OUT-COMANDO-ATTR-QJTM
               MOVE WRK-MSG-04         TO WRK-OUT-MENSA-QJTM
BSI002     END-IF
           END-IF.


      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1251-DETALHAR-PARCELA           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-INP-OPCAO-QJTM     TO WRK-COMU-OPCAO-N.
           MOVE WRK-INP-TMSTAMP-QJTM   TO WRK-COMU-TIMESTAMP.
           MOVE WRK-INP-PAGINA1-QJTM   TO WRK-COMU-PAGINA-1.
           MOVE WRK-INP-PAGINA2-QJTM   TO WRK-COMU-PAGINA-2.
           MOVE WRK-INP-PAGINA-QJTM-N  TO WRK-COMU-PAGINA-3.
           MOVE WRK-INP-OPMENU-QJTM    TO WRK-COMU-OP-MENU.
           MOVE WRK-INP-TPSELEC-QJTM   TO WRK-COMU-TP-SELECAO.
           MOVE WRK-INP-MODBAIX-QJTM   TO WRK-COMU-MODAL-BAIXA.
           MOVE WRK-INP-CDOPERA-QJTM   TO WRK-COMU-OPERACAO.

           MOVE 'DCOM1710'             TO WRK-COMU-TRANSACAO

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 3
                IF WRK-INP-SELEC-QJTM     (WRK-IND) EQUAL 'D'
                   MOVE WRK-INP-PAR-QJTM-N(WRK-IND)
                                       TO WRK-COMU-PARCELA
                END-IF
           END-PERFORM.


           MOVE 'DCOM1730'             TO WRK-TELA.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1252-ACESSAR-DCOM5335           SECTION.

      *----------------------------------------------------------------*

           INITIALIZE 5335-ENTRADA
                      5335-SAIDA.

           MOVE WRK-INP-CDOPERA-QJTM   TO 5335E-OPERACAO.
           MOVE WRK-INP-PAR-QJTM-N    (WRK-IND)
                                       TO 5335E-NPCELA-DESC-COML.
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
                                       TO WRK-OUT-MENSA-QJTM
                     MOVE '*'          TO WRK-INCONSIS
                     MOVE WRK-49353    TO WRK-OUT-SELEC-ATTR-QJTM
                                         (WRK-IND)
           END-EVALUATE.

      *----------------------------------------------------------------*
       1252-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BSI003*----------------------------------------------------------------*
BSI003 1253-DETALHAR-JUROS             SECTION.
BSI003*----------------------------------------------------------------*
BSI003

BSI003     INITIALIZE WRK-COMU-AREA.
BSI003
BSI003     MOVE LENGTH                 OF WRK-COMU-AREA
BSI003                                 TO WRK-COMU-LL.
BSI003     MOVE WRK-CHNG               TO WRK-FUNCAO.
BSI003     MOVE 'DCOM3928'             TO WRK-TELA.
BSI003     MOVE 'DCOM1710'             TO WRK-COMU-TRANSACAO.
BSI003     MOVE WRK-INP-OPCAO-QJTM     TO WRK-COMU-OPCAO-N.
BSI003     MOVE WRK-INP-TMSTAMP-QJTM   TO WRK-COMU-TIMESTAMP.
BSI003     MOVE WRK-INP-DANO-OPER-QJTM TO WRK-COMU-DANO-OPER.
BSI003     MOVE WRK-INP-NSEQ-OPER-QJTM TO WRK-COMU-NSEQ-OPER.
BSI003
BSI003     PERFORM
BSI003     VARYING WRK-IND             FROM 1 BY 1
BSI003       UNTIL WRK-IND             GREATER 3
BSI003          IF WRK-INP-SELEC-QJTM (WRK-IND) EQUAL 'M'
BSI003             MOVE WRK-INP-PAR-QJTM-N
BSI003                                (WRK-IND) TO WRK-COMU-PARCELA
BSI003          END-IF
BSI003     END-PERFORM.
BSI003

BSI003     MOVE WRK-INP-BCO-QJTM       TO WRK-COMU-BANCO
BSI003     MOVE WRK-INP-AGE-QJTM       TO WRK-COMU-AGENCIA
BSI003     MOVE WRK-INP-CTA-QJTM       TO WRK-COMU-CONTA
BSI003     MOVE WRK-INP-CPFCNPJOC-QJTM TO WRK-COMU-CCNPJ-CPF
BSI003     MOVE WRK-INP-FILIAL-QJTM    TO WRK-COMU-CFLIAL-CNPJ
BSI003     MOVE WRK-INP-CTRL-QJTM      TO WRK-COMU-CCTRL-CNPJCPF
BSI003     MOVE WRK-INP-PAGINA1-QJTM   TO WRK-COMU-PAGINA-1
BSI003     MOVE WRK-INP-PAGINA2-QJTM   TO WRK-COMU-PAGINA-2
BSI003     MOVE WRK-INP-OPMENU-QJTM    TO WRK-COMU-OP-MENU.
BSI003     MOVE WRK-INP-TPSELEC-QJTM   TO WRK-COMU-TP-SELECAO.
BSI003     MOVE WRK-INP-MODBAIX-QJTM   TO WRK-COMU-MODAL-BAIXA.
BSI003     MOVE WRK-INP-PAGINA-QJTM-N  TO WRK-COMU-PAGINA-3.
BSI003     MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.
BSI003
BSI003*----------------------------------------------------------------*
BSI003 1253-99-FIM.                    EXIT.
BSI003*----------------------------------------------------------------*
BSI003
      *----------------------------------------------------------------*
       1260-TRATAR-CONFIRMACAO         SECTION.
      *----------------------------------------------------------------*


           PERFORM VARYING WRK-IND FROM 1 BY 1
                     UNTIL WRK-IND GREATER 3
                        OR WRK-INCONSIS EQUAL '*'
               IF  WRK-INP-SELEC-QJTM(WRK-IND) EQUAL 'E'
                   PERFORM 1261-ACESSAR-DCOM5309
                   IF  WRK-INCONSIS    EQUAL SPACES
                       ADD 1           TO WRK-QTDE-EXCLUIDOS
                   END-IF
               END-IF
           END-PERFORM.

           IF  WRK-INCONSIS            EQUAL '*'
               MOVE '1'                TO WRK-OUT-FASE-QJTM
               GO TO 1260-99-FIM
           END-IF.


           IF  WRK-QTDE-EXCLUIDOS         EQUAL WRK-QTDE-TELA
               IF  WRK-INP-PAGINA-QJTM-N  EQUAL 1
                   MOVE WRK-MSG-05        TO WRK-OUT-MENSA-QJTM

                   PERFORM 1220-RETORNAR-CHAMADOR
                   GO TO 1260-99-FIM
               ELSE
                   PERFORM 1270-TRATAR-PFK07
               END-IF
           ELSE
               COMPUTE WRK-RESTART     = (WRK-INP-PAGINA-QJTM-N - 1) * 3
               PERFORM 1120-ACESSAR-DCOM5310
           END-IF.

           IF  5310S-COD-RETORNO       EQUAL '0003'
           OR  WRK-INP-TPSELEC-QJTM    EQUAL  2
               MOVE WRK-MSG-05         TO WRK-OUT-MENSA-QJTM
               PERFORM 1220-RETORNAR-CHAMADOR
               GO TO 1260-99-FIM
           END-IF.

           MOVE '1'                    TO WRK-OUT-FASE-QJTM.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 1260-99-FIM

           END-IF.

           MOVE WRK-MSG-05             TO WRK-OUT-MENSA-QJTM.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1261-ACESSAR-DCOM5309           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5309-ENTRADA-ROTEADOR
                      5309-SAIDA-ROTEADOR
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE WRK-COD-USER-R         TO 5309E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 5309E-CTERM
           MOVE WRK-INP-DANO-OPER-QJTM TO 5309E-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-OPER-QJTM TO 5309E-NSEQ-OPER-DESC

           MOVE WRK-INP-PAR-QJTM-N(WRK-IND)
                                       TO 5309E-NPCELA-DESC-COML
           MOVE WRK-INP-CDPRODT-QJTM-N TO 5309E-CPRODT
           MOVE WRK-INP-CDSPROD-QJTM-N TO 5309E-CSPROD-DESC-COML
           MOVE WRK-COD-DEPTO-N        TO 5309E-AGENCIA-FUNC.

           IF  WRK-INP-FASE-QJTM       EQUAL '1'
               MOVE 'C'                TO 5309E-FLAG-FUNCAO
           ELSE
               MOVE 'I'                TO 5309E-FLAG-FUNCAO
           END-IF.

           MOVE 'DCOM5309'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 5309-ENTRADA-ROTEADOR
                                             5309-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.


           IF  5309S-COD-RETORNO       NOT EQUAL '0000'

               IF  5309S-COD-RETORNO   EQUAL '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               ELSE
                   MOVE 5309S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-QJTM
                                          WRK-COMU-MENSAGEM
                   MOVE '*'            TO WRK-INCONSIS
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1261-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1270-TRATAR-PFK07               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-QJTM-N   EQUAL  1
           OR  WRK-INP-TPSELEC-QJTM    EQUAL  2

               MOVE 'NAO EXISTEM MAIS PAGINAS PARA RETORNAR'
                                       TO WRK-OUT-MENSA-QJTM
               GO TO 1270-99-FIM
           END-IF.

           COMPUTE WRK-RESTART   = ( WRK-INP-PAGINA-QJTM-N - 2 ) * 2.

           PERFORM 1120-ACESSAR-DCOM5310.

           MOVE 'AMOSTRAGEM CONTINUA'  TO WRK-OUT-MENSA-QJTM.

           SUBTRACT 1                  FROM WRK-OUT-PAGINA-QJTM-N.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK08              *
      *----------------------------------------------------------------*
       1280-TRATAR-PFK08               SECTION.

      *----------------------------------------------------------------*

           IF  WRK-INP-FIMAMOS-QJTM    EQUAL '*'
           OR  WRK-INP-TPSELEC-QJTM    EQUAL  2
               MOVE 'NAO EXISTEM PAGINAS PARA AVANCAR'
                                       TO WRK-OUT-MENSA-QJTM
               GO TO 1280-99-FIM
           END-IF.

           COMPUTE WRK-RESTART   = ( WRK-INP-PAGINA-QJTM-N ) * 2

           PERFORM 1120-ACESSAR-DCOM5310.

           IF  WRK-OUT-FIMAMOS-QJTM     EQUAL '*'
               MOVE 'FIM DE AMOSTRAGEM' TO WRK-OUT-MENSA-QJTM
           ELSE
               MOVE 'AMOSTRAGEM CONTINUA'
                                        TO WRK-OUT-MENSA-QJTM
           END-IF.

           ADD 1                       TO WRK-OUT-PAGINA-QJTM-N.


      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1400-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT-QJTM
                                       TO WRK-OUT-LL-QJTM
                                          WRK-660-ZZ-MENSAGEM-QJTM.
           MOVE LENGTH                 OF WRK-660-DCOMQJTM
                                       TO WRK-660-LL-AREA-QJTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-QJTM
                                             WRK-660-DCOMQJTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS

               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-QJTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM1710'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM1710'         TO ERR-PGM
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
