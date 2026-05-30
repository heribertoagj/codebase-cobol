      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1713.
       AUTHOR.     DAVI BRAZ.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM1713                                    *
      *    PROGRAMADOR.:   DAVI BRAZ                                   *
      *    DATA........:   25/01/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTAR ESTORNO DA BAIXA DE PARCELAS -    *
      *                    PARC. CHEQUES                               *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMQMTM - CONSULTAR ESTORNO DA BAIXA DE PARCELAS -         *
      *               PARC. CHEQUES                                    *
      *----------------------------------------------------------------*

      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSG - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM1702          CHAMA: DCOM1714             *
      *                  DCOM1703                                      *
      *                  DCOM1704                                      *
      *================================================================*
DTS001*                  ALTERACAO - DTS CONSULTING                    *
DTS001*----------------------------------------------------------------*
DTS001*    PROGRAMADORA: LUCIANDRA SILVEIRA - DTS CONSULTING           *
DTS001*    ANALISTA DDS: VERA MARIA POLLINI - GRUPO 70                 *
DTS001*    DATA........: 29/10/2009                                    *
DTS001*----------------------------------------------------------------*
DTS001*    OBJETIVO....: CORRIGIR AREA DE COMUNICACAO DO DCOM5310      *

DTS001*================================================================*
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
BSI002*    DATA........:   28/07/2017                                  *
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
       77  FILLER                      PIC  X(050)         VALUE
           '*** DCOM1713 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGENS ***'.
      *----------------------------------------------------------------*

       77  WRK-MSG-01                  PIC  X(079)         VALUE
BSI002     'SELECIONE COM "D" P/ DETALHAR BAIXA, "C" P/CONSULTAR CHEQUES
BSI002-    ' OU "M" P/MORATORIA'.
       77  WRK-MSG-02                  PIC  X(079)         VALUE
           'PF INVALIDA'.
       77  WRK-MSG-05                  PIC  X(079)         VALUE
BSI002     'SELECIONE APENAS COM "D", "C" OU "M" E TECLE <ENTER>'.

       77  WRK-MSG-06                  PIC  X(079)         VALUE
           'SELECIONE APENAS UMA PARCELA COM "D" E TECLE <ENTER>'.
       77  WRK-MSG-07                  PIC  X(079)         VALUE
           'SELECIONE APENAS UMA PARCELA COM "C" E TECLE <ENTER>'.
BSI002 77  WRK-MSG-09                  PIC  X(079)         VALUE
BSI002     'SELECIONE APENAS UMA PARCELA COM "M" E TECLE <ENTER>'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *----------------------------------------------------------------*

      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       77  WRK-49353                   PIC S9(008) COMP    VALUE +49353.
      *--- ALPHA    - NORMAL    - DESPROTEGIDO - POS. CURSOR    ---*
       77  WRK-49345                   PIC S9(008) COMP    VALUE +49345.
      *--- PROTEGIDO
       77  WRK-225                     PIC S9(008) COMP    VALUE +225.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE

           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-SELEC-C                 PIC  9(003) COMP-3  VALUE ZEROS.
       77  ACU-SELEC-D                 PIC  9(003) COMP-3  VALUE ZEROS.
BSI002 77  ACU-SELEC-M                 PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
BSI002     05  WRK-IND                 PIC  9(002) COMP-3  VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.

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
           '*** TELA DCOMQMTM - INPUT ***'.
      *---------------------------------------------------------------*


       01  WRK-INPUT-QMTM.
           05 FILLER                   PIC  X(018)         VALUE SPACES.
           05 WRK-INP-DADOS-QMTM.
             10 WRK-INP-PFK-QMTM       PIC  X(001)         VALUE SPACES.
             10 WRK-INP-SENHAS-QMTM    PIC  X(037)         VALUE SPACES.
             10 WRK-INP-COMANDO-QMTM   PIC  X(068)         VALUE SPACES.
             10 WRK-INP-FIMAMOS-QMTM   PIC  X(001)         VALUE SPACES.
             10 WRK-INP-PAGINA-QMTM.
               15 WRK-INP-PAGINA-QMTM-N
                                       PIC  9(003)         VALUE ZEROS.
           05 WRK-INP-DADOS-FIXOS1-QMTM.
             10 WRK-INP-OCULTO-QMTM    PIC  X(079)         VALUE SPACES.
             10 REDEFINES              WRK-INP-OCULTO-QMTM.
               15 WRK-INP-OPCAO-QMTM   PIC  9(001).
               15 WRK-INP-TMSTAMP-QMTM PIC  X(026).
               15 WRK-INP-PAGINA1-QMTM PIC  9(004).
               15 WRK-INP-PAGINA2-QMTM PIC  9(004).
               15 WRK-INP-OPMENU-QMTM  PIC  9(001).
               15 WRK-INP-TPSELEC-QMTM PIC  9(001).
               15 WRK-INP-MODBAIX-QMTM PIC  X(001).

4S2511*        15 WRK-INP-CPFCNPJOC-QMTM
4S2511*                                PIC  9(009).
4S2511         15 WRK-INP-CPFCNPJOC-QMTM
4S2511                                 PIC  X(09).
             10 WRK-INP-NOME-QMTM      PIC  X(040)         VALUE SPACES.
             10 WRK-INP-CPFCNPJ-QMTM   PIC  X(019)         VALUE SPACES.
             10 REDEFINES WRK-INP-CPFCNPJ-QMTM.
               15 WRK-INP-CCNPJ-CPF-QMTM
4S2511*                                PIC  999.999.999.
4S2511                                 PIC  X(11).
               15 FILLER               PIC  X(001).
4S2511*              15 WRK-INP-FILIAL-QMTM  PIC  9(004).
4S2511               15 WRK-INP-FILIAL-QMTM  PIC  X(04).
               15 FILLER               PIC  X(001).
               15 WRK-INP-CTRL-QMTM    PIC  9(002).
             10 WRK-INP-BCAGCTA-QMTM   PIC  X(023)         VALUE SPACES.
             10 REDEFINES              WRK-INP-BCAGCTA-QMTM.
               15 WRK-INP-BCO-QMTM     PIC  9(003).
               15 FILLER               PIC  X(001).
               15 WRK-INP-AGE-QMTM     PIC  9(005).
               15 FILLER               PIC  X(001).
               15 WRK-INP-CTA-QMTM     PIC  9(013).
             10 WRK-INP-CDPRODT-QMTM.
               15 WRK-INP-CDPRODT-QMTM-N
                                       PIC  9(003)         VALUE ZEROS.

             10 WRK-INP-DSPRODT-QMTM   PIC  X(012)         VALUE SPACES.
             10 WRK-INP-CDSPROD-QMTM.
               15 WRK-INP-CDSPROD-QMTM-N
                                       PIC  9(003)         VALUE ZEROS.
             10 WRK-INP-DSUBPRD-QMTM   PIC  X(012)         VALUE SPACES.
             10 WRK-INP-CDOPERA-QMTM.
               15 WRK-INP-DANO-OPER-QMTM
                                       PIC  9(004)         VALUE ZEROS.
               15 WRK-INP-NSEQ-OPER-QMTM
                                       PIC  9(009)         VALUE ZEROS.
             10 WRK-INP-SITU1-QMTM     PIC  X(015)         VALUE SPACES.
             10 WRK-INP-SITU2-QMTM     PIC  X(013)         VALUE SPACES.
             10 WRK-INP-SITU3-QMTM.
               15 WRK-INP-SITU3-QMTM-N PIC  9(009)         VALUE ZEROS.
             10 WRK-INP-SITU4-QMTM     PIC  X(013)         VALUE SPACES.
             10 WRK-INP-SITU5-QMTM.
               15 WRK-INP-SITU5-QMTM-N PIC  9(009)         VALUE ZEROS.
           05 WRK-INP-DADOS-VAR-QMTM.
             10 WRK-INP-TABELA-QMTM    OCCURS 007  TIMES.
               15 WRK-INP-SELEC-QMTM   PIC  X(001)         VALUE SPACES.
               15 WRK-INP-PARC-QMTM.

                20 WRK-INP-PARC-QMTM-N PIC  9(005)         VALUE ZEROS.
               15 WRK-INP-QTDCHQ-QMTM  PIC  X(004)         VALUE SPACES.
               15 WRK-INP-DTVENC-QMTM  PIC  X(010)         VALUE SPACES.
               15 WRK-INP-VLPARC-QMTM  PIC  X(014)         VALUE SPACES.
               15 WRK-INP-VLATUA-QMTM  PIC  X(014)         VALUE SPACES.
               15 WRK-INP-VLBAIX-QMTM  PIC  X(014)         VALUE SPACES.
           05 WRK-INP-DADOS-FIXOS2-QMTM.
             10 WRK-INP-TOTPRCB-QMTM   PIC  X(005)         VALUE SPACES.
             10 WRK-INP-VALORBX-QMTM   PIC  X(018)         VALUE SPACES.
           05 WRK-INP-MENSA-QMTM       PIC  X(079)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQMTM - OUTPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-OUTPUT-QMTM.
           05 WRK-OUT-LL-QMTM          PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-OUT-ZZ-QMTM          PIC  9(004) COMP    VALUE ZEROS.
           05 WRK-OUT-DADOS-QMTM.
             10 WRK-OUT-SENHAS-QMTM    PIC  X(037)         VALUE SPACES.

             10 WRK-OUT-COMANDO-ATTR-QMTM
                                       PIC  9(004) COMP    VALUE ZEROS.
             10 WRK-OUT-COMANDO-QMTM   PIC  X(068)         VALUE SPACES.
             10 WRK-OUT-FIMAMOS-QMTM   PIC  X(001)         VALUE SPACES.
             10 WRK-OUT-PAGINA-QMTM.
              15 WRK-OUT-PAGINA-QMTM-N PIC  999            VALUE ZEROS.
            05 WRK-OUT-DADOS-FIXOS1-QMTM.
             10 WRK-OUT-OCULTO-QMTM    PIC  X(079)         VALUE SPACES.
             10 REDEFINES              WRK-OUT-OCULTO-QMTM.
               15 WRK-OUT-OPCAO-QMTM   PIC  9(001).
               15 WRK-OUT-TMSTAMP-QMTM PIC  X(026).
               15 WRK-OUT-PAGINA1-QMTM PIC  9(004).
               15 WRK-OUT-PAGINA2-QMTM PIC  9(004).
               15 WRK-OUT-OPMENU-QMTM  PIC  9(001).
               15 WRK-OUT-TPSELEC-QMTM PIC  9(001).
               15 WRK-OUT-MODBAIX-QMTM PIC  X(001).
4S2511*        15 WRK-OUT-CPFCNPJOC-QMTM
4S2511*                                PIC  9(009).
4S2511         15 WRK-OUT-CPFCNPJOC-QMTM
4S2511                                 PIC  X(09).
             10 WRK-OUT-NOME-QMTM      PIC  X(040)         VALUE SPACES.
             10 WRK-OUT-CPFCNPJ-QMTM   PIC  X(019)         VALUE SPACES.
             10 REDEFINES WRK-OUT-CPFCNPJ-QMTM.

               15 WRK-OUT-CCNPJ-CPF-QMTM
4S2511*                                PIC  999.999.999.
4S2511                                 PIC  X(11).
               15 FILLER               PIC  X(001).
4S2511*              15 WRK-OUT-FILIAL-QMTM  PIC  9(004).
4S2511               15 WRK-OUT-FILIAL-QMTM  PIC  X(04).
               15 FILLER               PIC  X(001).
               15 WRK-OUT-CTRL-QMTM    PIC  9(002).
             10 WRK-OUT-BCAGCTA-QMTM   PIC  X(023)         VALUE SPACES.
             10 REDEFINES              WRK-OUT-BCAGCTA-QMTM.
               15 WRK-OUT-BCO-QMTM     PIC  9(003).
               15 FILLER               PIC  X(001).
               15 WRK-OUT-AGE-QMTM     PIC  9(005).
               15 FILLER               PIC  X(001).
               15 WRK-OUT-CTA-QMTM     PIC  9(013).
             10 WRK-OUT-CDPRODT-QMTM   PIC  X(003)         VALUE SPACES.
             10 WRK-OUT-DSPRODT-QMTM   PIC  X(012)         VALUE SPACES.
             10 WRK-OUT-CDSPROD-QMTM   PIC  X(003)         VALUE SPACES.
             10 WRK-OUT-DSUBPRD-QMTM   PIC  X(012)         VALUE SPACES.
             10 WRK-OUT-CDOPERA-QMTM.
                15 WRK-OUT-DANO-OPER-QMTM
                                       PIC  9(004)         VALUE ZEROS.
                15 WRK-OUT-NSEQ-OPER-QMTM

                                       PIC  9(009)         VALUE ZEROS.
             10 WRK-OUT-SITU1-QMTM     PIC  X(015)         VALUE SPACES.
             10 WRK-OUT-SITU2-QMTM     PIC  X(013)         VALUE SPACES.
             10 WRK-OUT-SITU3-QMTM.
              15 WRK-OUT-SITU3-QMTM-N  PIC  9(009)         VALUE ZEROS.
             10 WRK-OUT-SITU4-QMTM     PIC  X(013)         VALUE SPACES.
             10 WRK-OUT-SITU5-QMTM.
              15 WRK-OUT-SITU5-QMTM-N  PIC  9(009)         VALUE ZEROS.
            05 WRK-OUT-DADOS-VAR-QMTM.
             10 WRK-OUT-TABELA-QMTM    OCCURS 007  TIMES.
                15 WRK-OUT-SELEC-ATTR-QMTM
                                       PIC  9(004) COMP    VALUE ZEROS.
                15 WRK-OUT-SELEC-QMTM  PIC  X(001)         VALUE SPACES.
                15 WRK-OUT-PARC-QMTM.
                 20 WRK-OUT-PARC-QMTM-N
                                       PIC  99999          VALUE ZEROS.
                15 WRK-OUT-QTDCHQ-QMTM.
                 20 WRK-OUT-QTDCHQ-QMTM-N
                                       PIC  9(004)         VALUE ZEROS.
                15 WRK-OUT-DTVENC-QMTM PIC  X(010)         VALUE SPACES.
                15 WRK-OUT-VLPARC-QMTM.

                   20 WRK-OUT-VLPARC-QMTM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
                15 WRK-OUT-VLATUA-QMTM.
                   20 WRK-OUT-VLATUA-QMTM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
                15 WRK-OUT-VLBAIX-QMTM.
                   20 WRK-OUT-VLBAIX-QMTM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05 WRK-OUT-DADOS-FIXOS2-QMTM.
             10 WRK-OUT-TOTPRCB-QMTM   PIC  X(005)         VALUE SPACES.
             10 WRK-OUT-VALORBX-QMTM.
               15 WRK-OUT-VALORBX-QMTM-N
                                       PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
           05 WRK-OUT-MENSA-QMTM       PIC  X(079)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                         PIC X(070)       VALUE
           '*** TELA DCOMQMTM - BRAD0660 - SEG. 00 ***'.
      *---------------------------------------------------------------*


       01  WRK-660-DCOMQMTM.
           05 WRK-660-LL-AREA-QMTM     PIC  9(004) COMP    VALUE 0000.
           05 WRK-660-LL-MENSAGEM-QMTM PIC  9(004) COMP    VALUE 0000.
           05 WRK-660-SENHAS-QMTM      PIC  9(004) COMP    VALUE 0037.
           05 WRK-660-COMANDO-QMTM     PIC  9(004) COMP    VALUE 2070.
           05 WRK-660-FIMAMOS-QMTM     PIC  9(004) COMP    VALUE 0001.
           05 WRK-660-PAGINA-QMTM      PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-OCULTO-QMTM      PIC  9(004) COMP    VALUE 0079.
           05 WRK-660-NOME-QMTM        PIC  9(004) COMP    VALUE 0040.
           05 WRK-660-CPFCNPJ-QMTM     PIC  9(004) COMP    VALUE 0019.
           05 WRK-660-BCAGCTA-QMTM     PIC  9(004) COMP    VALUE 0023.
           05 WRK-660-CDPRODT-QMTM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-DSPRODT-QMTM     PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-CDSPROD-QMTM     PIC  9(004) COMP    VALUE 0003.
           05 WRK-660-DSUBPRD-QMTM     PIC  9(004) COMP    VALUE 0012.
           05 WRK-660-CDOPERA-QMTM     PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-SITU1-QMTM       PIC  9(004) COMP    VALUE 0015.
           05 WRK-660-SITU2-QMTM       PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-SITU3-QMTM       PIC  9(004) COMP    VALUE 0009.
           05 WRK-660-SITU4-QMTM       PIC  9(004) COMP    VALUE 0013.
           05 WRK-660-SITU5-QMTM       PIC  9(004) COMP    VALUE 0009.

           05 WRK-660-OCORRENCIAS      OCCURS 007 TIMES.
             10 WRK-660-SELEC-QMTM     PIC  9(004) COMP    VALUE 2003.
             10 WRK-660-PAR-QMTM       PIC  9(004) COMP    VALUE 0005.
             10 WRK-660-QTDCHQ-QMTM    PIC  9(004) COMP    VALUE 0004.
             10 WRK-660-DTVENC-QMTM    PIC  9(004) COMP    VALUE 0010.
             10 WRK-660-VLPARC-QMTM    PIC  9(004) COMP    VALUE 0014.
             10 WRK-660-VLATUA-QMTM    PIC  9(004) COMP    VALUE 0014.
             10 WRK-660-VLBAIX-QMTM    PIC  9(004) COMP    VALUE 0014.
           05 WRK-660-TOTPRCB-QMTM     PIC  9(004) COMP    VALUE 0005.
           05 WRK-660-VALORBX-QMTM     PIC  9(004) COMP    VALUE 0018.
           05  WRK-660-MENSA-QMTM      PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE COMUNICACAO ENTRE PROGRAMAS IMS'.
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
             10 WRK-COMU-DADOS-2.
               15 WRK-COMU-VENCIMENTO  PIC  X(010)         VALUE SPACES.
BSI003         15 WRK-FILLER           PIC  X(003)         VALUE SPACES.
             10 FILLER REDEFINES       WRK-COMU-DADOS-2.
               15 WRK-COMU-PAGINA-3    PIC  9(004).
BSI002         15 WRK-COMU-INCL-EXCL   PIC  X(001).
BSI002         15 WRK-COMU-CHAMADOR    PIC  X(008).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM DCOM5310 ***'.
      *----------------------------------------------------------------*

       01  WRK-5310-ENTRADA.
           05 5310E-COD-RETORNO          PIC  X(004)       VALUE SPACES.

           05 5310E-MSG-RETORNO          PIC  X(079)       VALUE SPACES.
           05 5310E-RESTART              PIC  9(005)       VALUE ZEROS.
           05 5310E-FLAG                 PIC  X(001)       VALUE SPACES.
           05 5310E-START-COUNT          PIC  X(001)       VALUE SPACES.
           05 5310E-CFUNC-BDSCO          PIC  9(009)       VALUE ZEROS.
           05 5310E-CTERM                PIC  X(008)       VALUE SPACES.
DTS001     05 WRK-5310E-AGENCIA-FUNC     PIC  9(005)       VALUE ZEROS.
DTS001     05 WRK-5310E-CPRODT           PIC  9(003)       VALUE ZEROS.
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
             10 5310S-NPCELA-DESC-COML   PIC  9(004)       VALUE ZEROS.
             10 5310S-DVCTO-PCELA-DESC   PIC  X(010)       VALUE SPACES.
             10 5310S-VPRINC-PCELA-DESC  PIC  9(015)V99    VALUE ZEROS.
             10 5310S-VATUAL-PCELA-DESC  PIC  9(015)V99    VALUE ZEROS.
             10 5310S-VEFETV-BAIXA-PCELA PIC  9(015)V99    VALUE ZEROS.
             10 5310S-CIND-BAIXA-COBR    PIC  X(001)       VALUE SPACES.
             10 5310S-CTITLO-COBR-BCO    PIC  X(012)       VALUE SPACES.
             10 5310S-CSEU-NRO-TITLO     PIC  X(015)       VALUE SPACES.
4S2511*      10 5310S-CCTRL-CNPJ-CPF     PIC  9(009)       VALUE ZEROS.
4S2511       10 5310S-CCTRL-CNPJ-CPF     PIC  X(009)       VALUE SPACES.
             10 5310S-IPSSOA-SACDO       PIC  X(040)       VALUE SPACES.
             10 5310S-QTDE-CHEQUES       PIC  9(009)       VALUE ZEROS.
             10 5310S-CNPJ               PIC  9(015)       VALUE ZEROS.
             10 FILLER REDEFINES         5310S-CNPJ.
4S2511*          15 5310S-CNPJ-RAIZ        PIC  9(009).
4S2511           15 5310S-CNPJ-RAIZ        PIC  X(09).
4S2511*          15 5310S-CNPJ-FLIAL       PIC  9(004).
4S2511           15 5310S-CNPJ-FLIAL       PIC  X(04).
               15 5310S-CNPJ-CTRL        PIC  9(002).
             10 5310S-NOSSO-NRO          PIC  9(018)       VALUE ZEROS.
BSI001       10 WRK-5310S-CADM-CATAO-CREDT
BSI001                                   PIC  9(009)       VALUE ZEROS.
BSI001       10 WRK-5310S-IADM-CATAO-CREDT

BSI001                                   PIC  X(025)       VALUE SPACES.
BSI001       10 WRK-5310S-CINDCD-VDA-RCBVL
BSI001                                   PIC  X(001)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM1713 - FIM DA AREA DE WORKING ***'.

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

           INITIALIZE WRK-5310-ENTRADA
                      5310-SAIDA-ROTEADOR
                      4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR
                      WRK-COMU-AREA
                      WRK-AUXILIARES.

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

           MOVE 'DCOMQMTM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1713'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMQMTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMQMTM'
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

           MOVE WRK-COMU-SENHAS        TO WRK-INP-SENHAS-QMTM
                                          WRK-OUT-SENHAS-QMTM.
           MOVE WRK-COMU-OPCAO-N       TO WRK-INP-OPCAO-QMTM
                                          WRK-OUT-OPCAO-QMTM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TMSTAMP-QMTM
                                          WRK-OUT-TMSTAMP-QMTM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGINA1-QMTM
                                          WRK-OUT-PAGINA1-QMTM.
           MOVE WRK-COMU-PAGINA-2      TO WRK-INP-PAGINA2-QMTM
                                          WRK-OUT-PAGINA2-QMTM.
           MOVE WRK-COMU-OP-MENU       TO WRK-INP-OPMENU-QMTM
                                          WRK-OUT-OPMENU-QMTM.

           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPSELEC-QMTM
                                          WRK-OUT-TPSELEC-QMTM.
           MOVE WRK-COMU-MODAL-BAIXA   TO WRK-INP-MODBAIX-QMTM
                                          WRK-OUT-MODBAIX-QMTM.
           MOVE WRK-COMU-OPERACAO      TO WRK-INP-CDOPERA-QMTM
                                          WRK-OUT-CDOPERA-QMTM.
           MOVE WRK-COMU-CCNPJ-CPF     TO WRK-INP-CCNPJ-CPF-QMTM
                                          WRK-OUT-CCNPJ-CPF-QMTM
BSI002                                    WRK-INP-CPFCNPJOC-QMTM
BSI002                                    WRK-OUT-CPFCNPJOC-QMTM
           MOVE WRK-COMU-CFLIAL-CNPJ   TO WRK-INP-FILIAL-QMTM
                                          WRK-OUT-FILIAL-QMTM
           MOVE WRK-COMU-CCTRL-CNPJCPF TO WRK-INP-CTRL-QMTM
                                          WRK-OUT-CTRL-QMTM
           MOVE WRK-COMU-BANCO         TO WRK-INP-BCO-QMTM
                                          WRK-OUT-BCO-QMTM
           MOVE WRK-COMU-AGENCIA       TO WRK-INP-AGE-QMTM
                                          WRK-OUT-AGE-QMTM
           MOVE WRK-COMU-CONTA         TO WRK-INP-CTA-QMTM
                                          WRK-OUT-CTA-QMTM


           PERFORM 1110-ACESSAR-DCOM4174.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 1100-99-FIM
           END-IF.

           IF  WRK-TRANSACAO           EQUAL 'DCOM1714' OR 'DCOM1730'
               MOVE WRK-COMU-PAGINA-2  TO WRK-OUT-PAGINA-QMTM-N
               COMPUTE 5310E-RESTART    = (WRK-COMU-PAGINA-2 - 1 ) * 7
           ELSE
               MOVE 1                  TO WRK-OUT-PAGINA-QMTM-N
           END-IF.

           PERFORM 1120-ACESSAR-DCOM5310.

           IF  WRK-INCONSIS            EQUAL SPACES
               IF (WRK-TRANSACAO       EQUAL 'DCOM1714'
                                          OR 'DCOM1730') AND
                   WRK-COMU-MENSAGEM   NOT EQUAL SPACES
                   MOVE WRK-COMU-MENSAGEM
                                       TO WRK-OUT-MENSA-QMTM

               ELSE
                   MOVE WRK-MSG-01     TO WRK-OUT-MENSA-QMTM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA O CABECALHO DA TELA.                      *
      *----------------------------------------------------------------*
       1110-ACESSAR-DCOM4174           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-DANO-OPER-QMTM TO 4174-ENT-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-OPER-QMTM TO 4174-ENT-NSEQ-OPER-DESC
           MOVE ZEROS                  TO 4174-ENT-RESTART.
           MOVE 'N'                    TO 4174-ENT-FLAG.
           MOVE WRK-COD-USER-R         TO 4174-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM.

           MOVE 'DCOM4174'             TO WRK-MODULO

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO
               WHEN '0000'
                    PERFORM 1111-FORMATAR-CABECALHO

               WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE 4174-SAI-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                    MOVE '*'           TO WRK-INCONSIS
                    PERFORM 1220-RETORNAR-CHAMADOR
           END-EVALUATE.


      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PREENCHE A TELA COM OS CAMPOS DO MODULO.          *
      *----------------------------------------------------------------*
       1111-FORMATAR-CABECALHO         SECTION.
      *----------------------------------------------------------------*

           MOVE 4174-SAI-IPSSOA-DESC-COML
                                       TO WRK-OUT-NOME-QMTM
           STRING 4174-SAI-CBCO
           '/'    4174-SAI-CAG-BCRIA
           '/'    4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE         INTO WRK-OUT-BCAGCTA-QMTM

           STRING 4174-SAI-CCNPJ-CPF(1:3)
           '.'    4174-SAI-CCNPJ-CPF(4:3)
           '.'    4174-SAI-CCNPJ-CPF(7:3)

           '/'    4174-SAI-CFLIAL-CNPJ
           '-'    4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE         INTO WRK-OUT-CPFCNPJ-QMTM.

           MOVE 4174-SAI-CPRODT        TO WRK-OUT-CDPRODT-QMTM
           MOVE 4174-SAI-IABREV-PRODT  TO WRK-OUT-DSPRODT-QMTM
           MOVE 4174-SAI-CSPROD-DESC-COML
                                       TO WRK-OUT-CDSPROD-QMTM
           MOVE 4174-SAI-IRSUMO-SPROD-DESC
                                       TO WRK-OUT-DSUBPRD-QMTM
           MOVE 4174-SAI-IRSUMO-SIT-DESC
                                       TO WRK-OUT-SITU1-QMTM
           MOVE SPACES                 TO WRK-OUT-SITU3-QMTM
                                          WRK-OUT-SITU5-QMTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC
                                       TO WRK-OUT-SITU1-QMTM

           EVALUATE TRUE
               WHEN 4174-SAI-CCONTR-LIM-DESC      GREATER ZEROS
                    MOVE 'CONTR LIMITE:'          TO WRK-OUT-SITU2-QMTM
                    MOVE 4174-SAI-CCONTR-LIM-DESC TO WRK-OUT-SITU3-QMTM


               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 8
                    MOVE 'CONV GERAL..:'          TO WRK-OUT-SITU2-QMTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITU3-QMTM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 9
                    MOVE 'CONV CLIENTE:'          TO WRK-OUT-SITU2-QMTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITU3-QMTM
           END-EVALUATE.

           IF  4174-SAI-NSMULA-OPER-DESC GREATER  ZEROS
               IF  WRK-OUT-SITU2-QMTM    EQUAL SPACES OR LOW-VALUES
                   MOVE 'SIMULACAO...:'  TO WRK-OUT-SITU2-QMTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                         TO WRK-OUT-SITU3-QMTM
               ELSE
                   MOVE 'SIMULACAO...:'  TO WRK-OUT-SITU4-QMTM

                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                         TO WRK-OUT-SITU5-QMTM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA DADOS NA TELA.                            *
      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM5310           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER-R         TO 5310E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 5310E-CTERM
DTS001     MOVE WRK-COD-DEPTO-N        TO WRK-5310E-AGENCIA-FUNC.
           MOVE WRK-INP-DANO-OPER-QMTM TO 5310E-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-OPER-QMTM TO 5310E-NSEQ-OPER-DESC
           MOVE 'DCOM5310'             TO WRK-MODULO.


           CALL WRK-MODULO             USING WRK-5310-ENTRADA
                                             5310-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 5310S-COD-RETORNO
               WHEN '0000'
                     PERFORM 1121-FORMATAR-TELA
                     IF  5310S-NPCELA-DESC-COML(8) EQUAL ZEROS
                         MOVE '*'      TO WRK-OUT-FIMAMOS-QMTM
                     ELSE
                         MOVE SPACES   TO WRK-OUT-FIMAMOS-QMTM
                     END-IF

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE 5310S-MSG-RETORNO

                                       TO WRK-COMU-MENSAGEM
                    MOVE '*'           TO WRK-INCONSIS
                    PERFORM 1220-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1121-FORMATAR-TELA              SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 7
               IF  5310S-NPCELA-DESC-COML(WRK-IND) EQUAL ZEROS
                   MOVE SPACES         TO WRK-OUT-TABELA-QMTM  (WRK-IND)
                   MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-QMTM
                                         (WRK-IND)
               ELSE

                   MOVE SPACES         TO WRK-OUT-SELEC-QMTM   (WRK-IND)
                   MOVE 5310S-NPCELA-DESC-COML                 (WRK-IND)
                                       TO WRK-OUT-PARC-QMTM-N  (WRK-IND)
                   MOVE 5310S-QTDE-CHEQUES                     (WRK-IND)
                                       TO WRK-OUT-QTDCHQ-QMTM-N(WRK-IND)
                   MOVE 5310S-DVCTO-PCELA-DESC                 (WRK-IND)
                                       TO WRK-OUT-DTVENC-QMTM  (WRK-IND)
                   INSPECT WRK-OUT-DTVENC-QMTM                 (WRK-IND)
                                       REPLACING ALL '.' BY '/'
                   MOVE 5310S-VPRINC-PCELA-DESC                (WRK-IND)
                                       TO WRK-OUT-VLPARC-QMTM-N(WRK-IND)
                   MOVE 5310S-VATUAL-PCELA-DESC                (WRK-IND)
                                       TO WRK-OUT-VLATUA-QMTM-N(WRK-IND)
                   MOVE 5310S-VEFETV-BAIXA-PCELA               (WRK-IND)
                                       TO WRK-OUT-VLBAIX-QMTM-N(WRK-IND)
               END-IF
           END-PERFORM.

           MOVE 5310S-QTDE-BAIXA       TO WRK-OUT-TOTPRCB-QMTM.
           MOVE 5310S-VBAIXA-TOTAL     TO WRK-OUT-VALORBX-QMTM-N.


      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMQMTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-QMTM.

           PERFORM 1210-DEVOLVER-TELA.

           EVALUATE TRUE

               WHEN WRK-INP-PFK-QMTM     EQUAL 'H' OR
                   (WRK-INP-PFK-QMTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QMTM EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-QMTM TO WRK-OUT-MENSA-QMTM

               WHEN WRK-INP-PFK-QMTM     EQUAL '3' OR
                   (WRK-INP-PFK-QMTM     EQUAL '.' AND

                    WRK-INP-COMANDO-QMTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-QMTM     EQUAL '5' OR
                   (WRK-INP-PFK-QMTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QMTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-QMTM     EQUAL 'A' OR
                   (WRK-INP-PFK-QMTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QMTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-QMTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QMTM EQUAL SPACES
                    PERFORM 1250-TRATAR-ENTER

               WHEN WRK-INP-PFK-QMTM     EQUAL '7' OR
                   (WRK-INP-PFK-QMTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QMTM EQUAL 'PFK7')
                    PERFORM 1270-TRATAR-PFK07


               WHEN WRK-INP-PFK-QMTM     EQUAL '8' OR
                   (WRK-INP-PFK-QMTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QMTM EQUAL 'PFK8')
                    PERFORM 1280-TRATAR-PFK08

              WHEN OTHER
                   MOVE WRK-MSG-02     TO WRK-OUT-MENSA-QMTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-QMTM    TO WRK-OUT-SENHAS-QMTM.
           MOVE WRK-INP-FIMAMOS-QMTM   TO WRK-OUT-FIMAMOS-QMTM.

           MOVE WRK-INP-PAGINA-QMTM    TO WRK-OUT-PAGINA-QMTM.
           MOVE WRK-INP-DADOS-FIXOS1-QMTM
                                       TO WRK-OUT-DADOS-FIXOS1-QMTM.
           MOVE WRK-INP-DADOS-FIXOS2-QMTM
                                       TO WRK-OUT-DADOS-FIXOS2-QMTM.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 7
               IF  WRK-INP-PARC-QMTM(WRK-IND) EQUAL SPACES
                   MOVE SPACES     TO WRK-OUT-TABELA-QMTM(WRK-IND)
                   MOVE WRK-225    TO WRK-OUT-SELEC-ATTR-QMTM(WRK-IND)
               ELSE
                   MOVE WRK-INP-SELEC-QMTM(WRK-IND)
                                   TO WRK-OUT-SELEC-QMTM(WRK-IND)
                   MOVE WRK-INP-PARC-QMTM(WRK-IND)
                                   TO WRK-OUT-PARC-QMTM(WRK-IND)
                   MOVE WRK-INP-QTDCHQ-QMTM(WRK-IND)
                                   TO WRK-OUT-QTDCHQ-QMTM(WRK-IND)
                   MOVE WRK-INP-QTDCHQ-QMTM(WRK-IND)
                                   TO WRK-OUT-QTDCHQ-QMTM(WRK-IND)
                   MOVE WRK-INP-DTVENC-QMTM(WRK-IND)

                                   TO WRK-OUT-DTVENC-QMTM(WRK-IND)
                   MOVE WRK-INP-VLPARC-QMTM(WRK-IND)
                                   TO WRK-OUT-VLPARC-QMTM(WRK-IND)
                   MOVE WRK-INP-VLATUA-QMTM(WRK-IND)
                                   TO WRK-OUT-VLATUA-QMTM(WRK-IND)
                   MOVE WRK-INP-VLBAIX-QMTM(WRK-IND)
                                   TO WRK-OUT-VLBAIX-QMTM(WRK-IND)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.


           EVALUATE WRK-INP-TPSELEC-QMTM
               WHEN '3'
                   MOVE 'DCOM1703'     TO WRK-TELA
                   MOVE WRK-INP-BCO-QMTM
                                       TO WRK-COMU-BANCO
                   MOVE WRK-INP-AGE-QMTM
                                       TO WRK-COMU-AGENCIA

               WHEN '4'
                   MOVE 'DCOM1704'     TO WRK-TELA
                   MOVE WRK-INP-BCO-QMTM
                                       TO WRK-COMU-BANCO
                   MOVE WRK-INP-AGE-QMTM
                                       TO WRK-COMU-AGENCIA
                   MOVE WRK-INP-CTA-QMTM
                                       TO WRK-COMU-CONTA

               WHEN '5'
                   MOVE 'DCOM1704'     TO WRK-TELA
                   STRING WRK-INP-CPFCNPJ-QMTM(1:3)
                          WRK-INP-CPFCNPJ-QMTM(5:3)

                          WRK-INP-CPFCNPJ-QMTM(9:3)
                   DELIMITED BY SIZE INTO WRK-COMU-CCNPJ-CPF
                   MOVE WRK-INP-FILIAL-QMTM
                                       TO WRK-COMU-CFLIAL-CNPJ
                   MOVE WRK-INP-CTRL-QMTM
                                       TO WRK-COMU-CCTRL-CNPJCPF
               WHEN OTHER
                   MOVE 'DCOM1702'     TO WRK-TELA

           END-EVALUATE.

           MOVE WRK-INP-OPCAO-QMTM     TO WRK-COMU-OPCAO-N
           MOVE WRK-INP-TMSTAMP-QMTM   TO WRK-COMU-TIMESTAMP
           MOVE WRK-INP-PAGINA1-QMTM   TO WRK-COMU-PAGINA-1
           MOVE WRK-INP-PAGINA2-QMTM   TO WRK-COMU-PAGINA-2
           MOVE WRK-INP-OPMENU-QMTM    TO WRK-COMU-OP-MENU
           MOVE WRK-INP-TPSELEC-QMTM   TO WRK-COMU-TP-SELECAO
           MOVE WRK-INP-MODBAIX-QMTM   TO WRK-COMU-MODAL-BAIXA
           MOVE WRK-INP-CDOPERA-QMTM   TO WRK-COMU-OPERACAO.
           MOVE WRK-INP-PFK-QMTM       TO WRK-COMU-PFK
           MOVE WRK-CHNG               TO WRK-FUNCAO.

           MOVE 'DCOM1713'             TO WRK-COMU-TRANSACAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-COMU-AREA
                                       TO WRK-COMU-LL
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1713'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE 5                      TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-COMU-AREA
                                       TO WRK-COMU-LL
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1713'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE 'A'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1250-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*


           MOVE ZEROS                  TO ACU-SELEC-D
                                          ACU-SELEC-C
BSI002                                    ACU-SELEC-M.
           MOVE SPACES                 TO WRK-INCONSIS.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 7
                        OR WRK-INP-PARC-QMTM(WRK-IND) EQUAL SPACES
                 IF WRK-INP-SELEC-QMTM      (WRK-IND)
                                       NOT EQUAL ' ' AND 'D' AND 'C'
BSI002                                                       AND 'M'
                    MOVE WRK-MSG-05    TO WRK-OUT-MENSA-QMTM
                    MOVE '*'           TO WRK-INCONSIS
                    MOVE WRK-49353     TO WRK-OUT-SELEC-ATTR-QMTM
                                         (WRK-IND)
               ELSE
                   IF  WRK-INP-SELEC-QMTM(WRK-IND) EQUAL 'D'
                       ADD 1           TO ACU-SELEC-D
                   END-IF
                   IF  WRK-INP-SELEC-QMTM(WRK-IND) EQUAL 'C'

                       ADD 1           TO ACU-SELEC-C
                   END-IF
BSI002             IF  WRK-INP-SELEC-QMTM(WRK-IND) EQUAL 'M'
BSI002                 ADD 1           TO ACU-SELEC-M
BSI002             END-IF
               END-IF
           END-PERFORM.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 1250-99-FIM
           END-IF.

           IF  ACU-SELEC-D             EQUAL ZEROS AND
               ACU-SELEC-C             EQUAL ZEROS AND
BSI002         ACU-SELEC-M             EQUAL ZEROS
               MOVE WRK-MSG-01         TO WRK-OUT-MENSA-QMTM
               MOVE '*'                TO WRK-INCONSIS
               GO TO 1250-99-FIM
           END-IF.

BSI002     IF (ACU-SELEC-D             GREATER ZEROS   AND

BSI002        (ACU-SELEC-C             GREATER ZEROS   OR
BSI002         ACU-SELEC-M             GREATER ZEROS)) OR
BSI002        (ACU-SELEC-C             GREATER ZEROS   AND
BSI002        (ACU-SELEC-D             GREATER ZEROS   OR
BSI002         ACU-SELEC-M             GREATER ZEROS)) OR
BSI002        (ACU-SELEC-M             GREATER ZEROS   AND
BSI002        (ACU-SELEC-D             GREATER ZEROS   OR
BSI002         ACU-SELEC-C             GREATER ZEROS))
               MOVE WRK-MSG-05         TO WRK-OUT-MENSA-QMTM
               MOVE '*'                TO WRK-INCONSIS
               PERFORM VARYING WRK-IND FROM 1 BY 1
                         UNTIL WRK-IND GREATER 7
                   IF  WRK-INP-SELEC-QMTM(WRK-IND) NOT EQUAL SPACES
                       MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-QMTM
                                          (WRK-IND)
                   END-IF
               END-PERFORM
               GO TO 1250-99-FIM
           END-IF.

           IF  ACU-SELEC-D             GREATER 1

               MOVE WRK-MSG-06         TO WRK-OUT-MENSA-QMTM
               MOVE '*'                TO WRK-INCONSIS
               PERFORM VARYING WRK-IND FROM 1 BY 1
                         UNTIL WRK-IND GREATER 7
                   IF  WRK-INP-SELEC-QMTM(WRK-IND) EQUAL 'D'
                       MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-QMTM
                                         (WRK-IND)
                   END-IF
               END-PERFORM
               GO TO 1250-99-FIM
           END-IF.

           IF  ACU-SELEC-C             GREATER 1
               MOVE WRK-MSG-07         TO WRK-OUT-MENSA-QMTM
               MOVE '*'                TO WRK-INCONSIS
               PERFORM VARYING WRK-IND FROM 1 BY 1
                         UNTIL WRK-IND GREATER 7
                   IF  WRK-INP-SELEC-QMTM(WRK-IND) EQUAL 'C'
                       MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-QMTM
                                         (WRK-IND)
                   END-IF

               END-PERFORM
               GO TO 1250-99-FIM
           END-IF.

BSI002     IF  ACU-SELEC-M             GREATER 1
BSI002         MOVE WRK-MSG-09         TO WRK-OUT-MENSA-QMTM
BSI002         MOVE '*'                TO WRK-INCONSIS
BSI002         PERFORM VARYING WRK-IND FROM 1 BY 1
BSI002                   UNTIL WRK-IND GREATER 7
BSI002             IF  WRK-INP-SELEC-QMTM(WRK-IND) EQUAL 'M'
BSI002                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-QMTM
BSI002                                   (WRK-IND)
BSI002             END-IF
BSI002         END-PERFORM
BSI002         GO TO 1250-99-FIM
BSI002     END-IF.

           IF  ACU-SELEC-C             GREATER ZEROS
               PERFORM 1251-ACESSAR-DCOM1714
           ELSE
               IF  ACU-SELEC-D         GREATER ZEROS

                   PERFORM 1252-ACESSAR-DCOM1730
               ELSE
BSI002             IF  ACU-SELEC-M     GREATER ZEROS
BSI002                 PERFORM 1253-DETALHAR-MORATORIA
BSI002             END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1251-ACESSAR-DCOM1714           SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-INP-OPCAO-QMTM     TO WRK-COMU-OPCAO-N
           MOVE WRK-INP-TMSTAMP-QMTM   TO WRK-COMU-TIMESTAMP
           MOVE WRK-INP-PAGINA1-QMTM   TO WRK-COMU-PAGINA-1

           MOVE WRK-INP-PAGINA-QMTM-N  TO WRK-COMU-PAGINA-2
           MOVE WRK-INP-OPMENU-QMTM    TO WRK-COMU-OP-MENU
           MOVE WRK-INP-TPSELEC-QMTM   TO WRK-COMU-TP-SELECAO
           MOVE WRK-INP-MODBAIX-QMTM   TO WRK-COMU-MODAL-BAIXA
           MOVE WRK-INP-CDOPERA-QMTM   TO WRK-COMU-OPERACAO.

           PERFORM VARYING WRK-IND FROM 1 BY 1
                     UNTIL WRK-IND GREATER 7
                        OR WRK-INP-SELEC-QMTM(WRK-IND) EQUAL 'D'
           END-PERFORM.

           MOVE WRK-INP-PARC-QMTM-N(WRK-IND)
                                       TO WRK-COMU-PARCELA.
           MOVE WRK-INP-DTVENC-QMTM(WRK-IND)
                                       TO WRK-COMU-VENCIMENTO.
           MOVE 'DCOM1713'             TO WRK-COMU-TRANSACAO.
           MOVE 'DCOM1714'             TO WRK-TELA.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*

       1251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1252-ACESSAR-DCOM1730           SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-INP-OPCAO-QMTM     TO WRK-COMU-OPCAO-N
           MOVE WRK-INP-TMSTAMP-QMTM   TO WRK-COMU-TIMESTAMP
           MOVE WRK-INP-PAGINA1-QMTM   TO WRK-COMU-PAGINA-1
           MOVE WRK-INP-PAGINA-QMTM-N  TO WRK-COMU-PAGINA-2
           MOVE ZEROS                  TO WRK-COMU-PAGINA-3
           MOVE WRK-INP-OPMENU-QMTM    TO WRK-COMU-OP-MENU
           MOVE WRK-INP-TPSELEC-QMTM   TO WRK-COMU-TP-SELECAO
           MOVE WRK-INP-MODBAIX-QMTM   TO WRK-COMU-MODAL-BAIXA
           MOVE WRK-INP-CDOPERA-QMTM   TO WRK-COMU-OPERACAO.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 7

                        OR WRK-INP-SELEC-QMTM(WRK-IND) EQUAL 'D'
           END-PERFORM.

           MOVE WRK-INP-PARC-QMTM-N(WRK-IND)
                                       TO WRK-COMU-PARCELA.
           MOVE 'DCOM1713'             TO WRK-COMU-TRANSACAO.
           MOVE 'DCOM1730'             TO WRK-TELA.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1252-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BSI002*----------------------------------------------------------------*
BSI002 1253-DETALHAR-MORATORIA         SECTION.
BSI002*----------------------------------------------------------------*
BSI002
BSI002     INITIALIZE WRK-COMU-AREA.
BSI002
BSI002     MOVE LENGTH                 OF WRK-COMU-AREA

BSI002                                 TO WRK-COMU-LL.
BSI002     MOVE WRK-CHNG               TO WRK-FUNCAO.
BSI002     MOVE 'DCOM3928'             TO WRK-TELA.
BSI002     MOVE 'DCOM1713'             TO WRK-COMU-TRANSACAO.
BSI002     MOVE WRK-INP-OPCAO-QMTM     TO WRK-COMU-OPCAO-N.
BSI002     MOVE WRK-INP-TMSTAMP-QMTM   TO WRK-COMU-TIMESTAMP
BSI002     MOVE WRK-INP-DANO-OPER-QMTM TO WRK-COMU-DANO-OPER
BSI002     MOVE WRK-INP-NSEQ-OPER-QMTM TO WRK-COMU-NSEQ-OPER
BSI002
BSI002     PERFORM
BSI002     VARYING WRK-IND             FROM 1 BY 1
BSI002       UNTIL WRK-IND             GREATER 7
BSI002          IF WRK-INP-SELEC-QMTM (WRK-IND) EQUAL 'M'
BSI002             MOVE WRK-INP-PARC-QMTM-N
BSI002                                (WRK-IND) TO WRK-COMU-PARCELA
BSI002          END-IF
BSI002     END-PERFORM.
BSI002
BSI002     MOVE WRK-INP-BCO-QMTM       TO WRK-COMU-BANCO
BSI002     MOVE WRK-INP-AGE-QMTM       TO WRK-COMU-AGENCIA
BSI002     MOVE WRK-INP-CTA-QMTM       TO WRK-COMU-CONTA

BSI002     MOVE WRK-INP-CPFCNPJOC-QMTM TO WRK-COMU-CCNPJ-CPF
BSI002     MOVE WRK-INP-FILIAL-QMTM    TO WRK-COMU-CFLIAL-CNPJ
BSI002     MOVE WRK-INP-CTRL-QMTM      TO WRK-COMU-CCTRL-CNPJCPF
BSI002     MOVE WRK-INP-PAGINA1-QMTM   TO WRK-COMU-PAGINA-1
BSI002     MOVE WRK-INP-PAGINA2-QMTM   TO WRK-COMU-PAGINA-2
BSI002     MOVE WRK-INP-OPMENU-QMTM    TO WRK-COMU-OP-MENU.
BSI002     MOVE WRK-INP-TPSELEC-QMTM   TO WRK-COMU-TP-SELECAO.
BSI002     MOVE WRK-INP-MODBAIX-QMTM   TO WRK-COMU-MODAL-BAIXA.
BSI002     MOVE WRK-INP-PAGINA-QMTM-N  TO WRK-COMU-PAGINA-3.
BSI002     MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.
BSI002
BSI003*----------------------------------------------------------------*
BSI003 1253-99-FIM.                    EXIT.
BSI003*----------------------------------------------------------------*
BSI003
      *----------------------------------------------------------------*
       1270-TRATAR-PFK07               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-QMTM-N   EQUAL  1
           OR  WRK-INP-TPSELEC-QMTM    EQUAL  2

               MOVE 'NAO EXISTEM MAIS PAGINAS PARA RETORNAR'
                                       TO WRK-OUT-MENSA-QMTM
               GO TO 1270-99-FIM
           END-IF.

           COMPUTE 5310E-RESTART = ( WRK-INP-PAGINA-QMTM-N - 2 ) * 7.

           PERFORM 1120-ACESSAR-DCOM5310.

           MOVE 'AMOSTRAGEM CONTINUA'  TO WRK-OUT-MENSA-QMTM.

           SUBTRACT 1                  FROM WRK-OUT-PAGINA-QMTM-N.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK08              *
      *----------------------------------------------------------------*
       1280-TRATAR-PFK08               SECTION.

      *----------------------------------------------------------------*

           IF  WRK-INP-FIMAMOS-QMTM    EQUAL  '*'
           OR  WRK-INP-TPSELEC-QMTM    EQUAL  2
               MOVE 'NAO EXISTEM PAGINAS PARA AVANCAR'
                                       TO WRK-OUT-MENSA-QMTM
               GO TO 1280-99-FIM
           END-IF.

           COMPUTE 5310E-RESTART = ( WRK-INP-PAGINA-QMTM-N ) * 7.

           PERFORM 1120-ACESSAR-DCOM5310.

           IF  WRK-OUT-FIMAMOS-QMTM    EQUAL '*'
               MOVE 'FIM DE AMOSTRAGEM'TO WRK-OUT-MENSA-QMTM
           ELSE
               MOVE 'AMOSTRAGEM CONTINUA'
                                       TO WRK-OUT-MENSA-QMTM
           END-IF.

           ADD 1                       TO WRK-OUT-PAGINA-QMTM-N.


      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1400-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT-QMTM
                                       TO WRK-OUT-LL-QMTM
                                          WRK-660-LL-MENSAGEM-QMTM.
           MOVE LENGTH                 OF WRK-660-DCOMQMTM
                                       TO WRK-660-LL-AREA-QMTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-QMTM
                                             WRK-660-DCOMQMTM.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-QMTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM1713'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM1713'         TO ERR-PGM
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
