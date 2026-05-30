
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1711.
       AUTHOR.     DAVI BRAZ.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM1711                                    *
      *    PROGRAMADOR.:   DAVI BRAZ                                   *
      *    DATA........:   15/01/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EXCLUIR ESTORNO DA BAIXA DE PARCELAS -      *
      *                    PARC. CHEQUES                               *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMQKTM - EXCLUIR ESTORNO DA BAIXA DE PARCELAS -           *
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
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM1711 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*


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
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.

BSI002     05  WRK-IND                 PIC  9(002) COMP-3  VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.
           05  WRK-MSG-AUX             PIC  X(079)         VALUE SPACES.
           05  WRK-QTDE-TELA           PIC  9(003)         VALUE ZEROS.
           05  WRK-QTDE-EXCLUIDOS      PIC  9(003)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-SELEC-E                 PIC  9(003) COMP-3  VALUE ZEROS.
       01  ACU-SELEC-D                 PIC  9(003) COMP-3  VALUE ZEROS.
       01  ACU-SELEC-C                 PIC  9(003) COMP-3  VALUE ZEROS.
BSI002 01  ACU-SELEC-M                 PIC  9(003) COMP-3  VALUE ZEROS.

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
           '*** TELA DCOMQKTM - INPUT ***'.
      *---------------------------------------------------------------*


       01  WRK-INPUT-QKTM.
           05 FILLER                   PIC  X(018)         VALUE SPACES.
           05 WRK-INP-DADOS-QKTM.
             10 WRK-INP-PFK-QKTM       PIC  X(001)         VALUE SPACES.
             10 WRK-INP-SENHAS-QKTM    PIC  X(037)         VALUE SPACES.
             10 WRK-INP-COMANDO-QKTM   PIC  X(068)         VALUE SPACES.
             10 WRK-INP-FIMAMOS-QKTM   PIC  X(001)         VALUE SPACES.
             10 WRK-INP-PAGINA-QKTM.
              15 WRK-INP-PAGINA-QKTM-N PIC  9(003)         VALUE ZEROS.
            05 WRK-INP-DADOS-FIXOS1-QKTM.
             10 WRK-INP-OCULTO-QKTM    PIC  X(079)         VALUE SPACES.
             10 REDEFINES              WRK-INP-OCULTO-QKTM.
               15 WRK-INP-OPCAO-QKTM   PIC  9(001).
               15 WRK-INP-TMSTAMP-QKTM PIC  X(026).
               15 WRK-INP-PAGINA1-QKTM PIC  9(004).
               15 WRK-INP-PAGINA2-QKTM PIC  9(004).
               15 WRK-INP-OPMENU-QKTM  PIC  9(001).
               15 WRK-INP-TPSELEC-QKTM PIC  9(001).
               15 WRK-INP-MODBAIX-QKTM PIC  X(001).
               15 WRK-INP-FASE-QKTM    PIC  X(001).

4S2511*        15 WRK-INP-CPFCNPJOC-QKTM
4S2511*                                PIC  9(009).
4S2511         15 WRK-INP-CPFCNPJOC-QKTM
4S2511                                 PIC  X(009).
             10 WRK-INP-NOME-QKTM      PIC  X(040)         VALUE SPACES.
             10 WRK-INP-CPFCNPJ-QKTM   PIC  X(019)         VALUE SPACES.
             10 REDEFINES              WRK-INP-CPFCNPJ-QKTM.
                15 WRK-INP-CCNPJ-CPF-QKTM
                                       PIC  999.999.999.
                15 FILLER              PIC  X(001).
4S2511*              15 WRK-INP-FILIAL-QKTM PIC  9(004).
4S2511               15 WRK-INP-FILIAL-QKTM PIC  X(04).
                15 FILLER              PIC  X(001).
                15 WRK-INP-CTRL-QKTM   PIC  9(002).
             10 WRK-INP-BCAGCTA-QKTM   PIC  X(023)         VALUE SPACES.
             10 REDEFINES              WRK-INP-BCAGCTA-QKTM.
                15 WRK-INP-BCO-QKTM    PIC  9(003).
                15 FILLER              PIC  X(001).
                15 WRK-INP-AGE-QKTM    PIC  9(005).
                15 FILLER              PIC  X(001).
                15 WRK-INP-CTA-QKTM    PIC  9(013).
             10 WRK-INP-CDPRODT-QKTM.
              15 WRK-INP-CDPRODT-QKTM-N
                                       PIC  9(003)         VALUE ZEROS.

             10 WRK-INP-DSPRODT-QKTM   PIC  X(012)         VALUE SPACES.
             10 WRK-INP-CDSPROD-QKTM.
              15 WRK-INP-CDSPROD-QKTM-N
                                       PIC  9(003)         VALUE ZEROS.
             10 WRK-INP-DSUBPRD-QKTM   PIC  X(012)         VALUE SPACES.
             10 WRK-INP-CDOPERA-QKTM.
                15 WRK-INP-DANO-OPER-QKTM
                                       PIC  9(004)         VALUE ZEROS.
                15 WRK-INP-NSEQ-OPER-QKTM
                                       PIC  9(009)         VALUE ZEROS.
             10 WRK-INP-SITU1-QKTM     PIC  X(015)         VALUE SPACES.
             10 WRK-INP-SITU2-QKTM     PIC  X(013)         VALUE SPACES.
             10 WRK-INP-SITU3-QKTM.
              15 WRK-INP-SITU3-QKTM-N  PIC  9(009)         VALUE ZEROS.
             10 WRK-INP-SITU4-QKTM     PIC  X(013)         VALUE SPACES.
             10 WRK-INP-SITU5-QKTM.
              15 WRK-INP-SITU5-QKTM-N  PIC  9(009)         VALUE ZEROS.
            05 WRK-INP-DADOS-VAR-QKTM.
             10 WRK-INP-TABELA-QKTM    OCCURS 007  TIMES.
               15 WRK-INP-SELEC-QKTM   PIC  X(001)         VALUE SPACES.
               15 WRK-INP-PARC-QKTM.

                20 WRK-INP-PARC-QKTM-N PIC  9(005)         VALUE ZEROS.
               15 WRK-INP-QTDCHQ-QKTM  PIC  X(004)         VALUE SPACES.
               15 WRK-INP-DTVENC-QKTM  PIC  X(010)         VALUE SPACES.
               15 WRK-INP-VLPARC-QKTM  PIC  X(014)         VALUE SPACES.
               15 WRK-INP-VLATUA-QKTM  PIC  X(014)         VALUE SPACES.
               15 WRK-INP-VLBAIX-QKTM  PIC  X(014)         VALUE SPACES.
           05 WRK-INP-DADOS-FIXOS2-QKTM.
             10 WRK-INP-TOTPRCB-QKTM   PIC  X(005)         VALUE SPACES.
             10 WRK-INP-VALORBX-QKTM   PIC  X(018)         VALUE SPACES.
           05 WRK-INP-MENSA-QKTM       PIC  X(079)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQKTM - OUTPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-OUTPUT-QKTM.
           05 WRK-OUT-LL-QKTM          PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-OUT-ZZ-QKTM          PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-OUT-DADOS-QKTM.
             10 WRK-OUT-SENHAS-QKTM    PIC  X(037)         VALUE SPACES.

             10 WRK-OUT-COMANDO-ATTR-QKTM
                                       PIC  9(004) COMP    VALUE ZEROS.
             10 WRK-OUT-COMANDO-QKTM   PIC  X(068)         VALUE SPACES.
             10 WRK-OUT-FIMAMOS-QKTM   PIC  X(001)         VALUE SPACES.
             10 WRK-OUT-PAGINA-QKTM.
              15 WRK-OUT-PAGINA-QKTM-N PIC  999.
            05 WRK-OUT-DADOS-FIXOS1-QKTM.
             10 WRK-OUT-OCULTO-QKTM    PIC  X(079)         VALUE SPACES.
             10 REDEFINES              WRK-OUT-OCULTO-QKTM.
               15 WRK-OUT-OPCAO-QKTM   PIC  9(001).
               15 WRK-OUT-TMSTAMP-QKTM PIC  X(026).
               15 WRK-OUT-PAGINA1-QKTM PIC  9(004).
               15 WRK-OUT-PAGINA2-QKTM PIC  9(004).
               15 WRK-OUT-OPMENU-QKTM  PIC  9(001).
               15 WRK-OUT-TPSELEC-QKTM PIC  9(001).
               15 WRK-OUT-MODBAIX-QKTM PIC  X(001).
               15 WRK-OUT-FASE-QKTM    PIC  X(001).
4S2511*        15 WRK-OUT-CPFCNPJOC-QKTM
4S2511*                                PIC  9(009).
4S2511         15 WRK-OUT-CPFCNPJOC-QKTM
4S2511                                 PIC  X(09).
             10 WRK-OUT-NOME-QKTM      PIC  X(040)         VALUE SPACES.
             10 WRK-OUT-CPFCNPJ-QKTM   PIC  X(019)         VALUE SPACES.

             10 REDEFINES              WRK-OUT-CPFCNPJ-QKTM.
                15 WRK-OUT-CCNPJ-CPF-QKTM
                                       PIC  999.999.999.
                15 FILLER              PIC  X(001).
4S2511*              15 WRK-OUT-FILIAL-QKTM PIC  9(004).
4S2511               15 WRK-OUT-FILIAL-QKTM PIC  X(04).
                15 FILLER              PIC  X(001).
                15 WRK-OUT-CTRL-QKTM   PIC  9(002).
             10 WRK-OUT-BCAGCTA-QKTM   PIC  X(023)         VALUE SPACES.
             10 REDEFINES              WRK-OUT-BCAGCTA-QKTM.
                15 WRK-OUT-BCO-QKTM    PIC  9(003).
                15 FILLER              PIC  X(001).
                15 WRK-OUT-AGE-QKTM    PIC  9(005).
                15 FILLER              PIC  X(001).
                15 WRK-OUT-CTA-QKTM    PIC  9(013).
             10 WRK-OUT-CDPRODT-QKTM   PIC  X(003)         VALUE SPACES.
             10 WRK-OUT-DSPRODT-QKTM   PIC  X(012)         VALUE SPACES.
             10 WRK-OUT-CDSPROD-QKTM   PIC  X(003)         VALUE SPACES.
             10 WRK-OUT-DSUBPRD-QKTM   PIC  X(012)         VALUE SPACES.
             10 WRK-OUT-CDOPERA-QKTM.
                15 WRK-OUT-DANO-OPER-QKTM
                                       PIC  9(004)         VALUE ZEROS.

                15 WRK-OUT-NSEQ-OPER-QKTM
                                       PIC  9(009)         VALUE ZEROS.
             10 WRK-OUT-SITU1-QKTM     PIC  X(015)         VALUE SPACES.
             10 WRK-OUT-SITU2-QKTM     PIC  X(013)         VALUE SPACES.
             10 WRK-OUT-SITU3-QKTM.
              15 WRK-OUT-SITU3-QKTM-N  PIC  9(009)         VALUE ZEROS.
             10 WRK-OUT-SITU4-QKTM     PIC  X(013)         VALUE SPACES.
             10 WRK-OUT-SITU5-QKTM.
              15 WRK-OUT-SITU5-QKTM-N  PIC  9(009)         VALUE ZEROS.
            05 WRK-OUT-DADOS-VAR-QKTM.
             10 WRK-OUT-TABELA-QKTM    OCCURS 007  TIMES.
                15 WRK-OUT-SELEC-ATTR-QKTM
                                       PIC  9(004) COMP    VALUE ZEROS.
                15 WRK-OUT-SELEC-QKTM  PIC  X(001)         VALUE SPACES.
                15 WRK-OUT-PARC-QKTM.
                  20 WRK-OUT-PARC-QKTM-N
                                       PIC  99999          VALUE ZEROS.
                15 WRK-OUT-QTDCHQ-QKTM.
                  20 WRK-OUT-QTDCHQ-QKTM-N
                                       PIC  9(004)         VALUE ZEROS.
                15 WRK-OUT-DTVENC-QKTM PIC  X(010)         VALUE SPACES.

                15 WRK-OUT-VLPARC-QKTM.
                   20 WRK-OUT-VLPARC-QKTM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
                15 WRK-OUT-VLATUA-QKTM.
                   20 WRK-OUT-VLATUA-QKTM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
                15 WRK-OUT-VLBAIX-QKTM.
                   20 WRK-OUT-VLBAIX-QKTM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05 WRK-OUT-DADOS-FIXOS2-QKTM.
             10 WRK-OUT-TOTPRCB-QKTM   PIC  X(005)         VALUE SPACES.
             10 WRK-OUT-VALORBX-QKTM.
               15 WRK-OUT-VALORBX-QKTM-N
                                       PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-MENSA-QKTM     PIC  X(079)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** TELA DCOMQKTM - BRAD0660 - SEG. 00 ***'.
      *---------------------------------------------------------------*


       01  WRK-660-DCOMQKTM.
           05  WRK-660-LL-AREA-QKTM        PIC  9(04)  COMP  VALUE 0000.
           05  WRK-660-LL-MENSAGEM-QKTM    PIC  9(04)  COMP  VALUE 0000.
           05  WRK-660-SENHAS-QKTM         PIC  9(04)  COMP  VALUE 0037.
           05  WRK-660-COMANDO-QKTM        PIC  9(04)  COMP  VALUE 2070.
           05  WRK-660-FIMAMOS-QKTM        PIC  9(04)  COMP  VALUE 0001.
           05  WRK-660-PAGINA-QKTM         PIC  9(04)  COMP  VALUE 0003.
           05  WRK-660-OCULTO-QKTM         PIC  9(04)  COMP  VALUE 0079.
           05  WRK-660-NOME-QKTM           PIC  9(04)  COMP  VALUE 0040.
           05  WRK-660-CPFCNPJ-QKTM        PIC  9(04)  COMP  VALUE 0019.
           05  WRK-660-BCAGCTA-QKTM        PIC  9(04)  COMP  VALUE 0023.
           05  WRK-660-CDPRODT-QKTM        PIC  9(04)  COMP  VALUE 0003.
           05  WRK-660-DSPRODT-QKTM        PIC  9(04)  COMP  VALUE 0012.
           05  WRK-660-CDSPROD-QKTM        PIC  9(04)  COMP  VALUE 0003.
           05  WRK-660-DSUBPRD-QKTM        PIC  9(04)  COMP  VALUE 0012.
           05  WRK-660-CDOPERA-QKTM        PIC  9(04)  COMP  VALUE 0013.
           05  WRK-660-SITU1-QKTM          PIC  9(04)  COMP  VALUE 0015.
           05  WRK-660-SITU2-QKTM          PIC  9(04)  COMP  VALUE 0013.
           05  WRK-660-SITU3-QKTM          PIC  9(04)  COMP  VALUE 0009.
           05  WRK-660-SITU4-QKTM          PIC  9(04)  COMP  VALUE 0013.

           05  WRK-660-SITU5-QKTM          PIC  9(04)  COMP  VALUE 0009.
           05  WRK-660-OCORRENCIAS         OCCURS 7 TIMES.
             10 WRK-660-SELEC-QKTM         PIC  9(04)  COMP  VALUE 2003.
             10 WRK-660-PAR-QKTM           PIC  9(04)  COMP  VALUE 0005.
             10 WRK-660-QTDCHQ-QKTM        PIC  9(04)  COMP  VALUE 0004.
             10 WRK-660-DTVENC-QKTM        PIC  9(04)  COMP  VALUE 0010.
             10 WRK-660-VLPARC-QKTM        PIC  9(04)  COMP  VALUE 0014.
             10 WRK-660-VLATUA-QKTM        PIC  9(04)  COMP  VALUE 0014.
             10 WRK-660-VLBAIX-QKTM        PIC  9(04)  COMP  VALUE 0014.
           05  WRK-660-TOTPRCB-QKTM        PIC  9(04)  COMP  VALUE 0005.
           05  WRK-660-VALORBX-QKTM        PIC  9(04)  COMP  VALUE 0018.
           05  WRK-660-MENSA-QKTM          PIC  9(04)  COMP  VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           'AREA DE COMUNICACAO COM OS MODULOS DCOM1702/1703/1704'.
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
BSI002         15 WRK-FILLER           PIC  X(003)         VALUE SPACES.
             10 FILLER REDEFINES       WRK-COMU-DADOS-2.
               15 WRK-COMU-PAGINA-3    PIC  9(004).
BSI002         15 WRK-COMU-INCL-EXCL   PIC  X(001).
BSI002         15 WRK-COMU-CHAMADOR    PIC  X(008).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE WRK-COMUNICACAO COM DCOM5309 ***'.
      *----------------------------------------------------------------*

       01  WRK-5309-ENTRADA.

           05 WRK-5309E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5309E-MSG-MENSAGEM   PIC  X(079)         VALUE SPACES.
           05 WRK-5309E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5309E-FLAG           PIC  9(001)         VALUE ZEROS.
           05 WRK-5309E-START-COUNT    PIC  X(001)         VALUE SPACES.
           05 WRK-5309E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5309E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5309E-OPERACAO       PIC  9(013)         VALUE ZEROS.
           05 FILLER REDEFINES         WRK-5309E-OPERACAO.
             10 WRK-5309E-DANO-OPER    PIC  9(004).
             10 WRK-5309E-NSEQ-OPER    PIC  9(009).
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
           '*** AREA DE COMUNICACAO COM DCOM5310 ***'.
      *----------------------------------------------------------------*

       01  5310-ENTRADA-ROTEADOR.
           05 5310E-COD-RETORNO        PIC  X(004)       VALUE SPACES.
           05 5310E-MSG-RETORNO        PIC  X(079)       VALUE SPACES.
           05 5310E-RESTART            PIC  9(005)       VALUE ZEROS.
           05 5310E-FLAG               PIC  X(001)       VALUE SPACES.
           05 5310E-START-COUNT        PIC  X(001)       VALUE SPACES.
           05 5310E-CFUNC-BDSCO        PIC  9(009)       VALUE ZEROS.
           05 5310E-CTERM              PIC  X(008)       VALUE SPACES.
           05 5310E-AGENCIA-FUNC       PIC  9(005)       VALUE ZEROS.
           05 5310E-CPRODT             PIC  9(003)       VALUE ZEROS.
           05 5310E-DANO-OPER-DESC     PIC  9(004)       VALUE ZEROS.
           05 5310E-NSEQ-OPER-DESC     PIC  9(009)       VALUE ZEROS.
           05 5310E-NPCELA-OPER-DESC   PIC  9(005)       VALUE ZEROS.

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
4S2511        10 5310S-CCTRL-CNPJ-CPF    PIC  X(09)        VALUE SPACES.
              10 5310S-IPSSOA-SACDO      PIC  X(040)       VALUE SPACES.
              10 5310S-QTDE-CHEQUES      PIC  9(009)       VALUE ZEROS.
4S2511*       10 5310S-CNPJ              PIC  9(015)       VALUE ZEROS.
4S2511        10 5310S-CNPJ              PIC  X(015)       VALUE SPACES.
              10 FILLER REDEFINES        5310S-CNPJ.
4S2511*          15 5310S-CNPJ-RAIZ      PIC  9(009).
4S2511           15 5310S-CNPJ-RAIZ      PIC  X(09).
4S2511*          15 5310S-CNPJ-FLIAL     PIC  9(004).
4S2511           15 5310S-CNPJ-FLIAL     PIC  X(04).
                 15 5310S-CNPJ-CTRL      PIC  9(002).
              10 5310S-NOSSO-NRO         PIC  9(018)       VALUE ZEROS.
BSI001        10 WRK-5310S-CADM-CATAO-CREDT
BSI001                                   PIC  9(009)       VALUE ZEROS.
BSI001        10 WRK-5310S-IADM-CATAO-CREDT
BSI001                                   PIC  X(025)       VALUE SPACES.
BSI001        10 WRK-5310S-CINDCD-VDA-RCBVL
BSI001                                   PIC  X(001)       VALUE SPACES.

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
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO DO DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG-01                  PIC  X(079)         VALUE
BSI002     'SELECIONE COM "E" P/EXCLUIR,"D" P/DET.BAIXA,"C" P/CHEQUES OU
BSI002-    ' "M" P/MORATORIA'.
       01  WRK-MSG-02                  PIC  X(079)         VALUE
           'PF INVALIDA'.
       01  WRK-MSG-03                  PIC  X(079)         VALUE
           'TECLE <PF6> PARA CONFIRMAR OU <ENTER> PARA ALTERAR'.
       01  WRK-MSG-04                  PIC  X(079)         VALUE
           'EXCLUSAO DE ESTORNO DA BAIXA COMANDADA DE PARCELA EFETUADA C
      -    'OM SUCESSO'.
       01  WRK-MSG-05                  PIC  X(079)         VALUE
BSI002     'SELECIONE APENAS COM "E", "D", "C" OU "M" E TECLE <ENTER>'.
       01  WRK-MSG-06                  PIC  X(079)         VALUE
           'SELECIONE APENAS UMA PARCELA COM "D" E TECLE <ENTER>'.
       01  WRK-MSG-07                  PIC  X(079)         VALUE

           'SELECIONE APENAS UMA PARCELA COM "C" E TECLE <ENTER>'.
BSI002 01  WRK-MSG-09                  PIC  X(079)         VALUE
BSI002     'SELECIONE APENAS UMA PARCELA COM "M" E TECLE <ENTER>'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM1711 - FIM DA AREA DE WORKING ***'.
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

           INITIALIZE 5310-ENTRADA-ROTEADOR

                      5310-SAIDA-ROTEADOR
                      4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR
                      WRK-COMU-AREA.

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

           MOVE 'DCOMQKTM'             TO WRK-TELA.


           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1711'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMQKTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMQKTM'
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


           MOVE WRK-COMU-OPCAO-N       TO WRK-INP-OPCAO-QKTM
                                          WRK-OUT-OPCAO-QKTM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TMSTAMP-QKTM
                                          WRK-OUT-TMSTAMP-QKTM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGINA1-QKTM
                                          WRK-OUT-PAGINA1-QKTM.
           MOVE WRK-COMU-PAGINA-2      TO WRK-INP-PAGINA2-QKTM
                                          WRK-OUT-PAGINA2-QKTM.
           MOVE WRK-COMU-OP-MENU       TO WRK-INP-OPMENU-QKTM
                                          WRK-OUT-OPMENU-QKTM.
           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPSELEC-QKTM
                                          WRK-OUT-TPSELEC-QKTM.
           MOVE WRK-COMU-MODAL-BAIXA   TO WRK-INP-MODBAIX-QKTM
                                          WRK-OUT-MODBAIX-QKTM.
           MOVE WRK-COMU-OPERACAO      TO WRK-INP-CDOPERA-QKTM
                                          WRK-OUT-CDOPERA-QKTM.

           IF  WRK-INP-TPSELEC-QKTM    EQUAL 5
               MOVE WRK-COMU-CCNPJ-CPF     TO WRK-INP-CCNPJ-CPF-QKTM
                                              WRK-OUT-CCNPJ-CPF-QKTM

BSI002                                        WRK-INP-CPFCNPJOC-QKTM
BSI002                                        WRK-OUT-CPFCNPJOC-QKTM
               MOVE WRK-COMU-CFLIAL-CNPJ   TO WRK-INP-FILIAL-QKTM
                                              WRK-OUT-FILIAL-QKTM
               MOVE WRK-COMU-CCTRL-CNPJCPF TO WRK-INP-CTRL-QKTM
                                              WRK-OUT-CTRL-QKTM
           ELSE
               MOVE WRK-COMU-BANCO         TO WRK-INP-BCO-QKTM
                                              WRK-OUT-BCO-QKTM
               MOVE WRK-COMU-AGENCIA       TO WRK-INP-AGE-QKTM
                                              WRK-OUT-AGE-QKTM
               MOVE WRK-COMU-CONTA         TO WRK-INP-CTA-QKTM
                                              WRK-OUT-CTA-QKTM
           END-IF.

           MOVE WRK-COMU-MENSAGEM      TO WRK-MSG-AUX.

           PERFORM 1110-ACESSAR-DCOM4174.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 1100-99-FIM

           END-IF.

           IF  WRK-TRANSACAO           EQUAL 'DCOM1714' OR 'DCOM1730'
               MOVE WRK-COMU-PAGINA-2  TO WRK-OUT-PAGINA-QKTM-N
               COMPUTE 5310E-RESTART = ( WRK-COMU-PAGINA-2 - 1 ) * 7
           END-IF.

           PERFORM 1120-ACESSAR-DCOM5310.

           IF  WRK-INCONSIS            EQUAL SPACES
               IF (WRK-TRANSACAO       EQUAL 'DCOM1714'  OR
                                             'DCOM1730') AND
                   WRK-MSG-AUX         NOT EQUAL SPACES
                   MOVE WRK-MSG-AUX
                                       TO WRK-OUT-MENSA-QKTM
               ELSE
                   MOVE WRK-MSG-01     TO WRK-OUT-MENSA-QKTM
               END-IF

               MOVE 1                  TO WRK-OUT-PAGINA-QKTM-N
               MOVE '1'                TO WRK-OUT-FASE-QKTM

           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA O CABECALHO DA TELA.                      *
      *----------------------------------------------------------------*
       1110-ACESSAR-DCOM4174           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-DANO-OPER-QKTM TO 4174-ENT-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-OPER-QKTM TO 4174-ENT-NSEQ-OPER-DESC
           MOVE ZEROS                  TO 4174-ENT-RESTART.
           MOVE 'N'                    TO 4174-ENT-FLAG.
           MOVE WRK-COD-USER-R         TO 4174-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM.
           MOVE 'DCOM4174'             TO WRK-MODULO

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
      *  ESSA ROTINA PREENCHE A TELA COM OS CAMPOS DO MODULO.          *
      *----------------------------------------------------------------*
       1111-FORMATAR-CABECALHO         SECTION.
      *----------------------------------------------------------------*

           MOVE 4174-SAI-IPSSOA-DESC-COML
                                       TO WRK-OUT-NOME-QKTM
           STRING 4174-SAI-CBCO
           '/'    4174-SAI-CAG-BCRIA
           '/'    4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE         INTO WRK-OUT-BCAGCTA-QKTM

           STRING 4174-SAI-CCNPJ-CPF(1:3)
           '.'    4174-SAI-CCNPJ-CPF(4:3)
           '.'    4174-SAI-CCNPJ-CPF(7:3)
           '/'    4174-SAI-CFLIAL-CNPJ
           '-'    4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE         INTO WRK-OUT-CPFCNPJ-QKTM.


           MOVE 4174-SAI-CPRODT        TO WRK-OUT-CDPRODT-QKTM
           MOVE 4174-SAI-IABREV-PRODT  TO WRK-OUT-DSPRODT-QKTM
           MOVE 4174-SAI-CSPROD-DESC-COML
                                       TO WRK-OUT-CDSPROD-QKTM
           MOVE 4174-SAI-IRSUMO-SPROD-DESC
                                       TO WRK-OUT-DSUBPRD-QKTM
           MOVE 4174-SAI-IRSUMO-SIT-DESC
                                       TO WRK-OUT-SITU1-QKTM
           MOVE SPACES                 TO WRK-OUT-SITU3-QKTM
                                          WRK-OUT-SITU5-QKTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC
                                       TO WRK-OUT-SITU1-QKTM

           EVALUATE TRUE
               WHEN 4174-SAI-CCONTR-LIM-DESC      GREATER ZEROS
                    MOVE 'CONTR LIMITE:'          TO WRK-OUT-SITU2-QKTM
                    MOVE 4174-SAI-CCONTR-LIM-DESC TO WRK-OUT-SITU3-QKTM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 8

                    MOVE 'CONV GERAL..:'          TO WRK-OUT-SITU2-QKTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITU3-QKTM

               WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                    4174-SAI-CELMTO-DESC-COML     EQUAL 9
                    MOVE 'CONV CLIENTE:'          TO WRK-OUT-SITU2-QKTM
                    MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                  TO WRK-OUT-SITU3-QKTM
           END-EVALUATE.

           IF  4174-SAI-NSMULA-OPER-DESC   GREATER  ZEROS
               IF  WRK-OUT-SITU2-QKTM      EQUAL SPACES OR LOW-VALUES
                   MOVE 'SIMULACAO...:'TO WRK-OUT-SITU2-QKTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                       TO WRK-OUT-SITU3-QKTM
               ELSE
                   MOVE 'SIMULACAO...:'TO WRK-OUT-SITU4-QKTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                       TO WRK-OUT-SITU5-QKTM
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
           MOVE WRK-COD-DEPTO-N        TO 5310E-AGENCIA-FUNC.
           MOVE WRK-INP-CDPRODT-QKTM-N TO 5310E-CPRODT.
           MOVE WRK-INP-DANO-OPER-QKTM TO 5310E-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-OPER-QKTM TO 5310E-NSEQ-OPER-DESC
           MOVE 'DCOM5310'             TO WRK-MODULO.

           IF  WRK-INP-TPSELEC-QKTM    EQUAL 2

               MOVE WRK-COMU-PARCELA   TO 5310E-NPCELA-OPER-DESC
           ELSE
               MOVE ZEROS              TO 5310E-NPCELA-OPER-DESC
           END-IF

           CALL WRK-MODULO             USING 5310-ENTRADA-ROTEADOR
                                             5310-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 5310S-COD-RETORNO
             WHEN '0000'
                  PERFORM 1121-FORMATAR-TELA
                  IF  5310S-NPCELA-DESC-COML(8) EQUAL ZEROS
                      MOVE '*'         TO WRK-OUT-FIMAMOS-QKTM
                  ELSE
                      MOVE SPACES      TO WRK-OUT-FIMAMOS-QKTM
                  END-IF

             WHEN '0099'
                  MOVE 'DB2'           TO ERR-TIPO-ACESSO

                  PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
                  MOVE 5310S-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                                          WRK-OUT-MENSA-QKTM
                  MOVE '*'             TO WRK-INCONSIS
                  IF WRK-INP-PFK-QKTM  NOT EQUAL '6'
                     IF (WRK-TRANSACAO EQUAL 'DCOM1714'  OR
                                             'DCOM1730') AND
                         WRK-MSG-AUX   NOT EQUAL SPACES
                         MOVE WRK-MSG-AUX
                                       TO WRK-COMU-MENSAGEM
                         PERFORM 1220-RETORNAR-CHAMADOR
                     END-IF
                  END-IF

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
                   MOVE SPACES         TO WRK-OUT-TABELA-QKTM  (WRK-IND)
                   MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-QKTM
                                                               (WRK-IND)
               ELSE
                   MOVE WRK-49345      TO WRK-OUT-SELEC-ATTR-QKTM
                                                               (WRK-IND)
                   MOVE SPACES         TO WRK-OUT-SELEC-QKTM   (WRK-IND)
                   MOVE 5310S-NPCELA-DESC-COML                 (WRK-IND)
                                       TO WRK-OUT-PARC-QKTM-N  (WRK-IND)
                   MOVE 5310S-QTDE-CHEQUES                     (WRK-IND)
                                       TO WRK-OUT-QTDCHQ-QKTM-N(WRK-IND)
                   MOVE 5310S-DVCTO-PCELA-DESC                 (WRK-IND)

                                       TO WRK-OUT-DTVENC-QKTM  (WRK-IND)
                   INSPECT WRK-OUT-DTVENC-QKTM                 (WRK-IND)
                                       REPLACING ALL '.' BY '/'
                   MOVE 5310S-VPRINC-PCELA-DESC                (WRK-IND)
                                       TO WRK-OUT-VLPARC-QKTM-N(WRK-IND)
                   MOVE 5310S-VATUAL-PCELA-DESC                (WRK-IND)
                                       TO WRK-OUT-VLATUA-QKTM-N(WRK-IND)
                   MOVE 5310S-VEFETV-BAIXA-PCELA               (WRK-IND)
                                       TO WRK-OUT-VLBAIX-QKTM-N(WRK-IND)
               END-IF
           END-PERFORM.

           MOVE 5310S-QTDE-BAIXA       TO WRK-OUT-TOTPRCB-QKTM.
           MOVE 5310S-VBAIXA-TOTAL     TO WRK-OUT-VALORBX-QKTM-N.

      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMQKTM         SECTION.

      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-QKTM.

           PERFORM 1210-DEVOLVER-TELA.

           EVALUATE TRUE

               WHEN WRK-INP-PFK-QKTM     EQUAL 'H' OR
                   (WRK-INP-PFK-QKTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QKTM EQUAL 'PFK01')
                    IF WRK-INP-FASE-QKTM EQUAL '2'
                       MOVE WRK-225      TO WRK-OUT-SELEC-ATTR-QKTM(1)
                                            WRK-OUT-SELEC-ATTR-QKTM(2)
                                            WRK-OUT-SELEC-ATTR-QKTM(3)
                                            WRK-OUT-SELEC-ATTR-QKTM(4)
                                            WRK-OUT-SELEC-ATTR-QKTM(5)
                                            WRK-OUT-SELEC-ATTR-QKTM(6)
                                            WRK-OUT-SELEC-ATTR-QKTM(7)
                       MOVE WRK-49345    TO WRK-OUT-COMANDO-ATTR-QKTM
                    END-IF

                    MOVE WRK-INP-MENSA-QKTM TO WRK-OUT-MENSA-QKTM

               WHEN WRK-INP-PFK-QKTM     EQUAL '3' OR
                   (WRK-INP-PFK-QKTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QKTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-QKTM     EQUAL '5' OR
                   (WRK-INP-PFK-QKTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QKTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-QKTM     EQUAL 'A' OR
                   (WRK-INP-PFK-QKTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QKTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-FASE-QKTM    EQUAL '1'
                    PERFORM 1201-TRATAR-FASE-1

               WHEN WRK-INP-FASE-QKTM    EQUAL '2'

                    PERFORM 1202-TRATAR-FASE-2

           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1201-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE

               WHEN WRK-INP-PFK-QKTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QKTM EQUAL SPACES
                    PERFORM 1250-TRATAR-ENTER

               WHEN WRK-INP-PFK-QKTM     EQUAL '7' OR
                   (WRK-INP-PFK-QKTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QKTM EQUAL 'PFK7')

                    PERFORM 1270-TRATAR-PFK07

               WHEN WRK-INP-PFK-QKTM     EQUAL '8' OR
                   (WRK-INP-PFK-QKTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QKTM EQUAL 'PFK8')
                    PERFORM 1280-TRATAR-PFK08

              WHEN OTHER
                   MOVE WRK-MSG-02     TO WRK-OUT-MENSA-QKTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1201-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1202-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE


               WHEN WRK-INP-PFK-QKTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QKTM EQUAL SPACES
                    MOVE WRK-MSG-01      TO WRK-OUT-MENSA-QKTM
                    MOVE '1'             TO WRK-OUT-FASE-QKTM

               WHEN WRK-INP-PFK-QKTM     EQUAL '6' OR
                   (WRK-INP-PFK-QKTM     EQUAL '.' AND
                    WRK-INP-COMANDO-QKTM EQUAL 'PFK06')
                    PERFORM
                    VARYING WRK-IND      FROM 1 BY 1
                      UNTIL WRK-IND      GREATER 7
                         OR WRK-INCONSIS EQUAL '*'
                         IF WRK-INP-SELEC-QKTM(WRK-IND)
                                         EQUAL 'E'
                            PERFORM 1262-ACESSAR-DCOM5335
                            MOVE '1'     TO WRK-OUT-FASE-QKTM
                         END-IF
                    END-PERFORM

                    IF  WRK-INCONSIS     EQUAL SPACES

                        PERFORM 1260-TRATAR-CONFIRMA
                    END-IF

              WHEN  OTHER
                    MOVE WRK-MSG-02    TO WRK-OUT-MENSA-QKTM
                    MOVE WRK-225       TO WRK-OUT-SELEC-ATTR-QKTM(1)
                                          WRK-OUT-SELEC-ATTR-QKTM(2)
                                          WRK-OUT-SELEC-ATTR-QKTM(3)
                                          WRK-OUT-SELEC-ATTR-QKTM(4)
                                          WRK-OUT-SELEC-ATTR-QKTM(5)
                                          WRK-OUT-SELEC-ATTR-QKTM(6)
                                          WRK-OUT-SELEC-ATTR-QKTM(7)
                    MOVE WRK-49345     TO WRK-OUT-COMANDO-ATTR-QKTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1202-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-QKTM    TO WRK-OUT-SENHAS-QKTM.
           MOVE WRK-INP-FIMAMOS-QKTM   TO WRK-OUT-FIMAMOS-QKTM.
           MOVE WRK-INP-PAGINA-QKTM    TO WRK-OUT-PAGINA-QKTM.
           MOVE WRK-INP-DADOS-FIXOS1-QKTM
                                       TO WRK-OUT-DADOS-FIXOS1-QKTM.
           MOVE WRK-INP-DADOS-FIXOS2-QKTM
                                       TO WRK-OUT-DADOS-FIXOS2-QKTM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 7
               IF  WRK-INP-PARC-QKTM(WRK-IND) EQUAL SPACES
                   MOVE SPACES     TO WRK-OUT-TABELA-QKTM(WRK-IND)
                   MOVE WRK-225    TO WRK-OUT-SELEC-ATTR-QKTM(WRK-IND)
               ELSE
                   MOVE WRK-INP-SELEC-QKTM(WRK-IND)
                                   TO WRK-OUT-SELEC-QKTM(WRK-IND)
                   MOVE WRK-INP-PARC-QKTM(WRK-IND)
                                   TO WRK-OUT-PARC-QKTM(WRK-IND)
                   MOVE WRK-INP-QTDCHQ-QKTM(WRK-IND)

                                   TO WRK-OUT-QTDCHQ-QKTM(WRK-IND)
                   MOVE WRK-INP-QTDCHQ-QKTM(WRK-IND)
                                   TO WRK-OUT-QTDCHQ-QKTM(WRK-IND)
                   MOVE WRK-INP-DTVENC-QKTM(WRK-IND)
                                   TO WRK-OUT-DTVENC-QKTM(WRK-IND)
                   MOVE WRK-INP-VLPARC-QKTM(WRK-IND)
                                   TO WRK-OUT-VLPARC-QKTM(WRK-IND)
                   MOVE WRK-INP-VLATUA-QKTM(WRK-IND)
                                   TO WRK-OUT-VLATUA-QKTM(WRK-IND)
                   MOVE WRK-INP-VLBAIX-QKTM(WRK-IND)
                                   TO WRK-OUT-VLBAIX-QKTM(WRK-IND)
                   ADD 1               TO WRK-QTDE-TELA
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

           EVALUATE WRK-INP-TPSELEC-QKTM
               WHEN '3'
                   MOVE 'DCOM1703'     TO WRK-TELA
                   MOVE WRK-INP-BCO-QKTM
                                       TO WRK-COMU-BANCO
                   MOVE WRK-INP-AGE-QKTM
                                       TO WRK-COMU-AGENCIA

               WHEN '4'
                   MOVE 'DCOM1704'     TO WRK-TELA
                   MOVE WRK-INP-BCO-QKTM
                                       TO WRK-COMU-BANCO
                   MOVE WRK-INP-AGE-QKTM
                                       TO WRK-COMU-AGENCIA
                   MOVE WRK-INP-CTA-QKTM
                                       TO WRK-COMU-CONTA


               WHEN '5'
                   MOVE 'DCOM1704'     TO WRK-TELA
                   STRING WRK-INP-CPFCNPJ-QKTM(1:3)
                          WRK-INP-CPFCNPJ-QKTM(5:3)
                          WRK-INP-CPFCNPJ-QKTM(9:3)
                   DELIMITED BY SIZE INTO WRK-COMU-CCNPJ-CPF
                   MOVE WRK-INP-FILIAL-QKTM
                                       TO WRK-COMU-CFLIAL-CNPJ
                   MOVE WRK-INP-CTRL-QKTM
                                       TO WRK-COMU-CCTRL-CNPJCPF
               WHEN OTHER
                   MOVE 'DCOM1702'     TO WRK-TELA

           END-EVALUATE.

           MOVE WRK-INP-OPCAO-QKTM     TO WRK-COMU-OPCAO-N
           MOVE WRK-INP-TMSTAMP-QKTM   TO WRK-COMU-TIMESTAMP
           MOVE WRK-INP-PAGINA1-QKTM   TO WRK-COMU-PAGINA-1
           MOVE WRK-INP-PAGINA2-QKTM   TO WRK-COMU-PAGINA-2
           MOVE WRK-INP-OPMENU-QKTM    TO WRK-COMU-OP-MENU

           MOVE WRK-INP-TPSELEC-QKTM   TO WRK-COMU-TP-SELECAO
           MOVE WRK-INP-MODBAIX-QKTM   TO WRK-COMU-MODAL-BAIXA
           MOVE WRK-INP-CDOPERA-QKTM   TO WRK-COMU-OPERACAO.
           MOVE WRK-INP-PFK-QKTM       TO WRK-COMU-PFK
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1711'             TO WRK-COMU-TRANSACAO.
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
           MOVE 'DCOM1711'             TO WRK-TELA
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
           MOVE 'DCOM1711'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE 'A'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1250-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO ACU-SELEC-C
                                          ACU-SELEC-D
                                          ACU-SELEC-E
BSI002                                    ACU-SELEC-M.
           MOVE SPACES                 TO WRK-INCONSIS.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 7
                        OR WRK-INP-PARC-QKTM(WRK-IND)
                                       EQUAL SPACES
                IF  WRK-INP-SELEC-QKTM(WRK-IND)
                                       NOT EQUAL ' ' AND 'C' AND
BSI002                                           'D' AND 'E' AND 'M'
                    MOVE WRK-MSG-05    TO WRK-OUT-MENSA-QKTM
                    MOVE '*'           TO WRK-INCONSIS

                    MOVE WRK-49353     TO WRK-OUT-SELEC-ATTR-QKTM
                                         (WRK-IND)
               ELSE
                   IF  WRK-INP-SELEC-QKTM(WRK-IND) EQUAL 'E'
                       ADD 1           TO ACU-SELEC-E
                   END-IF
                   IF  WRK-INP-SELEC-QKTM(WRK-IND) EQUAL 'D'
                       ADD 1           TO ACU-SELEC-D
                   END-IF
                   IF  WRK-INP-SELEC-QKTM(WRK-IND) EQUAL 'C'
                       ADD 1           TO ACU-SELEC-C
                   END-IF
BSI002             IF  WRK-INP-SELEC-QKTM(WRK-IND) EQUAL 'M'
BSI002                 ADD 1           TO ACU-SELEC-M
BSI002             END-IF
               END-IF
           END-PERFORM.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 1250-99-FIM
           END-IF.


           IF  ACU-SELEC-E             EQUAL ZEROS AND
               ACU-SELEC-D             EQUAL ZEROS AND
               ACU-SELEC-C             EQUAL ZEROS AND
BSI002         ACU-SELEC-M             EQUAL ZEROS
               MOVE WRK-MSG-01         TO WRK-OUT-MENSA-QKTM
               MOVE '*'                TO WRK-INCONSIS
               GO TO 1250-99-FIM
           END-IF.

BSI002     IF (ACU-SELEC-E             GREATER ZEROS   AND
BSI002        (ACU-SELEC-D             GREATER ZEROS   OR
BSI002         ACU-SELEC-C             GREATER ZEROS   OR
BSI002         ACU-SELEC-M             GREATER ZEROS)) OR
BSI002        (ACU-SELEC-D             GREATER ZEROS   AND
BSI002        (ACU-SELEC-E             GREATER ZEROS   OR
BSI002         ACU-SELEC-C             GREATER ZEROS   OR
BSI002         ACU-SELEC-M             GREATER ZEROS)) OR
BSI002        (ACU-SELEC-C             GREATER ZEROS   AND
BSI002        (ACU-SELEC-E             GREATER ZEROS   OR
BSI002         ACU-SELEC-D             GREATER ZEROS   OR

BSI002         ACU-SELEC-M             GREATER ZEROS)) OR
BSI002        (ACU-SELEC-M             GREATER ZEROS   AND
BSI002        (ACU-SELEC-E             GREATER ZEROS   OR
BSI002         ACU-SELEC-D             GREATER ZEROS   OR
BSI002         ACU-SELEC-C             GREATER ZEROS))
BSI002         MOVE WRK-MSG-05         TO WRK-OUT-MENSA-QKTM
BSI002         PERFORM VARYING WRK-IND FROM 1 BY 1
BSI002                   UNTIL WRK-IND GREATER 7
BSI002             IF  WRK-INP-SELEC-QKTM(WRK-IND) NOT EQUAL SPACES
BSI002                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-QKTM
BSI002                                   (WRK-IND)
BSI002             END-IF
BSI002         END-PERFORM
BSI002         MOVE '*'                TO WRK-INCONSIS
BSI002         GO TO 1250-99-FIM
BSI002     END-IF.

           IF  ACU-SELEC-D             GREATER 1
               MOVE WRK-MSG-06         TO WRK-OUT-MENSA-QKTM
               MOVE '*'                TO WRK-INCONSIS
               PERFORM VARYING WRK-IND FROM 1 BY 1

                         UNTIL WRK-IND GREATER 7
                   IF  WRK-INP-SELEC-QKTM(WRK-IND) EQUAL 'D'
                       MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-QKTM
                                         (WRK-IND)
                   END-IF
               END-PERFORM
               GO TO 1250-99-FIM
           END-IF.

           IF  ACU-SELEC-C             GREATER 1
               MOVE WRK-MSG-07         TO WRK-OUT-MENSA-QKTM
               MOVE '*'                TO WRK-INCONSIS
               PERFORM VARYING WRK-IND FROM 1 BY 1
                         UNTIL WRK-IND GREATER 7
                   IF  WRK-INP-SELEC-QKTM(WRK-IND) EQUAL 'C'
                       MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-QKTM
                                         (WRK-IND)
                   END-IF
               END-PERFORM
               GO TO 1250-99-FIM
           END-IF.


BSI002     IF  ACU-SELEC-M             GREATER 1
BSI002         MOVE WRK-MSG-09         TO WRK-OUT-MENSA-QKTM
BSI002         MOVE '*'                TO WRK-INCONSIS
BSI002         PERFORM VARYING WRK-IND FROM 1 BY 1
BSI002                   UNTIL WRK-IND GREATER 7
BSI002             IF  WRK-INP-SELEC-QKTM(WRK-IND) EQUAL 'M'
BSI002                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-QKTM
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
BSI002         IF  ACU-SELEC-M         GREATER ZEROS

BSI002             PERFORM 1253-DETALHAR-MORATORIA
BSI002         ELSE
                   PERFORM
                   VARYING WRK-IND           FROM 1 BY 1
                     UNTIL WRK-IND           GREATER 7
                        OR WRK-INCONSIS      EQUAL '*'
                        IF WRK-INP-SELEC-QKTM(WRK-IND)
                                             EQUAL 'E'
                           PERFORM 1262-ACESSAR-DCOM5335
                        END-IF
                   END-PERFORM

                   PERFORM
                   VARYING WRK-IND           FROM 1 BY 1
                     UNTIL WRK-IND           GREATER 7
                        OR WRK-INCONSIS      EQUAL '*'
                        IF WRK-INP-SELEC-QKTM(WRK-IND)
                                             EQUAL 'E'
                           PERFORM 1261-ACESSAR-DCOM5309
                           IF WRK-INCONSIS   EQUAL '*'
                              MOVE WRK-49353 TO WRK-OUT-SELEC-ATTR-QKTM

                                               (WRK-IND)
                           END-IF
                        END-IF
                   END-PERFORM

                   IF  WRK-INCONSIS        EQUAL SPACES
                       MOVE '2'            TO WRK-OUT-FASE-QKTM
                       MOVE WRK-MSG-03     TO WRK-OUT-MENSA-QKTM
                       MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-QKTM(1)
                                              WRK-OUT-SELEC-ATTR-QKTM(2)
                                              WRK-OUT-SELEC-ATTR-QKTM(3)
                                              WRK-OUT-SELEC-ATTR-QKTM(4)
                                              WRK-OUT-SELEC-ATTR-QKTM(5)
                                              WRK-OUT-SELEC-ATTR-QKTM(6)
                                              WRK-OUT-SELEC-ATTR-QKTM(7)
                       MOVE WRK-49345      TO WRK-OUT-COMANDO-ATTR-QKTM
                   END-IF
               END-IF
BSI002         END-IF
           END-IF.


      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1251-ACESSAR-DCOM1714           SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.

           MOVE WRK-INP-OPCAO-QKTM     TO WRK-COMU-OPCAO-N
           MOVE WRK-INP-TMSTAMP-QKTM   TO WRK-COMU-TIMESTAMP
           MOVE WRK-INP-PAGINA1-QKTM   TO WRK-COMU-PAGINA-1
           MOVE WRK-INP-PAGINA-QKTM-N  TO WRK-COMU-PAGINA-2
           MOVE WRK-INP-OPMENU-QKTM    TO WRK-COMU-OP-MENU
           MOVE WRK-INP-TPSELEC-QKTM   TO WRK-COMU-TP-SELECAO
           MOVE WRK-INP-MODBAIX-QKTM   TO WRK-COMU-MODAL-BAIXA
           MOVE WRK-INP-CDOPERA-QKTM   TO WRK-COMU-OPERACAO.

           PERFORM VARYING WRK-IND FROM 1 BY 1

                     UNTIL WRK-IND GREATER 7
                        OR WRK-INP-SELEC-QKTM(WRK-IND) EQUAL 'C'
           END-PERFORM.

           MOVE WRK-INP-PARC-QKTM-N(WRK-IND)
                                       TO WRK-COMU-PARCELA.
           MOVE WRK-INP-DTVENC-QKTM(WRK-IND)
                                       TO WRK-COMU-VENCIMENTO.

           MOVE 'DCOM1711'             TO WRK-COMU-TRANSACAO.
           MOVE WRK-INP-PFK-QKTM       TO WRK-COMU-PFK.
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
           MOVE WRK-INP-OPCAO-QKTM     TO WRK-COMU-OPCAO-N.
           MOVE WRK-INP-TMSTAMP-QKTM   TO WRK-COMU-TIMESTAMP.
           MOVE WRK-INP-PAGINA1-QKTM   TO WRK-COMU-PAGINA-1.
           MOVE WRK-INP-PAGINA-QKTM-N  TO WRK-COMU-PAGINA-2.
           MOVE ZEROS                  TO WRK-COMU-PAGINA-3.
           MOVE WRK-INP-OPMENU-QKTM    TO WRK-COMU-OP-MENU.
           MOVE WRK-INP-TPSELEC-QKTM   TO WRK-COMU-TP-SELECAO.
           MOVE WRK-INP-MODBAIX-QKTM   TO WRK-COMU-MODAL-BAIXA.
           MOVE WRK-INP-CDOPERA-QKTM   TO WRK-COMU-OPERACAO.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 7
                        OR WRK-INP-SELEC-QKTM(WRK-IND)
                                       EQUAL 'D'
           END-PERFORM.

           MOVE WRK-INP-PARC-QKTM-N   (WRK-IND)

                                       TO WRK-COMU-PARCELA.
           MOVE 'DCOM1711'             TO WRK-COMU-TRANSACAO.
           MOVE 'DCOM1730'             TO WRK-TELA.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1252-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BSI003*----------------------------------------------------------------*
BSI003 1253-DETALHAR-MORATORIA         SECTION.
BSI003*----------------------------------------------------------------*
BSI003
BSI003     INITIALIZE WRK-COMU-AREA.
BSI003
BSI003     MOVE LENGTH                 OF WRK-COMU-AREA
BSI003                                 TO WRK-COMU-LL.
BSI003     MOVE WRK-CHNG               TO WRK-FUNCAO.
BSI003     MOVE 'DCOM3928'             TO WRK-TELA.
BSI003     MOVE 'DCOM1711'             TO WRK-COMU-TRANSACAO.

BSI003     MOVE WRK-INP-OPCAO-QKTM     TO WRK-COMU-OPCAO-N.
BSI003     MOVE WRK-INP-TMSTAMP-QKTM   TO WRK-COMU-TIMESTAMP
BSI003     MOVE WRK-INP-DANO-OPER-QKTM TO WRK-COMU-DANO-OPER
BSI003     MOVE WRK-INP-NSEQ-OPER-QKTM TO WRK-COMU-NSEQ-OPER
BSI003
BSI003     PERFORM
BSI003     VARYING WRK-IND             FROM 1 BY 1
BSI003       UNTIL WRK-IND             GREATER 7
BSI003          IF WRK-INP-SELEC-QKTM (WRK-IND) EQUAL 'M'
BSI003             MOVE WRK-INP-PARC-QKTM-N
BSI003                                (WRK-IND) TO WRK-COMU-PARCELA
BSI003          END-IF
BSI003     END-PERFORM.
BSI003
BSI003     MOVE WRK-INP-BCO-QKTM       TO WRK-COMU-BANCO
BSI003     MOVE WRK-INP-AGE-QKTM       TO WRK-COMU-AGENCIA
BSI003     MOVE WRK-INP-CTA-QKTM       TO WRK-COMU-CONTA
BSI003     MOVE WRK-INP-CPFCNPJOC-QKTM TO WRK-COMU-CCNPJ-CPF
BSI003     MOVE WRK-INP-FILIAL-QKTM    TO WRK-COMU-CFLIAL-CNPJ
BSI003     MOVE WRK-INP-CTRL-QKTM      TO WRK-COMU-CCTRL-CNPJCPF
BSI003     MOVE WRK-INP-PAGINA1-QKTM   TO WRK-COMU-PAGINA-1

BSI003     MOVE WRK-INP-PAGINA2-QKTM   TO WRK-COMU-PAGINA-2
BSI003     MOVE WRK-INP-OPMENU-QKTM    TO WRK-COMU-OP-MENU.
BSI003     MOVE WRK-INP-TPSELEC-QKTM   TO WRK-COMU-TP-SELECAO.
BSI003     MOVE WRK-INP-MODBAIX-QKTM   TO WRK-COMU-MODAL-BAIXA.
BSI003     MOVE WRK-INP-PAGINA-QKTM-N  TO WRK-COMU-PAGINA-3.
BSI003     MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.
BSI003
BSI003*----------------------------------------------------------------*
BSI003 1253-99-FIM.                    EXIT.
BSI003*----------------------------------------------------------------*
BSI003
      *----------------------------------------------------------------*
       1260-TRATAR-CONFIRMA            SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND      FROM 1 BY 1
                     UNTIL WRK-IND      GREATER 7
                        OR WRK-INCONSIS EQUAL '*'
                 IF WRK-INP-SELEC-QKTM(WRK-IND) EQUAL 'E'
                    PERFORM 1261-ACESSAR-DCOM5309
                    IF WRK-INCONSIS     EQUAL SPACES

                       ADD 1            TO WRK-QTDE-EXCLUIDOS
                    END-IF
                 END-IF
           END-PERFORM.

           IF  WRK-INCONSIS            EQUAL '*'
               MOVE '1'                TO WRK-OUT-FASE-QKTM
               GO TO 1260-99-FIM
           END-IF.

           IF  WRK-QTDE-EXCLUIDOS      EQUAL WRK-QTDE-TELA
           AND WRK-INP-PAGINA-QKTM-N   NOT EQUAL 1
               PERFORM 1270-TRATAR-PFK07
           ELSE
               COMPUTE 5310E-RESTART = ( WRK-INP-PAGINA-QKTM-N - 1 )
                                                               * 7
               PERFORM 1120-ACESSAR-DCOM5310
           END-IF.

           IF  5310S-COD-RETORNO       EQUAL '0003'
           OR  WRK-INP-TPSELEC-QKTM    EQUAL  2

               MOVE WRK-MSG-04         TO WRK-COMU-MENSAGEM
               PERFORM 1220-RETORNAR-CHAMADOR
               GO TO 1260-99-FIM
           END-IF.

           MOVE '1'                    TO WRK-OUT-FASE-QKTM.

           IF  WRK-INCONSIS            EQUAL '*'
               GO TO 1260-99-FIM
           END-IF.

           MOVE WRK-MSG-04             TO WRK-OUT-MENSA-QKTM.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1261-ACESSAR-DCOM5309           SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-5309-ENTRADA
                      WRK-5309-SAIDA
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE WRK-COD-USER-R         TO WRK-5309E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5309E-CTERM.
           MOVE WRK-INP-DANO-OPER-QKTM TO WRK-5309E-DANO-OPER.
           MOVE WRK-INP-NSEQ-OPER-QKTM TO WRK-5309E-NSEQ-OPER.
           MOVE WRK-INP-PARC-QKTM-N   (WRK-IND)
                                       TO WRK-5309E-NPCELA-DESC.
           MOVE WRK-INP-CDPRODT-QKTM-N TO WRK-5309E-CPRODT.
           MOVE WRK-INP-CDSPROD-QKTM-N TO WRK-5309E-CSPROD-DESC.
           MOVE WRK-COD-DEPTO-N        TO WRK-5309E-AGENCIA-FUNC.

           IF  WRK-INP-FASE-QKTM       EQUAL '1'
               MOVE 'C'                TO WRK-5309E-FLAG-FUNCAO
           ELSE
               MOVE 'I'                TO WRK-5309E-FLAG-FUNCAO
           END-IF.


           MOVE 'DCOM5309'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5309-ENTRADA
                                             WRK-5309-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-5309S-COD-RETORNO   NOT EQUAL '0000'
               IF  WRK-5309S-COD-RETORNO   EQUAL '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               ELSE
                   MOVE WRK-5309S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-QKTM
                                          WRK-COMU-MENSAGEM
                   MOVE '*'            TO WRK-INCONSIS
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1261-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1262-ACESSAR-DCOM5335           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5335-ENTRADA
                      5335-SAIDA.

           MOVE WRK-INP-CDOPERA-QKTM   TO 5335E-OPERACAO.
           MOVE WRK-INP-PARC-QKTM-N   (WRK-IND)
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

               WHEN OTHER
                     MOVE 5335S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-QKTM
                     MOVE '*'          TO WRK-INCONSIS
                     MOVE WRK-49353    TO WRK-OUT-SELEC-ATTR-QKTM
                                         (WRK-IND)
           END-EVALUATE.

      *----------------------------------------------------------------*
       1262-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       1270-TRATAR-PFK07               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-QKTM-N   EQUAL  1
           OR  WRK-INP-TPSELEC-QKTM    EQUAL  2
               MOVE 'NAO EXISTEM MAIS PAGINAS PARA RETORNAR'
                                       TO WRK-OUT-MENSA-QKTM
               GO TO 1270-99-FIM
           END-IF.

           COMPUTE 5310E-RESTART = ( WRK-INP-PAGINA-QKTM-N - 2 ) * 7.

           PERFORM 1120-ACESSAR-DCOM5310.

           MOVE 'AMOSTRAGEM CONTINUA'  TO WRK-OUT-MENSA-QKTM.

           SUBTRACT 1                  FROM WRK-OUT-PAGINA-QKTM-N.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK08              *
      *----------------------------------------------------------------*
       1280-TRATAR-PFK08               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-FIMAMOS-QKTM    EQUAL  '*'
           OR  WRK-INP-TPSELEC-QKTM    EQUAL  2
               MOVE 'NAO EXISTEM PAGINAS PARA AVANCAR'
                                       TO WRK-OUT-MENSA-QKTM
               GO TO 1280-99-FIM
           END-IF.

           COMPUTE 5310E-RESTART = ( WRK-INP-PAGINA-QKTM-N ) * 7.

           PERFORM 1120-ACESSAR-DCOM5310.

           IF  WRK-OUT-FIMAMOS-QKTM    EQUAL '*'
               MOVE 'FIM DE AMOSTRAGEM'TO WRK-OUT-MENSA-QKTM

           ELSE
               MOVE 'AMOSTRAGEM CONTINUA'
                                       TO WRK-OUT-MENSA-QKTM
           END-IF.

           ADD 1                       TO WRK-OUT-PAGINA-QKTM-N.

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

           MOVE LENGTH                 OF WRK-OUTPUT-QKTM
                                       TO WRK-OUT-LL-QKTM

                                          WRK-660-LL-MENSAGEM-QKTM.
           MOVE LENGTH                 OF WRK-660-DCOMQKTM
                                       TO WRK-660-LL-AREA-QKTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-QKTM
                                             WRK-660-DCOMQKTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-QKTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM1711'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM1711'         TO ERR-PGM
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
