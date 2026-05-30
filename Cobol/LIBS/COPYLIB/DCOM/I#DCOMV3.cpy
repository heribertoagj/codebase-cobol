      *================================================================*
      * AREA PARA REDEFINIR MENSAGEM ENVIADA / RECEBIDA AO BACEN       *
      * LAYOUT AUXILIAR PARA O CAMPO M0-WCONTD-MSGEM-DESC-TEXT.        *
      *----------------------------------------------------------------*
      * OBSERVACOES:                                                   *
      *   - CAMPOS DE DATA NO FORMATO AAAAMMDD.                        *
      *   - CAMPOS DE VALOR POSITIVOS SEM SINAL.                       *
      *================================================================*
BRQ001*          ALTERACAO - BRQ SOLUCOES EM INFORMATICA S.A.          *
BRQ001*----------------------------------------------------------------*
BRQ001*    PROGRAMADORA:  VANESSA CHRISTINA SONETI DELGADO             *
BRQ001*    ANALISTA....:  VANESSA CHRISTINA SONETI DELGADO             *
BRQ001*    ANALISTA DDS:  RICARDO JAMMAL - GRUPO 70                    *
BRQ001*    DATA........:  11/09/2013                                   *
BRQ001*----------------------------------------------------------------*
BRQ001*    OBJETIVO....:  ALTERAR CATALOGO DE MENSAGENS PARA A VERSAO  *
BRQ001*                   4.0 DO BACEN.                                *
BRQ001*================================================================*
BSI001*               ALTERACAO - BSI TECNOLOGIA.                      *
BSI001*----------------------------------------------------------------*
BSI001*    PROGRAMADORA:  MAIRA MEDINA BARBOSA                         *
BSI001*    ANALISTA....:  LUCIANDRA RODRIGUES DA SILVEIRA              *
BSI001*    ANALISTA DDS:  RICARDO JAMMAL - GRUPO 39                    *
BSI001*    DATA........:  06/01/2014                                   *
BSI001*----------------------------------------------------------------*
BSI001*    OBJETIVO....:  ADEQUAR LAYOUT PARA VERSAO 4.01 BACEN.       *
BSI001*================================================================*
BSI002*                   ALTERACAO  -  BSI TECNOLOGIA                 *
BSI002*----------------------------------------------------------------*
BSI002*    PROGRAMADORA:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI002*    ANALISTA BSI:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI002*    ANALISTA DDS:  RICARDO JAMMAL - GRUPO 39                    *
BSI002*    DATA........:  16/06/2015                                   *
BSI002*----------------------------------------------------------------*
BSI002*    OBJETIVO....:  ADEQUAR LAYOUT PARA VERSAO 4.06 BACEN:       *
BSI002*                 - ACRESCENTAR O CAMPO CODIGO SUBPROGRAMA.      *
BSI002*                 - SUBSTITUIR O CAMPO TIPO DE CATEGORIA DO EMI- *
BSI002*                   TENTE POR CODIGO DO PORTE DO PRODUTOR.       *
BSI002*================================================================*
BSI003*                   ALTERACAO  -  BSI TECNOLOGIA                 *
BSI003*----------------------------------------------------------------*
BSI003*    ANALISTA BSI:  FERNANDO LUIZ DE SANTI  -  BSI TECNOLOGIA    *
BSI003*    ANALISTA DDS:  RICARDO JAMMAL          -  DDS / GRUPO 39    *
BSI003*    DATA........:  01/01/2016                                   *
BSI003*----------------------------------------------------------------*
BSI003*    OBJETIVO....:  ALTERAR FORMATACAO DOS CAMPOS:               *
BSI003*                 - LATITUDE                                     *
BSI003*                 - LONGITUDE                                    *
BSI003*                 - ALTITUDE                                     *
BSI003*================================================================*
BSI004*                   ALTERACAO  -  BSI TECNOLOGIA                 *
BSI004*----------------------------------------------------------------*
BSI004*    PROGRAMADORA:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI004*    ANALISTA BSI:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI004*    ANALISTA DDS:  RICARDO JAMMAL - GRUPO 39                    *
BSI004*    DATA........:  08/02/2016                                   *
BSI004*----------------------------------------------------------------*
BSI004*    OBJETIVO....:  ADEQUAR LAYOUT PARA VERSAO 4.07 BACEN:       *
BSI004*                 - SUBSTITUIR GRUPO PRODUTOR POR BENEFICIARIO.  *
BSI004*                 - EXCLUIR IDENTIFICACAO SCR.                   *
BSI004*                 - INCLUIR AREA NAO CULTIVADA.                  *
BSI004*                 - SUBSTITUIR VALOR PARCELA RECURSOS PROPRIO    *
BSI004*                   SERVICOS POR VALOR PARCELA MANUTENCAO FAMI-  *
BSI004*                   LIAR.                                        *
BSI004*                 - EXCLUIR IDENTIFICACAO DE SAFRA               *
BSI004*                 - INCLUIR CODIGO MODALIDADE SEGURO.            *
BSI004*                 - EXCLUIR TIPO GARANTIA EMPREENDIMENTO.        *
BSI004*                 - SUBSTITUIR GRUPO FORNECEDOR POR LIBERACAO.   *
BSI004*================================================================*
BSI005*                   ALTERACAO  -  BSI TECNOLOGIA                 *
BSI005*----------------------------------------------------------------*
BSI005*    PROGRAMADORA:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI005*    ANALISTA BSI:  LUCIANDRA SILVEIRA - BSI TECNOLOGIA          *
BSI005*    ANALISTA DDS:  RICARDO JAMMAL - GRUPO 39                    *
BSI005*    DATA........:  31/08/2016                                   *
BSI005*----------------------------------------------------------------*
BSI005*    OBJETIVO....:  ADEQUAR LAYOUT PARA VERSAO 4.08 BACEN:       *
BSI005*                 - ALTERAR A PRECISAO DA LATITUDE E LONGITUDE   *
BSI005*                 - ALTERAR TAG DO GRUPO BENCFRIO PARA BENFCRIO  *
BSI005*                 - EXCLUIR CAMPOS "TIPO CEDULA COR", "CNPJ BASE *
BSI005*                   IF SUBEMPRESTIMO" E "REFBACEN SUBEMPRESTIMO" *
BSI005*                 - INCLUIR GRUPO DE CAMPOS PARA DESCLASSIFICACAO*
BSI005*                   PARCIAL                                      *
BSI005*                 - INCLUIR CAMPO PARA INFORMAR MUNICIPIO GLEBA  *
BSI005*                 - INCLUIR CAMPO PARA INFORMAR TIPO BENEFICIARIO*
BRQ002*================================================================*
.     *               ALTERACAO  -  BRQ SOLUCOES EM INFORMATICA S.A.   *
.     *----------------------------------------------------------------*
.     *    PROGRAMADORA:  LEONARDO MANÇO CAMARGO - BRQ                 *
.     *    ANALISTA BRQ:  LEONARDO MANÇO CAMARGO - BRQ                 *
.     *    ANALISTA DDS:  MARCO NOGUEIRA - GRUPO 39                    *
.     *    DATA........:  31/05/2017                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  ADEQUAR LAYOUT PARA VERSAO 4.09 BACEN:       *
.     *                 - INCLUIR CAMPO: CODIGO BASE LEGAL RENEGOCIACAO*
.     *                 - INCLUIR CAMPO: NIRF                          *
.     *                 - INCLUIR CAMPO: CODIGO SNCR                   *
.     *                 - INCLUIR CAMPO: NUMERO REGISTRO CAR           *
.     *                 - INCLUIR CAMPO: DATA INICIO PLANTIO           *
.     *                 - INCLUIR CAMPO: DATA FIM PLANTIO              *
.     *                 - INCLUIR CAMPO: DATA INICIO COLHEITA          *
.     *                 - INCLUIR CAMPO: DATA FIM COLHEITA             *
.     *                 - EXCLUIR CAMPO: ANOMES PLANTIO                *
.     *                 - EXCLUIR CAMPO: ANOMES COLHEITA               *
BRQ002*================================================================*
BRQ003*          ALTERACAO - BRQ SOLUCOES EM INFORMATICA S.A.          *
BRQ003*----------------------------------------------------------------*
BRQ003*  ANALISTA BRQ..:   FABIO AUGUSTO FINK      -  BRQ              *
BRQ003*  ANALISTA DDS..:   MARCO AURELIO NOGUEIRA  -  BRADESCO / GP.39 *
BRQ003*  DATA..........:   23/11/2017                                  *
BRQ003*----------------------------------------------------------------*
BRQ003*  OBJETIVO......:   INCLUIR CAMPO CODIGO BASE LEGAL RENEGOCIACAO*
BRQ003*                    SICOR 4.10                                  *
BRQ003*================================================================*
BRQ004*               ALTERACAO - BRQ DIGITAL SOLUTIONS                *
BRQ004*----------------------------------------------------------------*
BRQ004*  ANALISTA BRQ..:   LUCIANDRA SILVEIRA      -  BRQ              *
BRQ004*  ANALISTA DS...:   MARCO AURELIO NOGUEIRA  -  BRADESCO / GP.39 *
BRQ004*  DATA..........:   04/06/2018                                  *
BRQ004*----------------------------------------------------------------*
BRQ004*  OBJETIVO......:   ADEQUAR PARA SICOR 4.11.                    *
BRQ004*================================================================*
BRQ005*                    ALTERACAO - BRQ DIGITAL SOLUTIONS           *
BRQ005*----------------------------------------------------------------*
BRQ005*  ANALISTA BRQ..:   FABIO AUGUSTO FINK    - BRQ                 *
BRQ005*  ANALISTA DS...:   RENATA DIAS FAUSTINI  - BRADESCO / GP.39    *
BRQ005*  DATA..........:   24/10/2018                                  *
BRQ005*----------------------------------------------------------------*
BRQ005*  OBJETIVO......: - BRQ 18/0000 - SICOR 4.12 (COMPLEMENTO)      *
BRQ005*================================================================*
BRQ006*                    ALTERACAO - BRQ DIGITAL SOLUTIONS           *
BRQ006*----------------------------------------------------------------*
BRQ006*  ANALISTA BRQ..:   FERNANDO LUIZ DE SANTI                      *
BRQ006*  ANALISTA DS...:   RICARDO JAMMAL       - GRUPO 39             *
BRQ006*  DATA..........:   09/11/2018                                  *
BRQ006*----------------------------------------------------------------*
BRQ006*  OBJETIVO......:   BRQ 18/0477 - SICOR 4.12                    *
BRQ006*================================================================*
WIP001*                    ALTERACAO  -  WIPRO                         *
WIP001*----------------------------------------------------------------*
WIP001*  ANALISTA BRQ..:   FERNANDO LUIZ DE SANTI                      *
WIP001*  ANALISTA DS...:   RICARDO JAMMAL       - GRUPO 39             *
WIP001*  DATA..........:   11/06/2019                                  *
WIP001*----------------------------------------------------------------*
WIP001*  OBJETIVO......:   ISI 19/0352 - SICOR 4.13                    *
WIP001*    INCLUIR O CAMPO 48 V3-PERC-CET                              *
WIP001*================================================================*
QT1120*                    ALTERACAO  -  QINTESS                       *
QT1120*----------------------------------------------------------------*
QT1120*  ANALISTA      :   MARILIA SANTOS                              *
QT1120*  ANALISTA DS...:   PAULO ROBERTO SARTORATO                     *
QT1120*  DATA..........:   11/11/2020                                  *
QT1120*----------------------------------------------------------------*
QT1120*  OBJETIVO......:   ADEQUACAO SICOR 5.01                        *
QT1120*    ALTERACOES NA COR0001:                                      *
QT1120*                    - NOVO CAMPO "NUMERO REFERENCIA BACEN COR   *
QT1120*                                  DESCLASSIFICADO PARCIALMENTE" *
QT1120*                    - NOVO CAMPO "VALOR DESCLASSIFICADO"        *
QT1120*    ALTERACOES NA COR0005:                                      *
QT1120*                    - RETIRADA DO CAMPO "PERCENTUAL DESCLASSIFI *
QT1120*                                         CACAO"                 *
QT1120*================================================================*
BR0521*                    ALTERACAO  -  BRQ DIGITAL SOLUTIONS         *
BR0521*----------------------------------------------------------------*
BR0521*  ANALISTA......:   SIMONE                                      *
BR0521*  ANALISTA DS...:   PAULO ROBERTO SARTORATO                     *
BR0521*  DATA..........:   28/05/2021                                  *
BR0521*----------------------------------------------------------------*
BR0521*  OBJETIVO......:   ADEQUACOES PARA SICOR 5.02                  *
BR0521*================================================================*
WP1021*---------------------------------------------------------------*
WP1021* ALTERACAO...........: INCLUSAO DOS CAMPOS SICOR 5.03          *
.     *                       TIPO DE SOLO                            *
.     *                       CICLO DE CULTIVO                        *
.     *                       TIPO DE MANEJO                          *
WP1021* GERACAO.............: 10/2021                                 *
WIP001*---------------------------------------------------------------*
-     *                    ALTERACAO  -  WIPRO                        *
-     *---------------------------------------------------------------*
-     * ALTERACAO...........: ALTERACAO DE CAMPO SICOR 5.03           *
-     *                       COD-SIST-PRODC                          *
-     * GERACAO.............: 11/2022                                 *
WIP001*---------------------------------------------------------------*
      *================================================================*
      *                   A L T E R A C A O                            *
      *----------------------------------------------------------------*
F2404 *  ANALISTA......:  FERNANDA RODRIGUES   -   BRADESCO GRP 39     *
      *  DATA..........:  ABRIL/2024                                   *
      *----------------------------------------------------------------*
      *  OBJETIVO......:  AJUSTE COR0001 PARA LAYOUT SICOR 5.08        *
      *================================================================*
      *                   A L T E R A C A O                            *
      *----------------------------------------------------------------*
F2410 *  ANALISTA......:  FERNANDA RODRIGUES   -   BRADESCO GRP 39     *
      *  DATA..........:  OUTUBRO/2024                                 *
      *----------------------------------------------------------------*
      *  OBJETIVO......:  AJUSTE COR0001 PARA LAYOUT SICOR 5.09        *
      *================================================================*
      *                   A L T E R A C A O                            *
      *----------------------------------------------------------------*
V2506 *  ANALISTA......:  VAGNER MANFRINATO    -   BRADESCO
      *  DATA..........:  JUNHO/2025                                   *
      *----------------------------------------------------------------*
      *  OBJETIVO......:   ADEQUACAO SICOR 5.10                        *
      *                    ALTERACOES NA COR0001:                      *
      *                    - RETIRADA DOS CAMPOS:                      *
      *                      DAP BENEFICIARIO                          *
      *                      NIRF                                      *
      *                      INDICADOR CONFORMIDADE IBAMA              *
      *                      INDICADOR CONFORMIDADE UNIDADE CONSERVACAO*
      *                      INDICADOR CONFORMIDADE FLORESTA PUBLICA   *
      *                      INDICADOR CONFORMIDADE IMOVEL RURAL       *
      *                      TIPO CONFORMIDADE OUTROS                  *
      *                    - INCLUSAO CAMPOS NOVOS:                    *
      *                      CAF BENEFICIARIO                          *
      *                      CIB                                       *
      *                      PERCENTUAL BONUS SUSTENTAVEL              *
      *                      TIPO CONFORMIDADE                         *
      *                      GRUPO LIBERACAO                           *
      *                        - DATA LIBERACAO RECURSOS EMPREENDIMENTO*
      *                        - VALOR RECURSOS LIBERACAO              *
      *                      GRUPO COOPERADO                           *
      *                        - TIPO PESSOA COOPERADO                 *
      *                        - CNPJ OU CPF COOPERADO                 *
      *                        - VALOR PARCELA COOPERADO               *
      *                        - CODIGO PROGRAMA OU LINHA CREDITO      *
      *                          COOPERADO                             *
      *                    - INCLUIR DEFINICAO ALFANUMERICA:           *
      *                      CNPJ ENTIDADE RESPONSAVEL                 *
      *                      CNPJ OU CPF BENEFICIARIO                  *
      *                    - READEQUACAO DA SEQUENCIA DOS CAMPOS EM    *
      *                      FUNCAO DAS ATUALIZACOES.                  *
      *                    ALTERACOES NA COR0005:                      *
      *                    - RETIRADA DOS CAMPOS:                      *
      *                      NIRF                                      *
      *                    - INCLUSAO CAMPOS NOVOS:                    *
      *                      CIB                                       *
      *                      PERCENTUAL BONUS SUSTENTAVEL              *
      *                      TIPO CONFORMIDADE                         *
      *                    ALTERACOES NA COR0005R1:                    *
      *                    - INCLUSAO CAMPOS NOVOS:                    *
      *                        - GRUPO REJEICAO OPERACAO COR           *
      *                        - CODIGO REJEICAO OPERACAO COR          *
      *                        - TEXTO REJEICAO OPERACAO COR           *
      *================================================================*

      *----------------------------------------------------------------*
      *    LAYOUT PARA MENSAGEM COR0001                                *
      *----------------------------------------------------------------*

       01  V3-COR0001.
 1         05 V3-CODMSG                 PIC  X(009)        VALUE SPACES.
 2         05 V3-NUM-CTRL-IF            PIC  X(020)        VALUE SPACES.
 3         05 V3-CNPJ-ENT-RESPONS-X.
              10 V3-CNPJ-ENT-RESPONS    PIC  9(014)        VALUE ZEROS.
 4         05 V3-CNPJ-PART-X.
              10 V3-CNPJ-PART           PIC  9(014)        VALUE ZEROS.
 5         05 V3-NUM-REF-BCCO           PIC  X(011)        VALUE SPACES.
 6         05 V3-DT-EMS                 PIC  9(008)        VALUE ZEROS.
 7         05 V3-DT-VENC                PIC  9(008)        VALUE ZEROS.
 8         05 V3-COD-BASE-PRORROGC      PIC  X(003)        VALUE ZEROS.
 9         05 V3-NUM-CEDL-CRED-RURAL-IF PIC  X(017)        VALUE SPACES.
10         05 V3-TP-INSTNTO-CRED        PIC  X(002)        VALUE SPACES.

11         05 V3-VLR-TOT-OP-X.
              10 V3-VLR-TOT-OP-ZERO     PIC  9(001)        VALUE ZEROS.
              10 V3-VLR-TOT-OP          PIC  9(016)V9(002) VALUE ZEROS.

12         05 V3-COD-PORTE-BENFCRIO     PIC  X(004)        VALUE SPACES.

      *--- INICIO GRUPO 1000 BENEFICIARIO -----------------------------*
           05 FILLER                    PIC  X(006)        VALUE
13            '{1000}'.
           05 V3-GRP-BENFCRIO.
14            10 V3-TP-PESSOA-BENFCRIO  PIC  X(001)        VALUE SPACES.
15            10 V3-CNPJ-CPF-BENFCRIO-X.
                 20 V3-CNPJ-CPF-BENFCRIO
                                        PIC  9(014)        VALUE ZEROS.
16            10 V3-CAF-BENFCRIO        PIC  X(025)        VALUE SPACES.
17            10 V3-TP-BENFCRIO-COR     PIC  X(003)        VALUE SPACES.
18    *FIM GRUPO '{/1000}'.

      *--- INICIO GRUPO 2000 DESTINACAO FINANCIAMENTO -----------------*
           05 FILLER                    PIC  X(006)        VALUE
19            '{2000}'.
           05 V3-GRP-DESTC-FINCMNTO.

      *--- INICIO GRUPO 2100 RENEGOCIACAO -----------------------------*
              10 FILLER                 PIC  X(006)        VALUE
20               '{2100}'.
              10 V3-GRP-RENEG.
21               15 V3-NUM-REF-BCCO-RENEGC
                                        PIC  X(011)        VALUE SPACES.

22               15 V3-VLR-RENEGC-X.
                    20 V3-VLR-RENEGC-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-RENEGC-OP PIC  9(016)V9(002) VALUE ZEROS.

23            10 V3-COD-BASE-LEGAL-RENEGC
                                        PIC  X(003)        VALUE SPACES.
24    *FIM GRUPO '{/2100}'.

25            10 V3-COD-PROG-LINHA-CRED PIC  X(004)        VALUE SPACES.
26            10 V3-COD-SUBPROG-X.
                 15 V3-COD-SUBPROG      PIC  9(004)        VALUE ZEROS.

27            10 V3-TP-FNTE-REC         PIC  X(004)        VALUE SPACES.
28            10 V3-COD-MUNIC           PIC  9(006)        VALUE ZEROS.

      *--- INICIO GRUPO 2200 GLEBA ------------------------------------*
              10 FILLER                 PIC  X(006)        VALUE
29               '{2200}'.
              10 V3-GRP-GLEBA.
30               15 V3-IDENTC-GLEBA-X.
                    20 V3-IDENTC-GLEBA-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-IDENTC-GLEBA  PIC  9(018)        VALUE ZEROS.

      *--- INICIO GRUPO 2210 PONTO POLIGONO ---------------------------*
                 15 FILLER              PIC  X(006)        VALUE
                    '{2210}'.
31               15 V3-GRP-PONTO-POLG.
32                  20 V3-LAT-PONTO-X.
                       25 V3-LAT-PONTO  PIC  9(002)V9(011) VALUE ZEROS.
33                  20 V3-LONG-PONTO-X.
                       25 V3-LONG-PONTO PIC  9(003)V9(011) VALUE ZEROS.
34                  20 V3-ALT-PONTO-X.
                       25 V3-ALT-PONTO  PIC  9(004)V9(002) VALUE ZEROS.
35    *FIM GRUPO '{/2210}'.

36               15 V3-AREA-NCULTD-X.
                    20 V3-AREA-NCULTD   PIC  9(009)V9(002) VALUE ZEROS.

      *--- INICIO GRUPO 2220 GLEBA EXCLUIDA ---------------------------*
37               15 FILLER              PIC  X(006)        VALUE
                    '{2220}'.

      *--- INICIO GRUPO 2221 PONTO POLIGONO EXCLUIDO  -----------------*
38               15 FILLER              PIC  X(006)        VALUE
                    '{2221}'.
                 15 V3-GRP-PONTO-POLG-EXCL.
39                  20 V3-LAT-EXCL-X.
                       25 V3-LAT-EXCL   PIC  9(002)V9(011) VALUE ZEROS.
40                  20 V3-LONG-EXCL-X.
                       25 V3-LONG-EXCL  PIC  9(003)V9(011) VALUE ZEROS.
41                  20 V3-ALT-EXCL-X.
                       25 V3-ALT-EXCL   PIC  9(004)V9(002) VALUE ZEROS.
42    *FIM GRUPO '{/2221}'.
43    *FIM GRUPO '{/2220}'.
44    *FIM GRUPO '{/2200}'.

              10 FILLER                 PIC  X(006)        VALUE
                 '{2300}'.
45            10 V3-COD-PRODT-CONSCD    PIC  X(004)        VALUE SPACES.
46            10 V3-COD-EMPNMNT         PIC  X(014)        VALUE SPACES.
47            10 V3-COD-SIST-PRODC      PIC  X(013)        VALUE SPACES.

48            10 V3-VLR-PARCL-CRED-X.
                 15 V3-VLR-PARCL-CRED-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                 15 V3-VLR-PARCL-CRED   PIC  9(016)V9(002) VALUE ZEROS.

49            10 V3-VLR-PARCL-REC-PROPRIO-X.
                 15 V3-VLR-PARCL-REC-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                 15 V3-VLR-PARCL-REC-PROPRIO
                                        PIC  9(016)V9(002) VALUE ZEROS.

50            10 V3-VLR-GAR-RENDA-MIN-X.
                 15 V3-VLR-GAR-RENDA-MIN-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                 15 V3-VLR-GAR-RENDA-MIN
                                        PIC  9(016)V9(002) VALUE ZEROS.

51            10 V3-ALQ-PROAGRO-X.
                 15 V3-ALQ-PROAGRO      PIC S9(003)V9(002) VALUE ZEROS.

52            10 V3-PJUROS-ENCARGO-FINANC
                                        PIC S9(003)V9(002) VALUE ZEROS.

53            10 V3-TP-ENC-FINANC-COMPLTAR-X.
                 15 V3-TP-ENC-FINANC-COMPLTAR
                                        PIC  9(002)        VALUE ZEROS.

54            10 V3-PERC-RSC-STN-X.
                 15 V3-PERC-RSC-STN     PIC S9(003)V9(002) VALUE ZEROS.

55            10 V3-PERC-RSC-FUND-CONSCL-X.
                 15 V3-PERC-RSC-FUND-CONSCL
                                        PIC S9(003)V9(002) VALUE ZEROS.

56            10 V3-PERC-CET-X.
                 15 V3-PERC-CET         PIC  9(003)V9(002) VALUE ZEROS.

57            10 V3-AREA-X.
                 15 V3-AREA             PIC  9(009)V9(002) VALUE ZEROS.

58            10 V3-QTD-ITEM-FINCD      PIC  9(012)V9(002) VALUE ZEROS.
59            10 V3-QTD-PRV-PRODC-X.
                 15 V3-QTD-PRV-PRODC    PIC  9(012)V9(002) VALUE ZEROS.

      *--- INICIO GRUPO 2400 PROPRIETARIO -----------------------------*
              10 FILLER                 PIC  X(006)        VALUE
60               '{2400}'.
              10 V3-GRP-PROPT.
61               15 V3-TP-PESSOA-PROPT  PIC  X(001)        VALUE SPACES.
62               15 V3-CNPJ-BASE-CPF-PROPT
                                        PIC  X(011)        VALUE SPACES.
63            10 V3-CIB                 PIC  X(008)        VALUE SPACES.
64            10 V3-COD-SNCR            PIC  X(013)        VALUE SPACES.
65            10 V3-NUM-REG-CAR         PIC  X(041)        VALUE SPACES.

66            10 V3-COUTGA-AGUA         PIC  X(030)        VALUE SPACES.
67            10 V3-PPRESV-AMBTL-X.
                 15 V3-PPRESV-AMBTL     PIC  9(003)V9(002) VALUE ZEROS.
68    *FIM GRUPO '{/2400}'.

69            10 V3-COD-MOD-SEGR-X.
                 15 V3-COD-MOD-SEGR     PIC  9(001)        VALUE ZEROS.

      *--- INICIO GRUPO PROAGRO-MAIS ----------------------------------*
70            10 V3-GRP-PROAGRO-MAIS.
71               15 V3-CNPJ-BASE-INST-MUTNTE
                                        PIC  X(008)        VALUE SPACES.
72               15 V3-NREF-BCCOR-INVEST-RUR
                                        PIC  X(011)        VALUE SPACES.
73               15 V3-VLR-PARCL-INVESTMTO-X.
                    20 V3-VLR-PARCL-INVESTMTO-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-PARCL-INVESTMTO
                                        PIC  9(016)V9(002) VALUE ZEROS.
74    *FIM GRUPO PROAGRO-MAIS

75            10 V3-VLR-RECBRUT-EMPNMNT-X.
                 15 V3-VLR-RECBRUT-EMPNMNT-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                 15 V3-VLR-RECBRUT-EMPNMNT
                                        PIC  9(016)V9(002) VALUE ZEROS.

      *--- INICIO GRUPO 2500 PARCELA ----------------------------------*
              10 FILLER                 PIC  X(006)        VALUE
76               '{2500}'.
              10 V3-GRP-PARCELA.
77               15 V3-NUM-PARCL        PIC  9(003)        VALUE ZEROS.
78               15 V3-DT-PRV-PGTO      PIC  9(008)        VALUE ZEROS.
79               15 V3-VLR-PRINCIPAL-PARCL-X.
                    20 V3-VLR-PRINCIPAL-PARCL-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-PRINCIPAL-PARCL
                                        PIC  9(016)V9(002) VALUE ZEROS.
80    *FIM GRUPO '{/2500}'.

81            10 V3-DT-INI-PLANT        PIC  X(008)        VALUE SPACES.
82            10 V3-DT-FIM-PLANT        PIC  X(008)        VALUE SPACES.
83            10 V3-DT-INI-COLHT        PIC  X(008)        VALUE SPACES.
84            10 V3-DT-FIM-COLHT        PIC  X(008)        VALUE SPACES.

      *--- INICIO GRUPO 2600 DESCLASSIFICADO PARCIALMENTE -------------*
              10 FILLER                 PIC  X(006)        VALUE
85               '{2600}'.
              10 V3-GRP-REF-DESCLASCD-PARCLMNT.
86               15 V3-NUM-REF-BCCO-DESCL
                                        PIC  X(011)        VALUE SPACES.
87               15 V3-VLR-DESCLASCD-X.
                    20 V3-VLR-DESCLASCD-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-DESCLASCD
                                        PIC  9(016)V9(002) VALUE ZEROS.

88               15 V3-MOTVO-DESCLASS   PIC  X(003)        VALUE SPACES.
89    *FIM GRUPO '{/2600}'.

90            10 V3-COD-STN             PIC  X(013)        VALUE SPACES.

      *--- INICIO GRUPO CERTIFICACAO ORGANICA -------------------------*
91            10 V3-GRP-CERTF-ORGNCA.
92               15 V3-CCETFC-ORGNC-EMPTO
                                        PIC  X(080)        VALUE SPACES.
93               15 V3-EURL-CETFC-ORGNC PIC  X(255)        VALUE SPACES.
94    *FIM GRUPO CERTIFICACAO ORGANICA

      *--- INICIO GRUPO RASTREABILIDADE EMPREENDIMENTO ----------------*
95            10 V3-GRP-RASTLDD-EMP.
96               15 V3-CRSTRB-EMPTO     PIC  X(080)        VALUE SPACES.
97               15 V3-EURL-RSTRB       PIC  X(255)        VALUE SPACES.
98    *FIM GRUPO RASTREABILIDADE EMPREENDIMENTO

99            10 V3-PENERG-RENOV-X.
                 15 V3-PENERG-RENOV     PIC  9(003)V9(02)  VALUE ZEROS.

100   *--- INICIO GRUPO ZARC ------------------------------------------*
101           10 V3-CICLO-CULTURA       PIC  X(002)        VALUE ZEROS.
102           10 V3-TIPO-SOLO           PIC  X(002)        VALUE ZEROS.
103           10 V3-TIPO-MANEJO         PIC  X(002)        VALUE ZEROS.
104   *FIM GRUPO ZARC

105           10 V3-PER-BONUS-CAR-X.
                 15 V3-PER-BONUS-CAR    PIC  9(003)V9(02)  VALUE ZEROS.
106           10 V3-CNPJ-BASE-INST-EXIG PIC  X(008)        VALUE ZEROS.

107           10 V3-PER-BONUS-SUS-X.
                 15 V3-PER-BONUS-SUS    PIC  9(003)V9(02)  VALUE ZEROS.

      *--- INICIO GRUPO 2700 TIPO CONFORMIDADE ------------------------*
              10 FILLER                 PIC  X(006)        VALUE
                 '{2700}'.
108           10 V3-TIPO-CONFORMIDADE   PIC  X(002)        VALUE ZEROS.
      *FIM GRUPO '{/2700}'.

      *--- INICIO GRUPO 2800 LIBERACAO --------------------------------*
109           10 FILLER                 PIC  X(006)        VALUE
                 '{2800}'.
              10 V3-GRP-LIBRC.
110              15 V3-DT-LIB-REC-EMPREED-X.
                    20 V3-DT-LIB-REC-EMPREED
                                        PIC  9(008)        VALUE ZEROS.
111              15 V3-VLR-REC-LIBRC-X.
                    20 V3-VLR-REC-LIBRC-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-REC-LIBRC PIC  9(016)V9(002) VALUE ZEROS.
112   *FIM GRUPO '{/2800}'.

      *--- INICIO GRUPO 2900 COOPERADO --------------------------------*
113           10 FILLER                 PIC  X(006)        VALUE
                 '{2900}'.
              10 V3-GRP-COOPERD.
114              15 V3-TP-PESSOA-COOPERD
                                        PIC  X(001)        VALUE SPACES.
115              15 V3-CNPJ-CPF-COOPERD-X.
                    20 V3-CNPJ-CPF-COOPERD
                                        PIC  9(014)        VALUE ZEROS.
116              15 V3-VLR-PARCL-COOPERD-X.
                    20 V3-VLR-PARCL-COOPERD-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-PARCL-COOPERD
                                        PIC  9(016)V9(002) VALUE ZEROS.
117              15 V3-COD-PROG-COOPERD PIC  X(004)        VALUE SPACES.
118   *FIM GRUPO '{/2900}'.
119   *FIM GRUPO '{/2000}'

120        05 V3-DT-MOVTO               PIC  9(008)        VALUE ZEROS.

      *----------------------------------------------------------------*
      *    LAYOUT PARA MENSAGEM COR0001R1                              *
      *----------------------------------------------------------------*

       01  V3-COR0001R1.
1          05 V3-COD-MSG               PIC  X(009)         VALUE SPACES.
2          05 V3-NRO-CTRLIF            PIC  X(020)         VALUE SPACES.
3          05 V3-CNPJ-ENT-RESP-X.
              10 V3-CNPJ-ENT-RESP      PIC  9(014)         VALUE ZEROS.
4          05 V3-NRO-REF-BCCOR         PIC  X(011)         VALUE SPACES.
           05 V3-CONTEUDO              PIC  X(3946)        VALUE SPACES.

      *----------------------------------------------------------------*
      *    LAYOUT PARA MENSAGEM COR0001E                               *
      *----------------------------------------------------------------*

       01  V3-COR0001E.
1          05 V3-CODMSG-R               PIC  X(009)        VALUE SPACES.
           05 V3-CODMSG-E               PIC  X(008)        VALUE SPACES.

2          05 V3-NUM-CTRL-IF-R          PIC  X(020)        VALUE SPACES.
           05 V3-NUM-CTRL-IF-E          PIC  X(008)        VALUE SPACES.

3          05 V3-CNPJ-ENT-RESPONS-X-R.
              10 V3-CNPJ-ENT-RESPONS-R  PIC  9(014)        VALUE ZEROS.
           05 V3-CNPJ-ENT-RESPONS-E     PIC  X(008)        VALUE SPACES.

4          05 V3-CNPJ-PART-X-R.
              10 V3-CNPJ-PART-R         PIC  9(014)        VALUE ZEROS.
           05 V3-CNPJ-PART-E            PIC  X(008)        VALUE SPACES.

5          05 V3-NUM-REF-BCCO-R         PIC  X(011)        VALUE SPACES.
           05 V3-NUM-REF-BCCO-E         PIC  X(008)        VALUE SPACES.

6          05 V3-DT-EMS-R               PIC  9(008)        VALUE ZEROS.
           05 V3-DT-EMS-E               PIC  X(008)        VALUE SPACES.

7          05 V3-DT-VENC-R              PIC  9(008)        VALUE ZEROS.
           05 V3-DT-VENC-E              PIC  X(008)        VALUE SPACES.

8          05 V3-COD-BASE-PRORROGC-R    PIC  X(003)        VALUE SPACES.
           05 V3-COD-BASE-PRORROGC-E    PIC  X(008)        VALUE SPACES.

9          05 V3-NUM-CEDL-CRED-RURAL-IF-R
                                        PIC  X(017)        VALUE SPACES.
           05 V3-NUM-CEDL-CRED-RURAL-IF-E
                                        PIC  X(008)        VALUE SPACES.

10         05 V3-TP-INSTNTO-CRED-R      PIC  X(002)        VALUE SPACES.
           05 V3-TP-INSTNTO-CRED-E      PIC  X(008)        VALUE SPACES.

11         05 V3-VLR-TOT-OP-X-R.
              10 V3-VLR-TOT-OP-ZERO-R   PIC  9(001)        VALUE ZEROS.
              10 V3-VLR-TOT-OP-R        PIC  9(016)V9(002) VALUE ZEROS.
           05 V3-VLR-TOT-OP-E           PIC  X(008)        VALUE SPACES.

12         05 V3-COD-PORTE-BENFCRIO-R   PIC  X(004)        VALUE SPACES.
           05 V3-COD-PORTE-BENFCRIO-E   PIC  X(008)        VALUE SPACES.

           05 FILLER                    PIC  X(006)        VALUE
13            '{1000}'.
           05 V3-GRP-BENFCRIO-R.
14            10 V3-TP-PESSOA-BENFCRIO-R
                                        PIC  X(001)        VALUE SPACES.
              10 V3-TP-PESSOA-BENFCRIO-E
                                        PIC  X(008)        VALUE SPACES.

15            10 V3-CNPJ-CPF-BENFCRIO-X-R.
                 20 V3-CNPJ-CPF-BENFCRIO-R
                                        PIC  9(014)        VALUE ZEROS.
              10 V3-CNPJ-CPF-BENFCRIO-E PIC  X(008)        VALUE SPACES.

16            10 V3-CAF-BENFCRIO-R      PIC  X(025)        VALUE SPACES.
              10 V3-CAF-BENFCRIO-E      PIC  X(008)        VALUE SPACES.

17            10 V3-TP-BENFCRIO-COR-R   PIC  X(003)        VALUE SPACES.
              10 V3-TP-BENFCRIO-COR-E   PIC  X(008)        VALUE SPACES.
18    *FIM GRUPO '{/1000}'.

           05 FILLER                    PIC  X(006)        VALUE
19            '{2000}'.
           05 V3-GRP-DSTINO-FINANC-R.
              10 FILLER                 PIC  X(006)        VALUE
20               '{2100}'.
              10 V3-GRP-RENEG-R.
21               15 V3-NUM-REF-BCCO-RENEGC-R
                                        PIC  X(011)        VALUE SPACES.
                 15 V3-NUM-REF-BCCO-RENEGC-E
                                        PIC  X(008)        VALUE SPACES.

22               15 V3-VLR-RENEGC-X-R.
                    20 V3-VLR-RENEGC-ZERO-R
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-RENEGC-OP-R
                                        PIC  9(016)V9(002) VALUE ZEROS.
                 15 V3-VLR-RENEGC-E     PIC  X(008)        VALUE SPACES.

23            10 V3-COD-BASE-LEGAL-RENEGC-R
                                        PIC  X(003)        VALUE SPACES.
              10 V3-COD-BASE-LEGAL-RENEGC-E
                                        PIC  X(008)        VALUE SPACES.
24    *FIM GRUPO '{/2100}'.

25            10 V3-COD-PROG-LINHA-CRED-R
                                        PIC  X(004)        VALUE SPACES.
              10 V3-COD-PROG-LINHA-CRED-E
                                        PIC  X(008)        VALUE SPACES.

26            10 V3-COD-SUBPROG-R-X.
                 15 V3-COD-SUBPROG-R    PIC  9(004)        VALUE ZEROS.
              10 V3-COD-SUBPROG-E       PIC  X(008)        VALUE SPACES.

27            10 V3-TP-FNTE-REC-R       PIC  X(004)        VALUE SPACES.
              10 V3-TP-FNTE-REC-E       PIC  X(008)        VALUE SPACES.

28            10 V3-COD-MUNIC-R         PIC  9(006)        VALUE ZEROS.
              10 V3-COD-MUNIC-E         PIC  X(008)        VALUE SPACES.

              10 FILLER                 PIC  X(006)        VALUE
29               '{2200}'.
              10 V3-GRP-GLEBA-R.
30               15 V3-IDENTC-GLEBA-X-R.
                    20 V3-IDENTC-GLEBA-ZERO-R
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-IDENTC-GLEBA-R
                                        PIC  9(018)        VALUE ZEROS.
                 15 V3-IDENTC-GLEBA-E   PIC  X(008)        VALUE SPACES.

                 15 FILLER              PIC  X(006)        VALUE
31                  '{2210}'.
                 15 V3-GRP-PONTO-POLG-R.
32                  20 V3-LAT-PONTO-R-X.
                      25 V3-LAT-PONTO-R PIC  9(002)V9(011) VALUE ZEROS.
                    20 V3-LAT-PONTO-E   PIC  X(008)        VALUE SPACES.

33                  20 V3-LONG-PONTO-R-X.
                      25 V3-LONG-PONTO-R
                                        PIC  9(003)V9(011) VALUE ZEROS.
                    20 V3-LONG-PONTO-E  PIC  X(008)        VALUE SPACES.

34                  20 V3-ALT-PONTO-R-X.
                      25 V3-ALT-PONTO-R PIC  9(004)V9(002) VALUE ZEROS.
                    20 V3-ALT-PONTO-E   PIC  X(008)        VALUE SPACES.
35    *FIM GRUPO '{/2210}'.

36               15 V3-AREA-NCULTD-R-X.
                    20 V3-AREA-NCULTD-R PIC  9(009)V9(002) VALUE ZEROS.
                 15 V3-AREA-NCULTD-E    PIC  X(008)        VALUE SPACES.

37               15 FILLER              PIC  X(006)        VALUE
                 '{2220}'.

38               15 FILLER              PIC  X(006)        VALUE
                 '{2221}'.
                 15 V3-GRP-PONTO-POLG-EXCL-R.
39                  20 V3-LAT-EXCL-R-X.
                      25 V3-LAT-EXCL-R  PIC  9(002)V9(011) VALUE ZEROS.
                    20 V3-LAT-EXCL-E    PIC  X(008)        VALUE SPACES.

40                  20 V3-LONG-EXCL-R-X.
                      25 V3-LONG-EXCL-R
                                        PIC  9(003)V9(011) VALUE ZEROS.
                    20 V3-LONG-EXCL-E   PIC  X(008)        VALUE SPACES.

41                  20 V3-ALT-EXCL-R-X.
                      25 V3-ALT-EXCL-R  PIC  9(004)V9(002) VALUE ZEROS.
                    20 V3-ALT-EXCL-E    PIC  X(008)        VALUE SPACES.

42    *FIM GRUPO '{/2221}'.
43    *FIM GRUPO '{/2220}'.
44    *FIM GRUPO '{/2200}'.

              10 FILLER                 PIC  X(006)        VALUE
                 '{2300}'.
45            10 V3-COD-PRODT-CONSCD-R  PIC  X(004)        VALUE SPACES.
              10 V3-COD-PRODT-CONSCD-E  PIC  X(008)        VALUE SPACES.

46            10 V3-COD-EMPNMNT-R       PIC  X(014)        VALUE SPACES.
              10 V3-COD-EMPNMNT-E       PIC  X(008)        VALUE SPACES.

47            10 V3-COD-SIST-PRODC-R    PIC  X(013)        VALUE SPACES.
              10 V3-COD-SIST-PRODC-E    PIC  X(008)        VALUE SPACES.

48            10 V3-VLR-PARCL-CRED-X-R.
                  15 V3-VLR-PARCL-CRED-ZERO-R
                                        PIC  9(001)        VALUE ZEROS.
                 15 V3-VLR-PARCL-CRED-R PIC  9(016)V9(002) VALUE ZEROS.
              10 V3-VLR-PARCL-CRED-E    PIC  X(008)        VALUE SPACES.

49            10 V3-VLR-PARCL-REC-PROPRIO-X-R.
                 15 V3-VLR-PARCL-REC-ZERO-R
                                        PIC  9(001)        VALUE ZEROS.
                 15 V3-VLR-PARCL-REC-PROPRIO-R
                                        PIC  9(016)V9(002) VALUE ZEROS.
              10 V3-VLR-PARCL-REC-PROPRIO-E
                                        PIC  X(008)        VALUE SPACES.

50            10 V3-VLR-GAR-RENDA-MIN-X-R.
                 15 V3-VLR-GAR-RENDA-MIN-ZER-R
                                        PIC  9(001)        VALUE ZEROS.
                 15 V3-VLR-GAR-RENDA-MIN-R
                                        PIC  9(016)V9(002) VALUE ZEROS.
              10 V3-VLR-GAR-RENDA-MIN-E PIC  X(008)        VALUE SPACES.

51            10 V3-ALQ-PROAGRO-X-R.
                 15 V3-ALQ-PROAGRO-R    PIC S9(003)V9(002) VALUE ZEROS.
              10 V3-ALQ-PROAGRO-E       PIC  X(008)        VALUE SPACES.

52            10 V3-PJUROS-ENCARGO-FINANC-R
                                        PIC S9(003)V9(002) VALUE ZEROS.
              10 V3-PJUROS-ENCARGO-FINANC-E
                                        PIC  X(008)        VALUE SPACES.

53            10 V3-TP-ENC-FINANC-COMPLTAR-X-R.
                 15 V3-TP-ENC-FINANC-COMPLTAR-R
                                        PIC  9(002)        VALUE ZEROS.
              10 V3-TP-ENC-FINANC-COMPLTAR-E
                                        PIC  X(008)        VALUE SPACES.

54            10 V3-PERC-RSC-STN-X-R.
                 15 V3-PERC-RSC-STN-R   PIC S9(003)V9(002) VALUE ZEROS.
              10 V3-PERC-RSC-STN-E      PIC  X(008)        VALUE SPACES.

55            10 V3-PERC-RSC-FUND-CONSCL-X-R.
                 15 V3-PERC-RSC-FUND-CONSCL-R
                                        PIC S9(003)V9(002) VALUE ZEROS.
              10 V3-PERC-RSC-FUND-CONSCL-E
                                        PIC  X(008)        VALUE SPACES.

56            10 V3-PERC-CET-R-X.
                 15 V3-PERC-CET-R       PIC  9(003)V9(002) VALUE ZEROS.
              10 V3-PERC-CET-E          PIC  X(008)        VALUE SPACES.

57            10 V3-AREA-R-X.
                 15 V3-AREA-R           PIC  9(009)V9(002) VALUE ZEROS.
              10 V3-AREA-E              PIC  X(008)        VALUE SPACES.

58            10 V3-QTD-ITEM-FINCD-R    PIC  9(012)V9(002) VALUE ZEROS.
              10 V3-QTD-ITEM-FINCD-E    PIC  X(008)        VALUE SPACES.

59            10 V3-QTD-PRV-PRODC-R-X.
                 15 V3-QTD-PRV-PRODC-R  PIC  9(012)V9(002) VALUE ZEROS.
              10 V3-QTD-PRV-PRODC-E     PIC  X(008)        VALUE SPACES.

              10 FILLER                 PIC  X(006)        VALUE
60               '{2400}'.
              10 V3-GRP-PROPT-R.
61               15 V3-TP-PESSOA-PROPT-R
                                        PIC  X(001)        VALUE SPACES.
                 15 V3-TP-PESSOA-PROPT-E
                                        PIC  X(008)        VALUE SPACES.

62               15 V3-CNPJ-BASE-CPF-PROPT-R
                                        PIC  X(011)        VALUE SPACES.
                 15 V3-CNPJ-BASE-CPF-PROPT-E
                                        PIC  X(008)        VALUE SPACES.

63               15 V3-CIB-R            PIC  X(008)        VALUE SPACES.
                 15 V3-CIB-E            PIC  X(008)        VALUE SPACES.

64               15 V3-COD-SNCR-R       PIC  X(013)        VALUE SPACES.
                 15 V3-COD-SNCR-E       PIC  X(008)        VALUE SPACES.

65               15 V3-NUM-REG-CAR-R    PIC  X(041)        VALUE SPACES.
                 15 V3-NUM-REG-CAR-E    PIC  X(008)        VALUE SPACES.

66            10 V3-COUTGA-AGUA-R       PIC  X(030)        VALUE SPACES.
              10 V3-COUTGA-AGUA-E       PIC  X(008)        VALUE SPACES.

67            10 V3-PPRESV-AMBTL-R-X.
                 15 V3-PPRESV-AMBTL-R   PIC  9(003)V9(002) VALUE ZEROS.
              10 V3-PPRESV-AMBTL-E      PIC  X(008)        VALUE SPACES.

68    *FIM GRUPO '{/2400}'.

69            10 V3-COD-MOD-SEGR-X-R.
                 15 V3-COD-MOD-SEGR-R   PIC  9(001)        VALUE ZEROS.
              10 V3-COD-MOD-SEGR-E      PIC  X(008)        VALUE SPACES.

70            10 V3-GRP-PROAGRO-MAIS-R.
71               15 V3-CNPJ-BASE-INST-MUTNTE-R
                                        PIC  X(008)        VALUE SPACES.
                 15 V3-CNPJ-BASE-INST-MUTNTE-E
                                        PIC  X(008)        VALUE SPACES.

72               15 V3-NREF-BCCOR-INVEST-RUR-R
                                        PIC  X(011)        VALUE SPACES.
                 15 V3-NREF-BCCOR-INVEST-RUR-E
                                        PIC  X(008)        VALUE SPACES.

73               15 V3-VLR-PARCL-INVESTMTO-X-R.
                    20 V3-VLR-PARCL-INVESTMTO-ZERO-R
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-PARCL-INVESTMTO-R
                                        PIC  9(016)V9(002) VALUE ZEROS.
                 15 V3-VLR-PARCL-INVESTMTO-E
                                        PIC  X(008)        VALUE SPACES.
74    *FIM GRUPO PROAGRO-MAIS

75            10 V3-VLR-RECBRUT-EMPNMNT-X-R.
                 15 V3-VLR-RECBRUT-EMPNMNT-ZERO-R
                                        PIC  9(001)        VALUE ZEROS.
                 15 V3-VLR-RECBRUT-EMPNMNT-R
                                        PIC  9(016)V9(002) VALUE ZEROS.
              10 V3-VLR-RECBRUT-EMPNMNT-E
                                        PIC  X(008)        VALUE SPACES.

              10 FILLER                 PIC  X(006)        VALUE
76               '{2500}'.
              10 V3-GRP-PARCELA-R.
77               15 V3-NUM-PARCL-R      PIC  9(003)        VALUE ZEROS.
                 15 V3-NUM-PARCL-E      PIC  X(008)        VALUE SPACES.

78               15 V3-DT-PRV-PGTO-R    PIC  9(008)        VALUE ZEROS.
                 15 V3-DT-PRV-PGTO-E    PIC  X(008)        VALUE SPACES.

79               15 V3-VLR-PRINCIPAL-PARCL-X-R.
                    20 V3-VLR-PRINCIPAL-PARCL-ZERO-R
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-PRINCIPAL-PARCL-R
                                        PIC  9(016)V9(002) VALUE ZEROS.
                 15 V3-VLR-PRINCIPAL-PARCL-E
                                        PIC  X(008)        VALUE SPACES.
80    *FIM GRUPO '{/2500}'.

81            10 V3-DT-INI-PLANT-R      PIC  X(008)        VALUE SPACES.
              10 V3-DT-INI-PLANT-E      PIC  X(008)        VALUE SPACES.

82            10 V3-DT-FIM-PLANT-R      PIC  X(008)        VALUE SPACES.
              10 V3-DT-FIM-PLANT-E      PIC  X(008)        VALUE SPACES.

83            10 V3-DT-INI-COLHT-R      PIC  X(008)        VALUE SPACES.
              10 V3-DT-INI-COLHT-E      PIC  X(008)        VALUE SPACES.

84            10 V3-DT-FIM-COLHT-R      PIC  X(008)        VALUE SPACES.
              10 V3-DT-FIM-COLHT-E      PIC  X(008)        VALUE SPACES.

              10 FILLER                 PIC  X(006)        VALUE
85               '{2600}'.
              10 V3-GRP-REF-DESCLASC-PARCLMNT-R.
86               15 V3-NUM-REF-BCCO-DESCL-R
                                        PIC  X(011)        VALUE SPACES.
                 15 V3-NUM-REF-BCCO-DESCL-E
                                        PIC  X(008)        VALUE SPACES.
87               15 V3-VLR-DESCLASCD-X-R.
                    20 V3-VLR-DESCLASCD-ZERO-R
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-DESCLASCD-R
                                        PIC  9(016)V9(002) VALUE ZEROS.
                 15 V3-VLR-DESCLASCD-E  PIC  X(008)        VALUE SPACES.

88               15 V3-MOTVO-DESCLASS-R PIC  X(003)        VALUE SPACES.
                 15 V3-MOTVO-DESCLASS-E PIC  X(008)        VALUE SPACES.
89    *FIM GRUPO '{/2600}'.

90            10 V3-COD-STN-R           PIC  X(013)        VALUE SPACES.
              10 V3-COD-STN-E           PIC  X(008)        VALUE SPACES.

91            10 V3-GRP-CERTF-ORGNCA-R.
92               15 V3-CCETFC-ORGNC-EMPTO-R
                                        PIC  X(080)        VALUE SPACES.
                 15 V3-CCETFC-ORGNC-EMPTO-E
                                        PIC  X(008)        VALUE SPACES.

93               15 V3-EURL-CETFC-ORGNC-R
                                        PIC  X(255)        VALUE SPACES.
                 15 V3-EURL-CETFC-ORGNC-E
                                        PIC  X(008)        VALUE SPACES.
94    *FIM GRUPO CERTIFICACAO ORGANICA

95            10 V3-GRP-RASTLDD-EMP-R.
96               15 V3-CRSTRB-EMPTO-R   PIC  X(080)        VALUE SPACES.
                 15 V3-CRSTRB-EMPTO-E   PIC  X(008)        VALUE SPACES.

97               15 V3-EURL-RSTRB-R     PIC  X(255)        VALUE SPACES.
                 15 V3-EURL-RSTRB-E     PIC  X(008)        VALUE SPACES.
98    *FIM GRUPO RASTREABILIDADE EMPREENDIMENTO

99            10 V3-PENERG-RENOV-X-R.
                 15 V3-PENERG-RENOV-R   PIC  9(003)V9(02)  VALUE ZEROS.
                 15 V3-PENERG-RENOV-E   PIC  X(008)        VALUE SPACES.

100   *------- INICIO GRUPO ZARC
101           10 V3-CICLO-CULTURA-R     PIC  X(002)        VALUE SPACES.
              10 V3-CICLO-CULTURA-E     PIC  X(008)        VALUE SPACES.

102           10 V3-TIPO-SOLO-R         PIC  X(002)        VALUE SPACES.
              10 V3-TIPO-SOLO-E         PIC  X(008)        VALUE SPACES.

103           10 V3-TIPO-MANEJO-R       PIC  X(002)        VALUE SPACES.
              10 V3-TIPO-MANEJO-E       PIC  X(008)        VALUE SPACES.
104   *FIM GRUPO ZARC

105           10 V3-PER-BONUS-CAR-R-X.
                 15 V3-PER-BONUS-CAR-R  PIC  9(003)V9(02)  VALUE ZEROS.
              10 V3-PER-BONUS-CAR-E     PIC  X(008)        VALUE SPACES.

106           10 V3-CNPJ-BASE-EXIG-R    PIC  X(008)        VALUE SPACES.
              10 V3-CNPJ-BASE-EXIG-E    PIC  X(008)        VALUE SPACES.

107           10 V3-PER-BONUS-SUS-X-R.
                 15 V3-PER-BONUS-SUS-R  PIC  9(003)V9(02)  VALUE ZEROS.
              10 V3-PER-BONUS-SUS-E     PIC  X(008)        VALUE SPACES.

      *--- INICIO GRUPO 2700 TIPO CONFORMIDADE ------------------------*
              10 FILLER                 PIC  X(006)        VALUE
                 '{2700}'.
108           10 V3-TIPO-CONFORMIDADE-R PIC  X(002)        VALUE SPACES.
              10 V3-TIPO-CONFORMIDADE-E PIC  X(008)        VALUE SPACES.
      *FIM GRUPO '{/2700}'.

      *--- INICIO GRUPO 2800 LIBERACAO --------------------------------*
109           10 FILLER                 PIC  X(006)        VALUE
                 '{2800}'.
              10 V3-GRP-LIBRC-R.
110              15 V3-DT-LIB-REC-EMPREED-R
                                        PIC  9(008)        VALUE ZEROS.
                 15 V3-DT-LIB-REC-EMPREED-E
                                        PIC  X(008)        VALUE SPACES.
111              15 V3-VLR-REC-LIBRC-X-R.
                    20 V3-VLR-REC-LIBRC-R-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-REC-LIBRC-R
                                        PIC  9(016)V9(002) VALUE ZEROS.
                 15 V3-VLR-REC-LIBRC-E  PIC  X(008)        VALUE SPACES.
112   *FIM GRUPO '{/2800}'.

      *--- INICIO GRUPO 2900 COOPERADO --------------------------------*
113           10 FILLER                 PIC  X(006)        VALUE
                 '{2900}'.
              10 V3-GRP-COOPERD-R.
114              15 V3-TP-PESSOA-COOPERD-R
                                        PIC  X(001)        VALUE SPACES.
                 15 V3-TP-PESSOA-COOPERD-E
                                        PIC  X(008)        VALUE SPACES.
115              15 V3-CNPJ-CPF-COOPERD-X-R.
                    20 V3-CNPJ-CPF-COOPERD-R
                                        PIC  9(014)        VALUE ZEROS.
                 15 V3-CNPJ-CPF-COOPERD-E
                                        PIC  X(008)        VALUE SPACES.
116              15 V3-VLR-PARCL-COOPERD-X-R.
                    20 V3-VLR-PARCL-COOPERD-R-ZERO
                                        PIC  9(001)        VALUE ZEROS.
                    20 V3-VLR-PARCL-COOPERD-R
                                        PIC  9(016)V9(002) VALUE ZEROS.
                 15 V3-VLR-PARCL-COOPERD-E
                                        PIC  X(008)        VALUE SPACES.
117              15 V3-COD-PROG-COOPERD-R
                                        PIC  X(004)        VALUE SPACES.
                 15 V3-COD-PROG-COOPERD-E
                                        PIC  X(008)        VALUE SPACES.
118   *FIM GRUPO '{/2900}'.
119   *FIM GRUPO '{/2000}'

120        05 V3-DT-MOVTO-R             PIC  9(008)        VALUE ZEROS.
           05 V3-DT-MOVTO-E             PIC  X(008)        VALUE SPACES.
