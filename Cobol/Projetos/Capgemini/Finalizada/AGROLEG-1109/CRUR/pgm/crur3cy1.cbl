      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CRUR3CY1.
       AUTHOR.     RONALDO CORREA.
      *================================================================*
      *             C A P G E M I N I  -  S I S T E M A S              *
      *----------------------------------------------------------------*
      *    PROGRAMA....: CRUR3CY1                                      *
      *    PROGRAMADOR.: RONALDO CORREA          - CAPGEMINI - ALPHA   *
      *    ANALISTA CPM: ANISIO DOS SANTOS       - CAPGEMINI - ALPHA   *
      *    DATA........: 26/05/2014                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: FUNCIONAL - CARREGA CATALOGO                  *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TCTRL_ENVIO_INFO                              CRURB0U2      *
      *    TCONTR_CREDT_RURAL                            CRURB008      *
      *    TLIN_MSGEM_REMSS                              CRURB0UA      *
      *    TDOCTO_CREDT_RURAL                            CRURB061      *
      *    TRESP_UND_RURAL                               CRURB0MV      *
      *    TPRTCP_PSSOA_RURAL                            CRURB0MS      *
      *    TORIGE_REC_RURAL                              CRURB037      *
      *    TCOMPO_CONTR_CREDT                            CRURB0L7      *
      *    TORCAM_CONTR_CREDT                            CRURB0MB      *
      *    TPCELA_PGTO_CONTR                             CRURB0MK      *
      *    TLCTO_PCELA_RURAL                             CRURB0LZ      *
      *    TPPRIE_CREDT_RURAL                            CRURB0C4      *
      *    TLOTE_COMCZ_RURAL                             CRURB0SX      *
      *    TCLAUS_CONTR_RURAL                            CRURB0O0      *
      *    TINSTT_CREDT_BACEN                            CRURB0TF      *
      *    TEMPTO_RENDA_AGROP                            CRURB068      *
      *    TMODLD_RURAL                                  CRURB0B9      *
      *    TBEM_IMOV_BENEF                               CRURB0L4      *
      *    TAPOLC_SEGUR_BACEN                            CRURB0V3      *
RO1603*    TSIST_PROD_BACEN                              CRURB0T2      *
STE001*    TOUTRO_TRATO_SOLO                             CRURB0X1      *
STE004*    TOUTRO_CONFM_BACEN                            CRURB01F      *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#FRWKGE - COMMAREA FRWK1999 (LOG DE ERRO)                  *
      *    I#FRWKCI - COMMAREA FRWK1999 (LOG DE ERROS CICS)            *
      *    I#FRWKMD - COMMAREA FRWK1999 (LOG DE ERROS MODULO)          *
      *    I#FRWKDB - COMMAREA FRWK1999 (LOG DE ERROS DB2)             *
      *    CRURWIY1 - AREA DE COMUNICACAO COM CHAMADOR                 *
      *    UORGW000 - AREA DE COMUNICACAO (BOOK DE CONTROLE)           *
      *    UORGW22B - AREA DE COMUNICACAO COM MODULO UORG122B          *
      *    PSDCW00W - AREA DE COMUNICACAO (BOOK DE CONTROLE)           *
      *    PSDCWBSI - AREA DE COMUNICACAO COM MODULO PSDC3BSR          *
      *    PSDCW12I - AREA DE COMUNICACAO COM MODULO PSDC312C          *
      *    CRURWI42 - AREA DE COMUNICACAO COM MODULO CRUR3C42          *
STE005*    CRURW07V - GRURO DE INFORMACAO PROPRIETARIO(RETIRADO)       *
STE005*    CRURW27V - GRURO DE INFORMACAO PROPRIETARIO                 *
      *    CRURWWJI - AREA DE COMUNICACAO ENTRE O MODULO CRUR3WJL      *
EL0506*    CRURW14C COMMAREA ENTRE PROGRAMA FUNCIONAL E CRUR414S       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO.     *
      *    UORG122B - MODULO PARA BUSCAR NOME DA AGENCIA.              *
      *    PSDC3BSR - MODULO PARA CONVERTER USUARIO.                   *
      *    PSDC312C - MODULO PARA BUSCAR DADOS DO CLUB.                *
EL0506*    CRUR414S - OBTER DESCRICAO DO EMPREENDIMENTO                *
      *================================================================*
ED0604*================================================================*
 .    *    ALTERACAO - ED0604
 .    *    ANALISTA  - EDUARDO ADAIME CASTILHO
 .    *    OBJETIVO  - OBTER A ALIQUOTA DO SEGURO PROAGRO
ED0604*================================================================*
ED2804*================================================================*
 .    *    ALTERACAO - ED2804
 .    *    ANALISTA  - EDUARDO ADAIME CASTILHO
 .    *    OBJETIVO  - 1-CORRECAO DE MOVIMENTACAO DO JUROS DE ENCARGO
 .    *                2-CORRECAO DA MOVIMENTACAO DO MUN. DO IMOVEL
 .    *                3-CORRECAO DA MOVIMENTACAO DO SISTEMA DE PROD
 .    *                4-CORRECAO DE QUERY ALIQUOTA PROAGRO
 .    *                5-CORRECAO DE MOVIMENTACAO AREA FINANCIADA
 .    *
ED2804*================================================================*
ED0405*================================================================*
 .    *    ALTERACAO - ED0405
 .    *    ANALISTA  - EDUARDO ADAIME CASTILHO
 .    *    OBJETIVO  - OBTER COORDENADAS GEODESICAS PARA O SICOR
ED0405*================================================================*
ED2105*================================================================*
.     *                ALTERACAO - CAPGEMINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ALTERACAO..........: ED2105                                    *
.     * ANALISTA REPONSAVEL: EDUARDO ADAIME CASTILHO                   *
.     * DATA...............: 21/05/2015                                *
.     * OBJETIVOS..........: 1- PARA PROPOSTA COM IMOVEL UTILIZAR O
.     *                      CMUN DO IMOVEL DETALHADO
.     *                      2- PARA PROPOSTA DE COMERCIALIZACAO SEM
.     *                      IMOVEL DETALHADO UTLIZAR O CMUN DE CLAS-
.     *                      SIFICACAO DO PRODUTO
.     *                      3- PARA PROPOSTA DE CUSTEIO SEM IMOVEL
.     *                      DETALHADO UTILIZAR O CMUN DO DETALHE DO
.     *                      PRODUTO
ED2105*================================================================*
EL0506*================================================================*
.     *                ALTERACAO - CAPGEMINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ALTERACAO..........: EL0506                                    *
.     * ANALISTA REPONSAVEL: EVERTON LUIZ ROCHA LIMA                   *
.     * DATA...............: 05/06/2015                                *
.     * OBJETIVOS..........: 1- AJUSTE PARA ENVIAR PRODUTO CONSORCIADO *
.     *                      PARA O SICOR CASO O MESMO EXISTA NA       *
.     *                      PROPOSTA                                  *
EL0506*================================================================*
AS0615*================================================================*
.     *                ALTERACAO - CAPGEMINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ALTERACAO..........: AS0615                                    *
.     * ANALISTA REPONSAVEL: ALEXSSANDRO SANTANA                       *
.     * DATA...............: 17/06/2015                                *
.     * OBJETIVOS..........: 1- INCLUSAO DO CAMPO SUBPROGRAMA NA VERSAO*
.     *                      4.06 DO CATALOGO SICOR                    *
.     *                      2- SOLUCAO PALIATIVA PARA O CAMPO DE      *
.     *                      SUBPROGRAMA                               *
AS0615*================================================================*
AS3007*================================================================*
.     *                ALTERACAO - CAPGEMINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ALTERACAO..........: AS3007                                    *
.     * ANALISTA REPONSAVEL: ALEXSSANDRO SANTANA                       *
.     * DATA...............: 30/07/2015                                *
.     * OBJETIVOS..........: 1- MOVIMENTACAO DA RECEITA BRUDA PARA     *
.     *                      CUSTEIO E INVESTIMENTO PECUARIO           *
AS3007*================================================================*
EL0715*================================================================*
.     *                ALTERACAO - CAPGEMINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ALTERACAO..........: EL0715                                    *
.     * ANALISTA REPONSAVEL: EVERTON LUIZ ROCHA LIMA                   *
.     * DATA...............: 31/07/2015                                *
.     * OBJETIVOS..........: 1- AJUSTE PARA TRATAR QUANDO PROPOSTA NAO *
.     *                      POSSUIR IMOVEL                            *
EL0715*================================================================*
EL0815*================================================================*
.     *                ALTERACAO - CAPGEMINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ALTERACAO..........: EL0815                                    *
.     * ANALISTA REPONSAVEL: EVERTON LUIZ ROCHA LIMA                   *
.     * DATA...............: 05/08/2015                                *
.     * OBJETIVOS..........: 1- PARA FINALIDADE INVESTIMENTO A SAFRA   *
.     *                      DEVE SER FORMATADA COM O ANO DA CONTRATA- *
.     *                      CAO (TANTO NO ANO INICIAL QUANTO NO FINAL)*
.     *                      2- PARA FINALIDADE CUSTEIO OU COMERCIALIZA*
.     *                      CAO E ATIVIDADE PECUARIA, A SAFRA DEVE SER*
.     *                      FORMATADA COM O ANO DA CONTRATACAO (TANTO *
.     *                      NO ANO INICIAL QUANTO NO FINAL)           *
EL0815*================================================================*
EL0915*================================================================*
.     *                ALTERACAO - CAPGEMINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ALTERACAO..........: EL0915                                    *
.     * ANALISTA REPONSAVEL: EVERTON LUIZ ROCHA LIMA                   *
.     * DATA...............: 09/09/2015                                *
.     * OBJETIVOS..........: PARA FINALIDADE COMERCIALIZACAO MOVER A   *
.     *                      QUANTIDADE TOTAL SUMARIZADA DE NOTAS POR  *
.     *                      PRODUTO                                   *
EL0915*================================================================*
      *----------------------------------------------------------------*
      *    ALTERACOES..:                                               *
      *                                                                *
      * DATA       NOME        DESCRICAO                      TAG      *
      * ========== =========== ============================== =========*
      * 19/08/2015 JOAO PEDRO  ALTERACAO DE CAMPOS             P-01    *
      *            BASTOS                                              *
      *----------------------------------------------------------------*
EL1115*================================================================*
.     *                ALTERACAO - CAPGEMINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ALTERACAO..........: EL0915                                    *
.     * ANALISTA REPONSAVEL: EVERTON LUIZ ROCHA LIMA                   *
.     * DATA...............: 06/11/2015                                *
.     * OBJETIVOS..........: ENVIAR VALOR DE RECURSOS PROPRIOS PARA O  *
.     *                      SICOR                                     *
EL1115*================================================================*
EL0116*================================================================*
.     *                ALTERACAO - CAPGEMINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ALTERACAO..........: EL0116                                    *
.     * ANALISTA REPONSAVEL: EVERTON LUIZ ROCHA LIMA                   *
.     * DATA...............: 05/01/2016                                *
.     * OBJETIVOS..........: 1 - ALTERACAO DE LAYOUT DE COMUNICACAO COM*
.     *                      SICOR (COORDENADAS GEODESICAS).           *
.     *                      2 - FORMATACAO DA IDENTIFICACAO DA GLEBA, *
.     *                      NUMERO SEQUENCIAL PARA CADA IMOVEL DO     *
.     *                      CONTRATO QUE POSSUA COORDENADAS GEODESICAS*
.     *                      3 - PARA COMERCIALIZACAO, FORMATAR O      *
.     *                      CPF/CNPJ DO PROPRIETARIO COM O CPF/CNPJ   *
.     *                      DO BENEFICIARIO.                          *
EL0116*================================================================*
AS1202*================================================================*
.     *                ALTERACAO - CAPGEMINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ALTERACAO..........: AS1202                                    *
.     * ANALISTA REPONSAVEL: ALEXSSANDRO SANTANA                       *
.     * DATA...............: 12/02/2016                                *
.     * OBJETIVOS..........: ADEQUAR SOLUCAO PELIATIVA PARA O LAYOUT   *
.     *                      SICOR 4.07                                *
AS1202*================================================================*
ED2502*================================================================*
.     * ALTERACAO..........: ED2502
.     * ANALISTA REPONSAVEL: EDUARDO ADAIME CASTILHO
.     * DATA...............: 25/02/2016                                *
.     * OBJETIVOS..........: ADICIONAR MOVIMENTACAO DA DATA DE PLANTIO
.     *                      E DATA DE COLHEITA PARA MELHORIA CATALOGO
.     *                      SICOR 4.07
ED2502*================================================================*
ID5687*================================================================*
.     * ALTERACAO..........: ID5687
.     * ANALISTA REPONSAVEL: EVERTON LIMA
.     * DATA...............: 06/02/2017                                *
.     * OBJETIVOS..........: ALTERACAO PARA OBTER O PORTE DO GRUPO DE
.     *                      INFORMACAO, NAO MAIS DIRETAMENTE FA FICHA.
ID5687*================================================================*
ID6151*================================================================*
ID6151* ALTERACAO..........: ID6151
ID6151* ANALISTA REPONSAVEL: EVANDRO GUIMARAES
ID6151* DATA...............: 05/12/2017                                *
ID6151* OBJETIVOS..........: ENVIAR COD-MUN-GLEBA QUANDO IMOVEL POSSUIR
ID6151*                      COORDENADAS GEODESICAS PREENCHIDAS.
ID6151*================================================================*
ID6140*================================================================*
ID6140* ALTERACAO..........: ID6140
ID6140* ANALISTA REPONSAVEL: HEBE/EVANDRO GUIMARAES                    *
ID6140* DATA...............: 08/12/2017                                *
ID6140* OBJETIVOS..........: ATUIALIZAR O CAMPO CODIGO MUNICIPIO GLEBA *
ID6140*================================================================*
EG1117*================================================================*
.     * ALTERACAO..........: COORDENADAS GEODESICAS
.     * ANALISTA REPONSAVEL: EVANDRO GUIMARAES
.     * DATA...............: 11/2017                                   *
.     * OBJETIVOS..........: - INCLUIR IDENTIFICACAO DA GLEBA
.     *                      - ENVIAR MENSAGEM DE CONTINUACAO
EG1117*================================================================*
BR1218*================================================================*
BR1218*    DATA........: 12/2018                                       *
BR1218*    ANALISTA....: EVANDRO GUIMARAES - BRQ                       *
BR1218*    OBJETIVO....: EXIGIR AREA PARA TODOS OS EMPREENDIMENTOS     *
BR1218*                  COM COORDENADAS GEODESICAS OBRIGATORIAS       *
BR1218*================================================================*
ID6695*================================================================*
ID6695*    DATA........: 05/2019                                       *
ID6695*    ANALISTA....: HAILTON                                       *
ID6695*    OBJETIVO....: NEM TODOS OS NUMEROS DE CAR E PROPRIETARIOS   *
ID6695*                  ESTAO SENDO ENVIADOS.                         *
ID6695*================================================================*
WMM   *                    ALTERACAO - PRIME                           *
WMM   *----------------------------------------------------------------*
WMM   *  ANALISTA PRIME:   WALTER MESSAS                               *
WMM   *  DATA..........:   26/12/2019                                  *
WMM   *----------------------------------------------------------------*
WMM   *  OBJETIVO......:   ENCARGOS FINANCEIROS COMPLEMENTARES         *
WMM   *                    ALTERACAO REGRA DE ENVIO SICOR              *
WMM   *================================================================*
7C0120*================================================================*
.     *    DATA........: 01/2020                                       *
.     *    ANALISTA....: EDSON MATSUMOTO - 7COMM                       *
.     *    OBJETIVO....: ALTERAR A FORMA DE ENVIO DA CONTINUACAO, SO   *
.     *                  SERA ENVIADO DEPOIS DE RECEBER O RETORNO,     *
.     *                  POIS AS MSG ESTAO SENDO ENVIADOS FORA DE ORDEM*
7C0120*================================================================*
IN-BRQ*================================================================*
IN-BRQ*    TAG........:  IN-BRQ - IN3634714                            *
IN-BRQ*    PROGRAMADOR.: LEILA FREIRE - BRQ                            *
IN-BRQ*    DATA........: 02/2020                                       *
IN-BRQ*    OBJETIVO....: AS OPERACOES DE INDUSTRIALIZACAO COM RECURSOS *
IN-BRQ*                  DE PRONAF DEVEM UTILIZAR O SUBPROGRAMA 53     *
IN-BRQ*================================================================*
7C0120*================================================================*
ONEDA *================================================================*
ONEDA *    TAG........:  BRASDECO                                      *
ONEDA *    PROGRAMADOR.: ADILSON ONEDA                                 *
ONEDA *    DATA........: 23/07/2020                                    *
ONEDA *    OBJETIVO....: RETIRADA DE REGRAS DE COORDENADAS GEODESICAS  *
ONEDA *                  AS REGRAS FICARAM TODAS NO CRUR3OA6           *
ONEDA *================================================================*
QT1120*================================================================*
QT1120*                    ALTERACAO - QINTESS                         *
QT1120*----------------------------------------------------------------*
QT1120*  ANALISTA......:   QINTESS                                     *
QT1120*  DATA..........:   13/11/2020                                  *
QT1120*----------------------------------------------------------------*
QT1120*  OBJETIVO......:   ADEQUACAO SICOR 5.01                        *
QT1120*    ALTERACOES NA COR0001:                                      *
QT1120*                    - NOVO CAMPO "NUMERO REFERENCIA BACEN COR   *
QT1120*                                  DESCLASSIFICADO PARCIALMENTE" *
QT1120*                    - NOVO CAMPO "VALOR DESCLASSIFICADO"        *
QT1120*================================================================*
4SYS21*
4SYS21*----------------------------------------------------------------*
4SYS21*  ANALISTA......:   PAULA VILLAS BOAS - FOURSYS                 *
4SYS21*  DATA..........:   06/04/2021                                  *
4SYS21*----------------------------------------------------------------*
4SYS21*  OBJETIVO......:   COORDENADAS GEODESICAS                      *
4SYS21*                    - INFORMACAO CEXIGE-IMOV-BENEF SAIU DA TAB  *
4SYS21*                      TMODLD_RURAL E ESTA NA TBNEFC_MODLD_RURAL *
      *================================================================*
RO1603*    ANALISTA    : ROSANA DE LIMA OLIVEIRA - DS-RURAL            *
RO1603*    DATA        : 16/03/2021                                    *
RO1603*    OBJETIVO    : NOTICIAS SICOR 09/03/2021                     *
RO1603*    OBRIGATORIEDADE DE CAR E GLEBASï¿½-Â -ï¿½-Â VISANDO ADEQUACAO
RO1603*    SERAO CRIADOS NO SISTEMA DE PRODUCAO/TIPO DE                *
RO1603*    CULTIVO/EXPLORACAO PECUARIA AS VARIACOES 21-CRIACAO EM AREAS*
RO1603*    MARINHAS DELIMITADAS E 22-CRIACAO EM RANARIOS. PARA A       *
RO1603*    MODALIDADE 60-AQUICULTURA, A EXIGENCIA DE CAR SERA PARA OS  *
RO1603*    EMPREENDIMENTOS CUJO SISTEMA DE PRODUCAO TIPO DE CULTIVO/   *
RO1603*    EXPLORACAO PECUARIA SEJA DIFERENTE DE 18-CRIACAO EM TANQUES *
RO1603*    REDES/FLUXO CONTINUO E 21-CRIACAO EM AREAS MARINHAS         *
RO1603*    DELIMITADAS.                                                *
RO1603*                                                                *
      *================================================================*
BR0521*================================================================*
.     *  ANALISTA......:   SIMONE - BRQ                                *
.     *  DATA..........:   06/06/2021                                  *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.02                       *
BR0521*================================================================*
ONEDA *================================================================*
ONEDA *                      A L T E R A C A O                         *
ONEDA *================================================================*
ONEDA *    ANALISTA....:   ADILSON ONEDA    -   BRADESCO               *
ONEDA *    DATA........:   JULHO/2021                                  *
ONEDA *    OBJETIVO....:   NOTICIAS SICOR DE 07/05/2021                *
ONEDA *                    NAO ENVIAR AREA PARA OPERACOES COM COORDENA-*
ONEDA *                    DAS GEODESICAS                              *
ONEDA *================================================================*
BR0621*================================================================*
.     *  ANALISTA......:   SIMONE - BRQ                                *
.     *  DATA..........:   JULHO/2021                                  *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.02                       *
.     *                    FORMATACAO DOS CAMPOS                       *
BR0621*================================================================*
WP1021*================================================================*
.     *  ANALISTA......:   WIPRO                                       *
.     *  DATA..........:   OUTUBRO/2021                                *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.03                       *
.     *                    FORMATACAO DOS CAMPOS                       *
WP1021*================================================================*
MJ1221*  ANALISTA......:   MILTON JANUARIO        - WIPRO              *
MJ1221*  DATA..........:   10/12/2021                                  *
MJ1221*----------------------------------------------------------------*
MJ1221*  OBJETIVO......:   IN6400402 - ERRO NO ENVIO DO SUBPROGRAMA    *
MJ1221*                    PARA AS LINHAS DE CREDITO DO BACEN          *
MJ1221*================================================================*
WIP001*================================================================*
.     *  ANALISTA......:   WIPRO                                       *
.     *  DATA..........:   OUTUBRO/2022                                *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.05                       *
.     *                    FORMATACAO DOS CAMPOS                       *
WIP001*================================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
MJ1223*================================================================*
MJ1223*                ALTERACAO - BRADESCO                            *
MJ1223*----------------------------------------------------------------*
MJ1223*                                                                *
MJ1223* ANALISTA REPONSAVEL: MILTON JANUARIO                           *
MJ1223* DATA...............: 18/12/2023                                *
MJ1223* OBJETIVOS..........: ENVIO DATAS DE INICIO E FIM DE COLHEITA   *
MJ1223*                      CUSTEIO DE CULTURAS PERENES               *
MJ1223*================================================================*
STE001*================================================================*
.     *                ALTERACAO - BRADESCO                            *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ANALISTA REPONSAVEL: VICTORINO                                 *
.     * EMPRESA............: STEFANINI                                 *
.     * DATA...............: MAIO / 2024                               *
.     * OBJETIVOS..........: SICOR 5.08                                *
.     *                                                                *
STE002*================================================================*
.     *                ALTERACAO - STEFANINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ANALISTA REPONSAVEL: VICTORINO                                 *
.     * EMPRESA............: STEFANINI                                 *
.     * DATA...............: JULHO/ 2024                               *
.     * OBJETIVOS..........: SICOR 5.08 - INC0467861                   *
.     *                      AJUSTE ENVIO DOS INDICADORES CAR NA CONTI-*
.     *                      NUACAO DA COR0001                         *
STE002*================================================================*
MJ0924*                ALTERACAO - DS BRADESCO                         *
MJ0924*----------------------------------------------------------------*
MJ0924*                                                                *
MJ0924* ANALISTA REPONSAVEL: MILTON JANUARIO                           *
MJ0924* EMPRESA............: BRADESCO                                  *
MJ0924* DATA...............: SETEMBRO/2024                             *
MJ0924* OBJETIVOS..........: RO ADICIONAL - ENVIO DA FONTE DE RECURSOS *
MJ0924*                      0226 - EXIGIBILIDADE ADICIONAL DOS        *
MJ0924*                      RECURSOS A VISTA - RESOLUï¿½-Â§ï¿½-Â£O 5157
MJ0924*================================================================*
STE003*                ALTERACAO - STEFANINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ANALISTA REPONSAVEL: VICTORINO                                 *
.     * EMPRESA............: STEFANINI                                 *
.     * DATA...............: OUTUBRO/ 2024                             *
.     * OBJETIVOS..........: SICOR 5.08 -                              *
.     *                      AJUSTE ENVIO DA AREA NAO CULTIVADA        *
STE003*================================================================*
STE004*                ALTERACAO - STEFANINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ANALISTA REPONSAVEL: VICTORINO                                 *
.     * EMPRESA............: STEFANINI                                 *
.     * DATA...............: OUTUBRO/ 2024                             *
.     * OBJETIVOS..........: SICOR 5.09 -                              *
.     *                      ENVIO DO TIPO DE CONFORMIDADE             *
.     *                      NIRF DE 9 PARA 8 BYTES                    *
STE004*================================================================*
STE005*                ALTERACAO - STEFANINI                           *
.     *----------------------------------------------------------------*
.     *                                                                *
.     * ANALISTA REPONSAVEL: VICTORINO                                 *
.     * EMPRESA............: STEFANINI                                 *
.     * DATA...............: JANEIRO / 2025                            *
.     * OBJETIVOS..........: PROJETO UNIDADE ANIMAL                    *
STE005*================================================================*
.     * ANALISTA REPONSAVEL: MILTON JANUARIO                           *
.     * EMPRESA............: BRADESCO                                  *
.     * DATA...............: JULHO / 2025                              *
.     * OBJETIVOS..........: INC1041189 - CORRECAO ENVIO UNIDADE ANIMAL*
MJ0725*================================================================*
7C5.10*================================================================*
.     * ANALISTA REPONSAVEL: RUBEM TRONCARELLI                         *
.     * EMPRESA............: 7COMM                                     *
.     * DATA...............: AGOSTO / 2025                             *
.     * OBJETIVOS..........: ALTERACAO DE CAMPO SICOR 5.10             *
7C5.10*================================================================*
211025*  ANALISTA......:   7COMM - SQUAD FLASH - AGRO7-2707            *
.     *  DATA..........:   OUTUBRO / 2025                              *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES DE LAYOUT PARA SICOR 5.11        *
211025*                    NOVA MONTAGEM DO GRUPO GLEBA                *
      *================================================================*
      *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(32)           VALUE
           '** INICIO DA WORKING CRUR3CY1 **'.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
      *
      ***********************VARIAVEIS DE PROGRAMA
       01  WRK-AUXILIARES.
           05 WRK-PROGRAMA             PIC  X(008)     VALUE 'CRUR3CY1'.
           05 WRK-FRWK1999             PIC  X(008)     VALUE 'FRWK1999'.
           05 WRK-UORG122B             PIC  X(008)     VALUE 'UORG122B'.
           05 WRK-PSDC3BSR             PIC  X(008)     VALUE 'PSDC3BSR'.
           05 WRK-PSDC312C             PIC  X(008)     VALUE 'PSDC312C'.
           05 WRK-CRUR3C42             PIC  X(008)     VALUE 'CRUR3C42'.
           05 WRK-RCOR7000             PIC  X(008)     VALUE 'RCOR7000'.
           05 WRK-FNEB3BZL             PIC  X(008)     VALUE 'FNEB3BZL'.
           05 WRK-FNEB33PL             PIC  X(008)     VALUE 'FNEB33PL'.
           05 WRK-CRUR3WJL             PIC  X(008)     VALUE 'CRUR3WJL'.
EL0506     05 WRK-CRUR414S             PIC  X(008)     VALUE 'CRUR414S'.
ED1401     05 WRK-CRUR3G8L             PIC X(008)      VALUE 'CRUR3G8L'.
ID6181     05 WRK-CEPN301R             PIC  X(008)     VALUE 'CEPN301R'.
CET        05 WRK-CRUR3XVL             PIC  X(008)     VALUE 'CRUR3XVL'.
CET        05 WRK-CRUR3CCI             PIC  X(008)     VALUE 'CRUR3CCI'.
4SYS21     05 WRK-CRUR4F0C             PIC  X(008)     VALUE 'CRUR4F0C'.
MJ1224     05 WRK-CRUR3NIR             PIC  X(008)     VALUE 'CRUR3NIR'.
STE003******5 WRK-MDGLEBA-X.
STE003******  *7 *ILLER                *IC 9(014) *ALUE *EROS.
STE003******  *7 *RK-MDGLEBA-9         *IC 9(005) *ALUE *EROS.
STE003     05 WRK-TEM-NCULT            PIC X(001) VALUE 'S'.
7C5.10     05 WRK-TEM-INDICADOR        PIC X(001) VALUE 'N'.
STE003     05 IND-PRIMEIRA             PIC 9(005) COMP-3
STE003                                                 VALUE ZEROS.
STE003     05 IND-QTD-GLEBA            PIC 9(005) COMP-3
STE003                                                 VALUE ZEROS.
STE003     05 IND-GLEBA-EXCL           PIC S9(005) COMP-3
STE003                                                 VALUE ZEROS.
STE003     05 IND-COORD-GLEBA          PIC S9(005) COMP-3
STE003                                                 VALUE ZEROS.
STE003     05 IND-COORD-GLEBA-2        PIC S9(005) COMP-3
STE003                                                 VALUE ZEROS.
STE003     05 WRK-GLEBA-NOVA           PIC S9(005) COMP-3
STE003                                                 VALUE ZEROS.
           05 WRK-FIM-GLEBA-EXCL       PIC X(001) VALUE 'N'.
STE003     05 WRK-FIM-GLB-EXCL         PIC X(001) VALUE 'N'.
           05 WRK-SEM-GLB-EXCL         PIC X(001) VALUE 'N'.
BS0217     05 IND-1                    PIC 9(005) COMP-3
                                                       VALUE ZEROS.
EL0817     05 IND-2                    PIC 9(005) COMP-3
EL0817                                                 VALUE ZEROS.
MJ0924     05 IND-3                    PIC 9(005) COMP-3
MJ0924                                                 VALUE ZEROS.
MJ1224     05 IND-NIRF                 PIC 9(003) COMP-3
MJ1224                                                 VALUE ZEROS.
EL0817     05 WRK-CCPF-CNPJ-BENE.
.            10 WRK-CCPF-CNPJ-BENEF    PIC 9(09)       VALUE ZEROS.
.            10 WRK-CD-FILIAL-CNPJ-BENEF
.                                      PIC 9(04)       VALUE ZEROS.
.            10 WRK-CCTRL-CPF-CNPJ-BENEF
.                                      PIC 9(02)       VALUE ZEROS.
.          05 WRK-CPSSOA               PIC S9(10)V COMP-3
.                                                      VALUE ZEROS.
STE001     05 WRK-INDICADOR-TELA       PIC S9(001) VALUE ZEROS.
STE001     05 WRK-INDICADOR-TELA-X     PIC X(001) VALUE SPACES.
.          05 WRK-COD-RET              PIC X(001)      VALUE SPACES.
ID6799     05 WRK-AINDA-TEM            PIC X(001)      VALUE 'N'.
EL0817     05 WRK-ACHOU                PIC X(001)      VALUE 'N'.
BR1218     05 WRK-CSR02-ABERTO         PIC X(001)      VALUE 'N'.
BR1901     05 WRK-ENVIADO              PIC X(001)      VALUE 'N'.
BR1218     05 WRK-SIT-PARM-AREAGEOD    PIC X(001)      VALUE SPACES.
ID6703     05 WRK-ACHOU-CLASF          PIC X(001)      VALUE SPACES.
TESTEM     05 WRK-PARAGRAFO            PIC X(025)      VALUE SPACES.
TESTEM     05 WRK-RLIN-MSGEM-TEXT      PIC X(450)      VALUE SPACES.
RO1603     05 WRK-COUNT                PIC S9(09) COMP VALUE ZEROS.
WP1021     05 WRK-CSR-VJ-FIM           PIC X(001)      VALUE SPACES.
CET        05 WRK-CSR-LZ-FIM           PIC X(001)      VALUE SPACES.
CET        05 WRK-COUNT-SEG            PIC S9(005) COMP-3 VALUE ZEROS.
CET        05  WRK-VAPOLC-CS           PIC +9(13)V99    VALUE ZEROS.
CET        05  FILLER                  REDEFINES WRK-VAPOLC-CS.
CET            10 FILLER               PIC X(01).
CET            10 WRK-VAPOLC-SS        PIC 9(13)V99.
CET        05  WRK-VLR-SWISS           PIC 9(13)V99     VALUE ZEROS.
CET        05  WRK-IND-FINANC          PIC X(01)        VALUE SPACES.
CET        05  WRK-IND-PAG-ORCAM       PIC X(001)      VALUE SPACES.
CET        05  WRK-FIM-ORCAM           PIC X(001)      VALUE SPACES.
CET        05  WRK-13V2-COM-S          PIC +9(13)V99    VALUE ZEROS.
CET        05  FILLER                  REDEFINES WRK-13V2-COM-S.
CET            10 FILLER               PIC X(01).
CET            10 WRK-13V2-SEM-S       PIC 9(13)V99.
CET   *
CET        05  WRK-S9-08V7             PIC +9(08)V9(07) VALUE ZEROS.
CET        05  FILLER                  REDEFINES WRK-S9-08V7.
CET            10  FILLER              PIC  X(06).
CET            10  WRK-9-03V99         PIC  9(03)V99.
CET            10  FILLER              PIC  X(05).
CET   *
STE001     05  WRK-03V2-COM-S          PIC +9(03)V99    VALUE ZEROS.
STE001     05  FILLER                  REDEFINES WRK-03V2-COM-S.
STE001         10 FILLER               PIC X(01).
STE001         10 WRK-03V2-SEM-S       PIC 9(03)V99.
STE001*
STE001     05  WRK-PERCENTUAL-CAR-X.
STE001         10 WRK-PERC-CAR-N       PIC 9(03)V99 VALUE ZEROS.
      *
       01  WRK-LAT-SAIDA.
           05  WRK-LAT-PONTO-E-SAIDA   PIC S9(003)V9(11) VALUE ZEROS.

7C2510 01  FILLER      REDEFINES WRK-LAT-SAIDA.
7C2510     05  WRK-LAT-SAIDA-9V7       PIC S9(03)V9(07).
7C2510     05  FILLER                  PIC X(4).

       01  WRK-LONG-SAIDA.
           05  WRK-LONG-PONTO-E-SAIDA  PIC S9(003)V9(11) VALUE ZEROS.

7C2510 01  FILLER      REDEFINES WRK-LONG-SAIDA.
7C2510     05  WRK-LONG-SAIDA-9V7      PIC S9(03)V9(07).
7C2510     05  FILLER                  PIC X(4).

7C2511*01  WRK-ALT-SAIDA.
7C2511*    05  WRK-ALT-PONTO-E-SAIDA   PIC S9(004)V9(2) VALUE ZEROS.
      *
STE004 01  WRK-S9-2                    PIC +9(002) VALUE ZEROS.
STE004 01  FILLER REDEFINES WRK-S9-2.
STE004     05 FILLER                   PIC  X(001).
STE004     05 WRK-9-2                  PIC  9(002).
      *
STE004 01  WRK-X-2.
STE004     05 WRK-9-2-X                PIC  9(002) VALUE ZEROS.
CET   *
CET    01  WRK-PARCELAS.
CET        05 WRK-OCCURS-PCELA
CET                           OCCURS 0 TO 50 TIMES DEPENDING ON
CET                                                            IND-4.
CET           10 WRK-VLCTO-PCELA       PIC 9(013)V99   VALUE ZEROS.
CET           10 WRK-DATA-PCELA        PIC X(010)      VALUE SPACES.
CET   *
CET    01  IND-S                       PIC  9(005) COMP-3 VALUE ZEROS.
CET    01  WRK-PARCELAS.
CET        05 WRK-OCCURS-PCELA
CET                           OCCURS 0 TO 50 TIMES DEPENDING ON IND-S.
CET           10 WRK-TAB-SEG-VLR       PIC 9(013)V99   VALUE ZEROS.
CET           10 WRK-TAB-SEG-DATA      PIC X(010)      VALUE SPACES.
STE005*
MJ0725 01  WRK-QTD-TOTAL-UND.
MJ0725     05 WRK-QTD-TOTAL-UND-N      PIC 9(005)V99 VALUE ZEROS.

CET   *
      ***********************VARIAVEIS DE INDEXADOR
       77  WRK-TAM                     PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-POSICAO                 PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-NEW-POS                 PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-NEW-POS2                PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-1                       PIC  9(05) COMP-3 VALUE 1.
      *
       77  WRK-TAM2                    PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-POSICAO2                PIC  9(05) COMP-3 VALUE ZEROS.
      *
CET    77  IND-4                       PIC 9(005)  COMP-3
CET                                                    VALUE ZEROES.
       77  WRK-999                     PIC S9(05) COMP-3 VALUE 999.
211025*77  WRK-50                      PIC S9(05) COMP-3 VALUE 50.
211025 77  WRK-60                      PIC S9(05) COMP-3 VALUE 60.
       77  WRK-10                      PIC S9(05) COMP-3 VALUE 10.
       77  WRK-IND1                    PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND2                    PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND3                    PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND4                    PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND6                    PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND7                    PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND8                    PIC  9(05) COMP-3 VALUE ZEROS.
EG1117 77  WRK-INDC                    PIC  9(05) COMP-3 VALUE ZEROS.
EL0915 77  WRK-IND9                    PIC  9(05) COMP-3 VALUE ZEROS.
EL0915 77  WRK-IND10                   PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND11                   PIC  9(05) COMP-3 VALUE ZEROS.
ID6695 77  WRK-IND-P01                 PIC  9(05) COMP-3 VALUE ZEROS.
ID6695 77  WRK-IND-P02                 PIC  9(05) COMP-3 VALUE ZEROS.
ID6699 77  WRK-IND-CAR                 PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-INDCORDN                PIC  9(05) COMP-3 VALUE ZEROS.
ID6260 77  WRK-QTDE-6510               PIC  9(05) COMP-3 VALUE ZEROS.
ID6260 77  WRK-SEQ-ENVIO-RCOR          PIC  9(05) COMP-3 VALUE ZEROS.
ID6260 77  WRK-DSPRZ-CORDN             PIC  9(05) COMP-3 VALUE ZEROS.
ID6260 77  WRK-QTDE-CHAMADA            PIC  9(05) COMP-3 VALUE ZEROS.
EL0116 77  WRK-INDGLEBA                PIC  9(09) COMP-3 VALUE ZEROS.
EL0506 77  WRK-INDCONSOR               PIC  9(05) COMP-3 VALUE ZEROS.
EL0915 77  WRK-QEMPTO-NOTA-PROTR-TOTAL PIC S9(12)V99 COMP-3 VALUE ZEROS.
EL0915 77  WRK-QEMPTO-NOTA-PROTR       PIC S9(12)V99 COMP-3 VALUE ZEROS.
       77  WRK-NPCELA-ANT              PIC S9(03) COMP-3 VALUE ZEROS.
       77  WRK-CODE100-B0MV            PIC  X(01)        VALUE 'N'.
       77  WRK-CODE100-B0L8            PIC  X(01)        VALUE 'N'.
       77  WRK-CODE100-B0L7            PIC  X(01)        VALUE 'N'.
       77  WRK-CODE100-B0LZ            PIC  X(01)        VALUE 'N'.
       77  WRK-CODE100-B0C4            PIC  X(01)        VALUE 'N'.
       77  WRK-CODE100-B0MK            PIC  X(01)        VALUE 'N'.
       77  WRK-TAMANHO-SISMSG          PIC S9(04) COMP   VALUE ZEROS.
EG1117 77  WRK-CONTINUA                PIC  X(01)        VALUE SPACES.
STE003**7  WRK-BUSCAR-CORDN            PIC  X(01)        VALUE SPACES.
      *
      * *************************************************************
MJ0924 01  WRK-CORIGEM-AUX             PIC S9(003)  COMP-3 VALUE ZEROS.
MJ0924 01  WRK-LISTA-ORIGENS           PIC  X(256)  VALUE SPACES.
MJ0924 01  FILLER                REDEFINES  WRK-LISTA-ORIGENS.
MJ0924     05  FILLER               OCCURS     28   TIMES.
MJ0924         10  WRK-CORIGEM-PARM.
MJ0924             15  WRK-CORIGEM-PARM-N
MJ0924                                 PIC  9(003).
MJ0924         10  FILLER              PIC  X(001).
MJ0924         10  WRK-CFONTE-REC-PARM.
MJ0924             15  WRK-CFONTE-REC-PARM-N
MJ0924                                 PIC  9(004).
MJ0924         10  FILLER              PIC  X(001).
MJ0924     05  FILLER                  PIC  X(004).
ID6695*
ID6695* **** PROPRIETARIOS
ID6695 01  WRK-TAB-PROP.
ID6695      10 WRK-PROPIE-IMOV   OCCURS 030 TIMES.
ID6695         15 WRK-CCPF-CNPJ           PIC 9(09) VALUE ZEROS.
ID6695         15 WRK-FILIAL              PIC 9(04) VALUE ZEROS.
ID6695         15 WRK-CCTRL               PIC 9(02) VALUE ZEROS.
ID6695         15 WRK-IMOV-PROPIE  OCCURS 050 TIMES.
ID6695            20 WRK-TP-PESSOA-PROPT  PIC X(01) VALUE SPACES.
ID6695            20 WRK-CPFCNPJ-PROPT    PIC X(11) VALUE SPACES.
STE004*           20 WRK-NIRF             PIC X(09) VALUE SPACES.
STE004            20 WRK-NIRF             PIC X(08) VALUE SPACES.
ID6695            20 WRK-COD-SNCR         PIC X(13) VALUE SPACES.
ID6695            20 WRK-CREG-AMBTL-RURAL PIC X(41) VALUE SPACES.
BR0621            20 WRK-COUTGA-AGUA-IMOV PIC X(30) VALUE SPACES.
BR0621            20 WRK-PPRESV-AMBTL-PPRIE PIC 9(03)V9(2)
BR0621                                              VALUE ZEROS.

ID6699 01  WRK-TAB-CAR.
ID6699      10 WRK-CAR-GRAVAD    OCCURS 050 TIMES.
STE004*        15 WRK-GRV-NIRF            PIC X(09) VALUE SPACES.
STE004         15 WRK-GRV-NIRF            PIC X(08) VALUE SPACES.
ID6699         15 WRK-GRV-SNCR            PIC X(13) VALUE SPACES.
ID6699         15 WRK-GRV-CREG-AMBTL      PIC X(41) VALUE SPACES.
BR0621         15 WRK-GRV-COUTGA-AGUA     PIC X(30) VALUE SPACES.
BR0621         15 WRK-GRV-PPRESV-AMBTL    PIC 9(03)V9(2) VALUE ZEROS.
BR0621         15 WRK-GRV-PPRESV-AMBTL-X  REDEFINES
BR0621            WRK-GRV-PPRESV-AMBTL    PIC X(05).

       01  WRK-GRAVA-PROPIE               PIC X(01) VALUE SPACES.
ID6699 01  WRK-GRAVA-CAR                  PIC X(01) VALUE SPACES.
      *
      *
      *********************VARIAVEIS DE USO GERAL
       01  WRK-NENVIO-INFO-RECOR       PIC  S9(09) COMP-3 VALUE ZEROS.
ED2804 01  WRK-AREA-FINCD-ACUM         PIC  S9(9)V9(4)   COMP-3
 .                                                       VALUE ZEROS.
 .     01  WRK-AREA-FINCD-ACUM2        PIC  S9(9)V9(2)   COMP-3
 .                                                       VALUE ZEROS.
 .     01  WRK-AREA-FINCD-ACUM-SC      PIC  S9(9)V9(2)   VALUE ZEROS.
 .     01  WRK-QITEM-FINCD-ACUM        PIC  S9(9)V9(4)   COMP-3
 .                                                       VALUE ZEROS.
 .     01  WRK-QITEM-FINCD-ACUM-2      PIC  S9(9)V9(2)   COMP-3
 .                                                       VALUE ZEROS.
 .     01  WRK-QITEM-FINCD-ACUM-SC     PIC  S9(9)V9(2)   VALUE ZEROS.
 .     01  WRK-QPROTV-IMOV-ACUM        PIC  S9(9)V9(4)   COMP-3
ED2804                                                   VALUE ZEROS.
BSI999 01  WRK-QPROTV-IMOV-ACUM-AREA   PIC  S9(12)V99    COMP-3
BSI999                                                   VALUE ZEROS.
BSI999 01  WRK-QPROTV-IMOV-ACUM-ITEM   PIC  S9(12)V99    COMP-3
BSI999                                                   VALUE ZEROS.
       01  WRK-QPROTV-IMOV             PIC  S9(12)V99    COMP-3
                                                         VALUE ZEROS.
EL1115 01  WRK-VLR-PARCL-REC-PROP      PIC  S9(17)V99    COMP-3
.                                                        VALUE ZEROS.
.      01  WRK-VORCAM-CONTR-CREDT-TOTAL
.                                      PIC  S9(17)V99    COMP-3
.                                                        VALUE ZEROS.
.      01  WRK-VFINAM-EMPTO-CREDT-TOTAL
.                                      PIC  S9(17)V99    COMP-3
.                                                        VALUE ZEROS.
.      01  WRK-VORCAM-CONTR-CREDT      PIC  S9(13)V99    COMP-3
.                                                        VALUE ZEROS.
.      01  WRK-VFINAM-EMPTO-CREDT      PIC  S9(13)V99    COMP-3
EL1115                                                   VALUE ZEROS.
       01  WRK-VLCTO-PCELA-ACUM        PIC  S9(17)V99    COMP-3
                                                         VALUE ZEROS.
       01  WRK-VLR-RECBRUTA-ESPR       PIC  S9(17)V99    COMP-3
                                                         VALUE ZEROS.
       01  WRK-VLR-RECBRUTA-ESPR-SC    PIC  S9(17)V99    VALUE ZEROS.
      *
ED2804 77  WRK-MAREA-FINCD-COMP3       PIC  9(07)V9(04)  COMP-3
 .                                                       VALUE ZEROS.
 .     77  WRK-QITEM-FINCD-COMP3       PIC  9(07)V9(04)  COMP-3
 .                                                       VALUE ZEROS.
 .     77  WRK-QPROTV-IMOV-COMP3       PIC  9(07)V9(04)  COMP-3
ED2804                                                   VALUE ZEROS.
       77  WRK-AREA-FINAN              PIC S9(07)V9(04)  COMP-3
                                                         VALUE ZEROS.
       77  WRK-QTDE-FINAN              PIC S9(07)V9(02)  COMP-3
                                                         VALUE ZEROS.
      *
       01  WRK-CLIN-SIST-BACEN         PIC S9(03) COMP-3 VALUE ZEROS.
      *
       01  WRK-CRUR3WJL-FIM            PIC X(01)         VALUE SPACES.
EL0915 01  WRK-CRUR3WJL-FIM1           PIC X(01)         VALUE SPACES.
       01  WRK-NOCOR-GRP-PPSTA         PIC 9(03)         VALUE ZEROS.
EL0915 01  WRK-NOCOR-GRP-PPSTA-NOTA    PIC 9(03)         VALUE ZEROS.
EL1115 01  WRK-NOCOR-GRP-PPSTA-REC-PRO PIC 9(03)         VALUE ZEROS.
ID6260 01  WRK-MAX-OCORR               PIC 9(05) COMP-3  VALUE ZEROS.
      *
ED0604 01  WRK-VLR-ALIQ                PIC 9(003)V9(04)  COMP-3
 .                                                       VALUE ZEROS.
 .     01  WRK-VLR-ALIQ-SICOR          PIC 9(003)V9(02)  COMP-3
ED0604                                                   VALUE ZEROS.
ED2804 01  WRK-VLR-ALIQ-SICOR-SC       PIC 9(003)V9(02)  VALUE ZEROS.
      *
       01  FILLER.
RC         03  WRK-CEDULA-RECOR        PIC  X(17)        VALUE SPACES.
RC         03  FILLER                  REDEFINES  WRK-CEDULA-RECOR.
RC             05  WRK-ANO-CALE        PIC  9(04).
RC             05  WRK-NUM-FILL        PIC  9(03).
RC             05  WRK-CCDULA          PIC  9(10).
ID6520*
ID6520 01  WRK-AUX-X14                 PIC  X(14) VALUE SPACES.
ID6520 01  FILLER                REDEFINES WRK-AUX-X14.
ID6520     03  WRK-AUX-INT-09          PIC  X(09).
ID6520     03  WRK-AUX-X1              PIC  X(01).
ID6520     03  WRK-AUX-DEC-2           PIC  X(02).
ID6520     03  FILLER                  PIC  X(02).
ID6520*
ID6520 01  WRK-AREA-AUX.
ID6520     03  WRK-AREA-AUX-N          PIC  9(09)V99 VALUE ZEROS.
ID6520*
ID6520 01  WRK-QTDE-AUX.
ID6520     03  WRK-QTDE-AUX-N          PIC  9(09)V99 VALUE ZEROS.
ID6520*
ID6695 01  WRK-EDIT-CPF.
ID6695     05 WRK-EDIT-NCPF            PIC  9(09)        VALUE ZEROS.
ID6695     05 WRK-EDIT-CCPF            PIC  9(02)        VALUE ZEROS.
ID6695 01  FILLER REDEFINES WRK-EDIT-CPF.
ID6695     05 WRK-EDIT-NCNPJ           PIC  X(08).
ID6695     05 WRK-EDIT-CCNPJ           PIC  X(03).
      *
ID6854 01  WRK-ENVIO-NUMCTRLIF-AUX     PIC  X(00020)     VALUE SPACES.
      *
       01  WRK-ENVIO-SIS               PIC X(32000)      VALUE SPACES.
       01  WRK-ENVIO-SIS-R             REDEFINES WRK-ENVIO-SIS.
           05 FILLER                   PIC X(00009).
           05 WRK-ENVIO-NUMCTRLIF      PIC X(00020).
           05 FILLER                   PIC X(31971).
      *
       01  WRK-B09A-SIS                PIC X(32000)      VALUE SPACES.
       01  WRK-B09A-SIS-R              REDEFINES WRK-B09A-SIS.
           05 FILLER                   PIC X(00009).
           05 WRK-B09A-NUMCTRLIF       PIC X(00020).
           05 FILLER                   PIC X(31971).
      *
       01  WRK-B09A-SIS2               PIC X(32000)      VALUE SPACES.
       01  WRK-B09A-SIS-R2             REDEFINES WRK-B09A-SIS2.
           05 FILLER                   PIC X(00009).
           05 WRK-B09A-NUMCTRLI2       PIC X(00020).
           05 FILLER                   PIC X(31971).
      *
       01  WRK-DATA-ATUAL              PIC  X(10)        VALUE SPACES.
       01  WRK-TIMESTAMP               PIC  X(26)        VALUE SPACES.
       01  WRK-FORMA-DATA              REDEFINES WRK-TIMESTAMP.
           05 WRK-FORMA-AAAA           PIC  9(04).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-MM             PIC  9(02).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-DD             PIC  9(02).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-HORA           PIC  9(02).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-MIN            PIC  9(02).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-SEG            PIC  9(02).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-MILE           PIC  9(06).
      *
       01  WRK-COD-EMPNMNT             PIC  X(14)        VALUE SPACES.
       01  FILLER                      REDEFINES WRK-COD-EMPNMNT.
           05 FILLER                   PIC  X(02).
           05 WRK-MODALIDADE           PIC  X(02).
           05 FILLER                   PIC  X(10).
ED2804*
 .     01  WRK-CNEGOC-CONTR            PIC S9(10) COMP-3 VALUE ZEROS.
 .     01  WRK-TIPO-AGRIC-GRP          PIC S9(05) COMP-3 VALUE ZEROS.
 .     01  WRK-TIPO-INGR-GRP           PIC S9(05) COMP-3 VALUE ZEROS.
 .     01  WRK-TIPO-IRRIGC-GRP         PIC S9(05) COMP-3 VALUE ZEROS.
 .     01  WRK-CCULTR-GRP              PIC S9(05) COMP-3 VALUE ZEROS.
 .     01  WRK-TIPO-CICLO-GRP          PIC S9(05) COMP-3 VALUE ZEROS.
PRI499 01  WRK-TIPO-CLIMA-GRP          PIC S9(02) COMP-3 VALUE ZEROS.
 .
 .
 .     01  WRK-TIPO-AGRIC-TAB          PIC S9(01) COMP-3 VALUE ZEROS.
 .     01  WRK-TIPO-INGR-TAB           PIC S9(01) COMP-3 VALUE ZEROS.
 .     01  WRK-TIPO-IRRIGC-TAB         PIC S9(02) COMP-3 VALUE ZEROS.
 .     01  WRK-TIPO-CULTR-TAB          PIC S9(02) COMP-3 VALUE ZEROS.
 .     01  WRK-TIPO-CICLO-TAB          PIC S9(02) COMP-3 VALUE ZEROS.
PRI499 01  WRK-TIPO-CLIMA-TAB          PIC S9(02) COMP-3 VALUE ZEROS.
 .
 .     01  WRK-CMUN-IMOV-SICOR         PIC S9(10) COMP-3 VALUE ZEROS.
 .     01  WRK-CMUN-IMOV-SICOR-SC      PIC S9(10)        VALUE ZEROS.
ED2804*
ED0405 01  WRK-COORDN-FIM              PIC  X(01)        VALUE 'N'.
 .     01  WRK-IND-PAG                 PIC  X(01)        VALUE 'I'.
EL0817 01  WRK-IND-PAG-FICHA           PIC  X(01)        VALUE 'I'.
ED0405 01  WRK-COORDN-EXIST            PIC  X(01)        VALUE 'S'.
EL0506 01  WRK-CONSOR-FIM              PIC  X(01)        VALUE 'N'.
EL0506 01  WRK-CONSOR-EXIST            PIC  X(01)        VALUE 'S'.
       01  WRK-AREA-NCULT              PIC  X(01)        VALUE SPACES.
      *********************VARIAVEIS DE REDEFINICAO
      ************NUMERICO PARA CHAR
       01  WRK-VAR2-NUM                PIC  +9(02)       VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-VAR2-NUM.
           05 FILLER                   PIC  X(01).
           05 WRK-VAR2-CHAR            PIC  X(02).
      *
ID6181 01  WRK-CEP-S5                  PIC  +9(05)       VALUE ZEROS.
ID6181 01  FILLER                      REDEFINES WRK-CEP-S5.
ID6181     05 FILLER                   PIC  X(01).
ID6181     05 WRK-CEP-N5               PIC  9(05).
ID6181*
ID6181 01  WRK-COMPL-CEP-S3            PIC  +9(03)       VALUE ZEROS.
ID6181 01  FILLER                      REDEFINES WRK-COMPL-CEP-S3.
ID6181     05 FILLER                   PIC  X(01).
ID6181     05 WRK-COMPL-CEP-X3         PIC  X(03).
      *
 P-01  01  WRK-VAR5-NUM                PIC  9(05)        VALUE ZEROS.
 P-01  01  FILLER                      REDEFINES WRK-VAR5-NUM.
 P-01      05 FILLER                   PIC  X(01).
 P-01      05 WRK-VAR4-CHAR-5          PIC  X(04).
      *
       01  WRK-TAM10-NUM               PIC  9(10)        VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-TAM10-NUM.
           05 WRK-TAM10-CHAR           PIC  X(10).
      *
       01  WRK-VAR19-NUM               PIC  9(17)V99     VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-VAR19-NUM.
           05 WRK-VAR19-CHAR           PIC  X(19).
      *
ID6151*01  WRK-CMUN-NUM                -IC +9(10)        -ALUE -EROS.
ID6151*01  -ILLER                      -EDEFINES WRK-CMUN-NUM.
ID6151*    05 -ILLER                   -IC  X(05).
ID6151*    05 WRK-CMUN-NUM-6           -IC  X(06).
ID6151*
       01  WRK-PORT-NUM                PIC +9(06)        VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-PORT-NUM.
           05 FILLER                   PIC  X(01).
           05 FILLER                   PIC  X(02).
           05 WRK-PORT-CHAR            PIC  X(04).
      *
EL0506 01  WRK-CPRODT-RURAL.
.          05 WRK-CPRODT-RURAL-N       PIC  9(04)        VALUE ZEROS.
EL0506*
      *
       01  WRK-COD9-NUM                PIC  9(09)        VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-COD9-NUM.
           05 FILLER                   PIC  9(01).
           05 WRK-COD9-NUM8            PIC  9(08).
           05 WRK-COD9-NUM8-X          REDEFINES  WRK-COD9-NUM8
                                       PIC  X(08).
      *
       01  WRK-COM-SINAL-03            PIC +9(03)        VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-COM-SINAL-03.
           05 FILLER                   PIC X(01).
           05 WRK-SEM-SINAL-03         PIC 9(03).
      *
       01  WRK-05-CSINAL              PIC +9(05)         VALUE ZEROS.
       01  WRK-05-CSINAL-R            REDEFINES WRK-05-CSINAL.
           05 FILLER                  PIC X(01).
           05 WRK-05-SSINAL           PIC 9(05).
      *
4SYS21 01  WRK-02-CSINAL              PIC +9(02)         VALUE ZEROS.
4SYS21 01  WRK-02-CSINAL-R            REDEFINES WRK-02-CSINAL.
4SYS21     05 FILLER                  PIC X(01).
4SYS21     05 WRK-02-SSINAL           PIC 9(02).
      *
       01  WRK-COM-SINAL-04-N          PIC +9(04)        VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-COM-SINAL-04-N.
           05 FILLER                   PIC X(01).
           05 WRK-SEM-SINAL-04-X       PIC X(04).
      *
STE001 01  WRK-VAR11-NUM               PIC 9(13)         VALUE ZEROS.
.      01  FILLER                      REDEFINES WRK-VAR11-NUM.
STE001     05 WRK-VAR11-CHAR           PIC X(13).
      *
       01  WRK-VLR-RECBRUTA-N          PIC +9(017)V99    VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-VLR-RECBRUTA-N.
           05 FILLER                   PIC X(01).
           05 WRK-VLR-RECBRUTA-A       PIC X(19).
ED0604*
 .     01  WRK-VLR-ALIQ-N              PIC 9(003)V9(02)  VALUE ZEROS.
 .     01  FILLER                      REDEFINES WRK-VLR-ALIQ-N.
 .         05 WRK-VLR-ALIQ-C           PIC X(005).
ED0604*
BSI1   01  WRK-DT-PLANTIO-INI          PIC X(008)        VALUE SPACES.
 .     01  WRK-DT-PLANTIO-FIM          PIC X(008)        VALUE SPACES.
 .     01  WRK-DT-COLHEITA-INI         PIC X(008)        VALUE SPACES.
 .     01  WRK-DT-COLHEITA-FIM         PIC X(008)        VALUE SPACES.
BSI1  *
AS1202 01  WRK-CORDN.
BSI        10 WRK-LAT-PONTO            PIC  S9(03)V9(11) COMP-3
.                                                        VALUE ZEROS.
.          10 WRK-LONG-PONTO           PIC  S9(03)V9(11) COMP-3
.                                                        VALUE ZEROS.
.          10 WRK-ALTT-PONTO           PIC  S9(04)V9(02) COMP-3
.                                                        VALUE ZEROS.
.      01  WRK-LAT-PONTO-A.
BSI        05 WRK-LAT-PONTO-N          PIC  S9(03)V9(11) VALUE ZEROS.
7C2510 01  FILLER      REDEFINES WRK-LAT-PONTO-A.
7C2510     05  WRK-LAT-PONTO-9V7       PIC S9(03)V9(07).
7C2510     05  FILLER                  PIC X(3).

.     *
.      01  WRK-LONG-PONTO-A.
BSI        05 WRK-LONG-PONTO-N         PIC  S9(03)V9(11) VALUE ZEROS.
7C2510 01  FILLER      REDEFINES WRK-LONG-PONTO-A.
7C2510     05  WRK-LONG-PONTO-9V7      PIC S9(03)V9(07).
7C2510     05  FILLER                  PIC X(3).
.     *
7C2511*01  WRK-ALTT-PONTO-A.
7C2511*    05 WRK-ALTT-PONTO-N         PIC  S9(04)V9(02) VALUE ZEROS.
.     *
EL0116*
4.12   01  WRK-QTD-PROD-ESP-X.
4.12       05  WRK-QTD-PROD-ESP-N      PIC 9(014)        VALUE ZEROS.
4.12   01  WRK-QTD-PROD-ESP-C          PIC 9(014) COMP-3 VALUE ZEROS.
EL0116 01  WRK-INDGLEBA-NUM            PIC 9(009)        VALUE ZEROS.
EL0116 01  FILLER                      REDEFINES WRK-INDGLEBA-NUM.
EL0116     05 WRK-INDGLEBA-ALFA        PIC X(009).
EL0116 01  WRK-CIMOV-CREDT-RURAL-ANT   PIC 9(010)        VALUE ZEROS.
7C2511 01  WRK-MDGLEBA-ANT-AUX         PIC X(005)        VALUE ZEROS.
EL1117 01  WRK-MDGLEBA-ANT             PIC X(005)        VALUE ZEROS.
STE003 01  WRK-MDGLEBA-ANT-X.
STE003     05  WRK-MDGLEBA-ANT-N       PIC 9(005)        VALUE ZEROS.
EL0116 01  WRK-IDENTC-GLEBA            PIC X(019)        VALUE SPACES.
EL0116*
EL0116 01  WRK-CPFPROPT.
EL0116     05 WRK-CPFPROPT-CORPO       PIC 9(009)        VALUE ZEROS.
EL0116     05 WRK-CPFPROPT-DG          PIC 9(002)        VALUE ZEROS.
EL0116*
EL0116 01  WRK-CNPJBASE-PROPT.
EL0116     05 WRK-CNPJBASE-PROPT-N     PIC 9(008)        VALUE ZEROS.
ED0405*
BSI1   01  WRK-CIMOV-CREDT-RURAL       PIC 9(010)        VALUE ZEROS.
      ************CHAR PARA NUMERICO
       01  WRK-VLR1.
           05 FILLER                   PIC  X(04)        VALUE ZEROS.
           05 WRK-VLR15-PARC           PIC  9(13)V99     VALUE ZEROS.
      *
       01  WRK-DATADIA-AMD-X           PIC  X(08)        VALUE SPACES.
       01  WRK-DATADIA-AMD-XR          REDEFINES WRK-DATADIA-AMD-X.
           05 WRK-DATADIA-AMD-N        PIC  9(08).
      *
BR0621 01  WRK-DVALDD.
BR0621     05 WRK-DVALDD-NUM           PIC  9(08)        VALUE ZEROS.
      *
      ************REMOCAO DE SINAL
ED2804 01  WRK-VAR9-NUMS               PIC  +9(09)V99    VALUE ZEROS.
 .     01  FILLER                      REDEFINES WRK-VAR9-NUMS.
 .         05 FILLER                   PIC  X(01).
ED2804     05 WRK-VAR9-NUMSS           PIC  9(09)V99.
      *
       01  WRK-VAR17-NUMS              PIC  +9(17)V99     VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-VAR17-NUMS.
           05 FILLER                   PIC  X(01).
           05 WRK-VAR17-NUMSS          PIC  9(17)V99.
      *
      *01  WRK-VAR03-NUMS              -IC  +9(03)V99    -ALUE -EROS.
      *01  -ILLER                      -EDEFINES WRK-VAR03-NUMS.
      *    05 -ILLER                   -IC  X(01).
      *    05 WRK-VAR03-NUMSS          -IC  9(03)V99.
      *
       01  WRK-VAR12-NUMS              PIC  +9(12)V99    VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-VAR12-NUMS.
           05 FILLER                   PIC  X(01).
           05 WRK-VAR12-NUMSS          PIC  9(12)V99.
      *
       01  WRK-VLR15                   PIC S9(13)V99     VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-VLR15.
           05 WRK-VLR15-SS             PIC  9(13)V99.
      *
ED2804 01  WRK-C-SINAL-9V              PIC +9(003)V9(04) VALUE ZEROS.
 .     01  FILLER                      REDEFINES WRK-C-SINAL-9V.
 .         05 FILLER                   PIC  X(01).
 .         05 WRK-S-SINAL-9V           PIC  9(003)V9(04).
 .    *
 .     01  WRK-COM-SINAL-01            PIC +9(01)        VALUE ZEROS.
 .     01  FILLER                      REDEFINES  WRK-COM-SINAL-01.
 .         05 FILLER                   PIC X(01).
 .         05 WRK-SEM-SINAL-01         PIC 9(01).
 .
 .     01  WRK-COM-SINAL-02            PIC +9(02)        VALUE ZEROS.
 .     01  FILLER                      REDEFINES  WRK-COM-SINAL-02.
 .         05 FILLER                   PIC X(01).
 .         05 WRK-SEM-SINAL-02         PIC 9(02).
ED2804*
       01  WRK-NUMER-03                PIC 9(03)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-NUMER-03.
           05 FILLER                   PIC X(01).
           05 WRK-NUMER-02             PIC 9(02).
      *                                                                *
       01  WRK-COM-SINAL-10            PIC +9(10)        VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-COM-SINAL-10.
              10 FILLER                PIC X(01).
              10 WRK-SEM-SINAL-10      PIC 9(10).
      *
       01  WRK-COM-SINAL-09            PIC +9(09)        VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-COM-SINAL-09.
              10 FILLER                PIC X(01).
              10 WRK-SEM-SINAL-09      PIC 9(09).
ED2804*
 .     01  WRK-IMOV-N                  PIC +9(010)       VALUE ZEROS.
 .     01  FILLER                      REDEFINES  WRK-IMOV-N.
 .         05 FILLER                   PIC X(05).
 .         05 WRK-IMOV-N-SICOR         PIC 9(06).
ED2804*
ED2105 01  WRK-IMOV-N-SC               PIC 9(010)       VALUE ZEROS.
 .     01  FILLER                      REDEFINES  WRK-IMOV-N-SC.
 .         05 FILLER                   PIC X(04).
 .         05 WRK-IMOV-N-SICOR-SC      PIC 9(06).
ID6151     05 WRK-IMOV-N-SICOR-CH6     REDEFINES
ID6151        WRK-IMOV-N-SICOR-SC      PIC X(06).
ED2105*
      ******************VARIAVEIS DE TRATAMENTO DE NULIDADE
CET    01  WRK-NULOS.
CET        05  WRK-CFONTE-BACEN-RURAL-N
CET                                    PIC S9(004) COMP  VALUE +0.
       01  WRK-CRURB0U2-NULL.
           05 WRK-CRQUIS-FINCR-NULL    PIC S9(004) COMP  VALUE +0.
           05 WRK-CUSUAR-MANUT-NULL    PIC S9(004) COMP  VALUE +0.
           05 WRK-HMANUT-REG-NULL      PIC S9(004) COMP  VALUE +0.
      *
       01  WRK-B0UA-NULL.
           05 WRK-B0UA-CUS-MAN-NULL    PIC S9(004) COMP  VALUE +0.
           05 WRK-B0UA-HMA-REG-NULL    PIC S9(004) COMP  VALUE +0.
      *
       01  WRK-CRURB037-NULL.
           05 WRK-CFONTE-BACENR-NULL   PIC S9(004) COMP  VALUE +0.
      *
MJ0924 01  WRK-CRURB038-NULL.
MJ0924     05 WRK-RPARM-CREDT-RURAL-NULL
MJ0924                                 PIC S9(004) COMP  VALUE +0.
      *
       01  WRK-CRURB068-NULL.
           05 WRK-PRBATE-EMPTO-NULL    PIC S9(004) COMP  VALUE +0.
STE001     05 WRK-CINDCD-CONFM-FLORS-N PIC S9(004) COMP  VALUE +0.
STE001     05 WRK-CINDCD-CONFM-IBAMA-N PIC S9(004) COMP  VALUE +0.
STE001     05 WRK-CINDCD-CONFM-CSVAO-N PIC S9(004) COMP  VALUE +0.
STE001     05 WRK-PREDC-JURO-AMBTL-N   PIC S9(004) COMP  VALUE +0.
STE004     05 WRK-CINDCD-CONFM-RURAL-N PIC S9(004) COMP  VALUE +0.
STE004     05 WRK-COUTRO-TPO-CONFM-N   PIC S9(004) COMP  VALUE +0.
      *
BSI1   01  WRK-CRURB0B1-NULL.
BSI1       05  WRK-CNIRF-IMOV-RURAL-NULL
BSI1                                   PIC S9(004) COMP  VALUE +0.
BSI1       05  WRK-CCCIR-NULL          PIC S9(004) COMP  VALUE +0.
BSI1       05  WRK-CREG-AMBTL-RURAL-NULL
BSI1                                   PIC S9(004) COMP  VALUE +0.
BR0621     05  WRK-COUTGA-AGUA-IMOV-NULL
BR0621                                 PIC S9(004) COMP  VALUE +0.
BR0621     05  WRK-PPRESV-AMBTL-PPRIE-NULL
BR0621                                 PIC S9(004) COMP  VALUE +0.
BR0621     05  WRK-DVALDD-OUTGA-AGUA-NULL
BR0621                                 PIC S9(004) COMP  VALUE +0.
MJ1224     05  WRK-CMATR-IMOV-RURAL-NULL
MJ1224                                 PIC S9(004) COMP  VALUE +0.
      *
      *-01  WRK-B0L4-NULL.
      *   -05 MAREA_FINCD_RURAL CD-NULL  PIC S9(04) COMP VALUE +0.
      *   -05 WRK-B0L4-QITEM-FINCD-NULL  PIC S9(04) COMP VALUE +0.
      *   -05 WRK-B0L4-QPROTV-IMOV-NULL  PIC S9(04) COMP VALUE +0.
      *
       01  WRK-CRURB0V3-NULL.
           05 WRK-PALIQT-SEGUR-BACEN-NULL
                                       PIC S9(004) COMP  VALUE +0.
      *
BR0621 01  WRK-CRURB0MB-NULL.
BR0621     05 WRK-CCETFC-ORGNC-EMPTO-NULL
BR0621                                 PIC S9(004) COMP  VALUE +0.
BR0621     05 WRK-EURL-CETFC-ORGNC-NULL
BR0621                                 PIC S9(004) COMP  VALUE +0.
BR0621     05 WRK-CRSTRB-EMPTO-RURAL-NULL
BR0621                                 PIC S9(004) COMP  VALUE +0.
BR0621     05 WRK-EURL-RSTRB-EMPTO-NULL
BR0621                                 PIC S9(004) COMP  VALUE +0.
BR0621     05 WRK-PENERG-RENOV-EMPTO-NULL
BR0621                                 PIC S9(004) COMP  VALUE +0.
      *
WP1021 01  WRK-CCICLO-CTVDA-NULL       PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CMANUT-SOLO-NULL        PIC S9(04)  COMP    VALUE ZEROS.

WP1021 01  WRK-CTPO-SOLO-AGCLT         PIC  9(02)+         VALUE ZEROS.
WP1021 01  FILLER   REDEFINES  WRK-CTPO-SOLO-AGCLT.
WP1021     05  WRK-CTPO-SOLO-AGCLT-X   PIC  X(02).
WP1021     05  FILLER                  PIC  X(01).

WP1021 01  WRK-CCICLO-CTVDA            PIC  9(02)+         VALUE ZEROS.
WP1021 01  FILLER   REDEFINES  WRK-CCICLO-CTVDA.
WP1021     05  WRK-CCICLO-CTVDA-X      PIC  X(02).
WP1021     05  FILLER                  PIC  X(01).

WP1021 01  WRK-CMANUT-SOLO             PIC  9(02)+         VALUE ZEROS.
WP1021 01  FILLER   REDEFINES  WRK-CMANUT-SOLO.
WP1021     05  WRK-CMANUT-SOLO-X       PIC  X(02).
WP1021     05  FILLER                  PIC  X(01).

       77  WRK-ENVIO-LEN               PIC S9(004) COMP  VALUE ZEROS.

7C2510 01  WRK-MASK-COORD-X.
7C2510     05  WRK-MASK-COORD          PIC -999,9999999    VALUE ZEROS.

7C2510 01  FILLER.
7C2510     05 WRK-QTD-CULTVD           PIC 9(04) COMP-3    VALUE ZEROS.
7C2510     05 WRK-PONTO-CULTVD         OCCURS 999.
7C2510        10 WRK-CULTVD-LON        PIC X(12)           VALUE SPACES.
7C2510        10 FILLER                PIC X(01)           VALUE SPACES.
7C2510        10 WRK-CULTVD-LAT        PIC X(12)           VALUE SPACES.
7C2510     05 WRK-QTD-NCULTVD          PIC 9(04) COMP-3    VALUE ZEROS.
7C2510     05 WRK-PONTO-NCULTVD        OCCURS 999.
7C2510        10 WRK-NCULTVD-LON       PIC X(12)           VALUE SPACES.
7C2510        10 FILLER                PIC X(01)           VALUE SPACES.
7C2510        10 WRK-NCULTVD-LAT       PIC X(12)           VALUE SPACES.

7C2510 01  WRK-NUM-POLIGN              PIC 9(01) COMP-3    VALUE ZEROS.
7C2510 01  WRK-TAM-POLIGN              PIC 9(05) COMP-3    VALUE ZEROS.
7C2510 01  WRK-POLIGN                  PIC X(10000)        VALUE SPACES.
7C2510 01  IND-5                       PIC 9(04) COMP-3    VALUE ZEROS.
      *
      *----------------------------------------------------------------*
      *    'AREA DE COMUNICACAO COM O MODULO UORG122B'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-UORG122B.
           COPY UORGW000.
           COPY UORGW22B.
      *
STE001*----------------------------------------------------------------*
.      01  FILLER                      PIC  X(050) VALUE
.          'AREA DE TRATAMENTO DE DEBUG'.
.     *----------------------------------------------------------------*
.      01  WRK-MSG-ERRO-LIVRE          PIC  X(500) VALUE SPACES.
.
.      01  WRK-SQLCODE.
.          05  WRK-SQLCODE-N           PIC +9(003) VALUE ZEROS.
.          05  FILLER REDEFINES WRK-SQLCODE-N.
.              07  FILLER              PIC  X(001).
.              07  WRK-SQLCODE-SS      PIC  9(003).
.
.      01  WRK-AUX-02-X.
.          05  WRK-AUX-02-N            PIC +9(002) VALUE ZEROS.
.          05  FILLER REDEFINES WRK-AUX-02-N.
.              07  FILLER              PIC  X(001).
.              07  WRK-AUX-02          PIC  9(002).
STE001*----------------------------------------------------------------*
      *    'AREA DE COMUNICACAO COM O MODULO PSDC3BSR'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-PSDC3BSR.
           COPY PSDCW00W.
           COPY PSDCWBSI.
      *
      *---------------------------------------------------------------*
      *    '*** AREA PARA CHAMADA MODULO RCOR7000 ***'.
      *---------------------------------------------------------------*
      *
       COPY 'I#RCOR10'.
      *
MJ1224*----------------------------------------------------------------*
MJ1224*    'AREA DE COMUNICACAO COM O MODULO CRUR3NIR'.
MJ1224*----------------------------------------------------------------*
MJ1224*
MJ1224 01  WRK-AREA-CRUR3NIR.
MJ1224     COPY CRURW00W.
MJ1224     COPY CRURWNIC.
MJ1224*
      *----------------------------------------------------------------*
      *    'AREA DE COMUNICACAO COM O MODULO CRUR3C42'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-CRUR3C42.
           COPY CRURWI42.
CET   *
CET   *----------------------------------------------------------------*
CET   *    'AREA DE COMUNICACAO COM O MODULO CRUR3XVL'.
CET   *----------------------------------------------------------------*
CET    01  WRK-AREA-CRUR3XVL.
CET        COPY CRURW00W.
CET        COPY CRURWXVI.
      *----------------------------------------------------------------*
      *    'AREA DE COMUNICACAO COM O MODULO PSDC312C'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-PSDC312C.
           COPY PSDCW00W.
           COPY PSDCW12I.
STE005*----------------------------------------------------------------*
STE005 01  FILLER                      PIC X(050)      VALUE
STE005     'AREA DE INTERFACE COM SERVICO'.
STE005*----------------------------------------------------------------*
STE005 01  WRK-AREA-CRURW27V.
STE005     COPY CRURW27V.
STE005*
 P-01 *----------------------------------------------------------------*
 P-01 *    'DECLARACAO DO BOOK CRURW00V'.
 P-01 *----------------------------------------------------------------*
 P-01 *
 .     01 WRK-AREA-CRURW00V.
 P-01      COPY CRURW00V.
EL0506*----------------------------------------------------------------*
.     *    'AREA DE FORMATACAO BOOK DO BOOK CRURW14C'.
.     *----------------------------------------------------------------*
.     *
.      01  WRK-AREA-CRUR414S.
.          COPY CRURW00W.
EL0506     COPY CRURW14C.
      *
      *----------------------------------------------------------------*
      *    'AREA DE INTERFACE COM O MODULO FNEB3BZL'.
      *----------------------------------------------------------------*
       01  WRK-AREA-FNEB3BZL.
           COPY FNEBW000.
           COPY FNEBWBZI.
      *
      *----------------------------------------------------------------*
      *    'AREA DE INTERFACE COM SERVICO FNEB33PL'.
      *----------------------------------------------------------------*
       01  WRK-AREA-FNEB33PL.
           COPY FNEBW000.
           COPY FNEBW3PI.
      *
      *----------------------------------------------------------------*
      *    'AREA DE INTERFACE COM O MODULO CRUR3WJL'.
      *----------------------------------------------------------------*
       01  WRK-AREA-CRUR3WJL.
           COPY CRURW00W.
           COPY CRURWWJI.
CET   *----------------------------------------------------------------*
CET    01  WRK-AREA-CRUR3CCI.
CET        COPY CRURW00W.
CET        COPY CRURWCAA.
CET   *
      *----------------------------------------------------------------*
EL0817*    'AREA DE INTERFACE COM SERVICO CRUR3G8L '.
 .    *----------------------------------------------------------------*
 .     01  WRK-AREA-CRUR3G8L.
 .         COPY CRURW00W.
 .         COPY CRURWG8I.
 .    *
ID6181*----------------------------------------------------------------*
  ""   01  FILLER                      PIC X(050)      VALUE
  ""       'AREA DE COMUNICACAO COM O MODULO CEPN301R'.
  ""  *----------------------------------------------------------------*
  ""   01  WRK-AREA-CEPN301R.
  ""       COPY CEPNW00W.
  ""       COPY CEPNW01I.
ID6181*
4SYS21*----------------------------------------------------------------*
.      01  FILLER                      PIC X(050)      VALUE
.          'AREA DE COMUNICACAO COM O MODULO CRUR4F0C'.
.     *----------------------------------------------------------------*
.      01  WRK-AREA-CRUR4F0C.
.          COPY CRURW00W.
.          COPY CRURWFGI.
4SYS21*
EL0817*----------------------------------------------------------------*
      *
EL1115*----------------------------------------------------------------*
.     *    'AREA DE INTERFACE COM O BOOK CRURW04V'.
.     *----------------------------------------------------------------*
.      01  WRK-AREA-CRURW04V.
EL1115     COPY CRURW04V.
      *
      *----------------------------------------------------------------*
      *    'AREA DE INTERFACE COM O BOOK CRURW12V'.
      *----------------------------------------------------------------*
       01  WRK-AREA-CRURW12V.
           COPY CRURW12V.
ID6703*----------------------------------------------------------------*
ID6703*    'AREA DE INTERFACE COM O BOOK CRURW08V'.
ID6703*----------------------------------------------------------------*
ID6703 01  WRK-AREA-CRURW08V.
ID6703     COPY CRURW08V.
EL0915*----------------------------------------------------------------*
.     *    'AREA DE INTERFACE COM O BOOK CRURW13V'.
.     *----------------------------------------------------------------*
.      01  WRK-AREA-CRURW13V.
EL0915     COPY CRURW13V.
      *
      *----------------------------------------------------------------*
      *    'AREA DE INTERFACE COM O BOOK CRURW06V'.
      *----------------------------------------------------------------*
       01  WRK-AREA-CRURW06V.
           COPY CRURW06V.
      *
7C5.10*----------------------------------------------------------------*
 .    *    'AREA DE INTERFACE COM O BOOK CRURW07V'.
 .    *----------------------------------------------------------------*
 .      1  WRK-AREA-CRURW07V.
7C5.10     COPY CRURW07V.
      *
AS3007*----------------------------------------------------------------*
.     *    'AREA DE INTERFACE COM O BOOK CRURW03V'.
.     *----------------------------------------------------------------*
.      01  WRK-AREA-CRURW03V.
AS3007     COPY CRURW03V.
      *
      *----------------------------------------------------------------*
      *    'AREA DE INTERFACE COM O BOOK CRURW15V'.
      *----------------------------------------------------------------*
       01  WRK-AREA-CRURW15V.
           COPY CRURW15V.
      *
      *----------------------------------------------------------------*
EL0506*    'AREA DE INTERFACE COM O BOOK CRURW17V'.
.     *----------------------------------------------------------------*
.      01  WRK-AREA-CRURW17V.
.          COPY CRURW17V.
EL0506*
      *
      *----------------------------------------------------------------*
      *    'AREA DE FORMATACAO BOOK PARA ENVIO BACEN'.
      *----------------------------------------------------------------*
      *
           COPY CRURWAY1.
      *----------------------------------------------------------------*
      *    'AREA DE COMUNICACAO DO PROGRAMA FRWK1999'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO      PIC X(526)          VALUE SPACES.
      *
      *----------------------------------------------------------------*
      *    'AREA DE TRATAMENTO DE ERRO MODULO'.
      *----------------------------------------------------------------*
      *
       01 WRK-COPY-MOD.
          COPY 'I#FRWKMD'.
      *
      *----------------------------------------------------------------*
      *    'AREA DE TRATAMENTO DE ERRO DB2  (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-DB2.
           COPY 'I#FRWKDB'.
      *
      *----------------------------------------------------------------*
      *    'AREA DE TRATAMENTO DE ERRO CICS'.
      *----------------------------------------------------------------*
      *
       01 WRK-COPY-CICS.
          COPY 'I#FRWKCI'.
      *                                                                *
TESTEM 01 WRK-COPY-LIVRE.
TESTEM    COPY 'I#FRWKLI'.
      *----------------------------------------------------------------*
      *    '*** AREA PARA DECLARADAO DE DCLGENS ***'.
      *----------------------------------------------------------------*
      *                                                                *
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *
      *    TABELA TAPOLC_SEGUR_BACEN
           EXEC SQL
               INCLUDE CRURB0V3
           END-EXEC.
      *
      *    TABELA TBEM_IMOV_BENEF                                      *
           EXEC SQL
               INCLUDE CRURB0L4
           END-EXEC.
      *                                                                *
WP1021     EXEC SQL
WP1021         INCLUDE CRURB0VJ
WP1021     END-EXEC.
WP1021*                                                                *
      *    TABELA TMODLD_RURAL                                         *
           EXEC SQL
               INCLUDE CRURB0B9
           END-EXEC.
      *                                                                *
      *    TABELA TCTRL_ENVIO_INFO                                     *
           EXEC SQL
               INCLUDE CRURB0U2
           END-EXEC.
      *                                                                *
      *    TABELA TCONTR_CREDT_RURAL                                   *
           EXEC SQL
               INCLUDE CRURB008
           END-EXEC.
      *                                                                *
      *    TABELA TLIN_MSGEM_REMSS                                     *
           EXEC SQL
               INCLUDE CRURB0UA
           END-EXEC.
      *                                                                *
      *    TABELA TDOCTO_CREDT_RURAL                                   *
           EXEC SQL
               INCLUDE CRURB061
           END-EXEC.
      *                                                                *
      *    TRESP_UND_RURAL                                             *
           EXEC SQL
               INCLUDE CRURB0MV
           END-EXEC.
      *                                                                *
      *    TPRTCP_PSSOA_RURAL                                          *
           EXEC SQL
               INCLUDE CRURB0MS
           END-EXEC.
      *                                                                *
      *    TABELA TORIGE_REC_RURAL                                     *
           EXEC SQL
               INCLUDE CRURB037
           END-EXEC.
      *                                                                *
      *    TABELA TCOMPO_CONTR_CREDT
           EXEC SQL
               INCLUDE CRURB0L7
           END-EXEC.
      *                                                                *
      *    TABELA TORCAM_CONTR_CREDT
           EXEC SQL
               INCLUDE CRURB0MB
           END-EXEC.
      *
      *    TPCELA_PGTO_CONTR                                           *
           EXEC SQL
               INCLUDE CRURB0MK
           END-EXEC.
      *                                                                *
      *    TABELA TLCTO_PCELA_RURAL
           EXEC SQL
               INCLUDE CRURB0LZ
           END-EXEC.
      *                                                                *
      *    TABELA TPPRIE_CREDT_RURAL
           EXEC SQL
               INCLUDE CRURB0C4
           END-EXEC.
      *                                                                *
      *    TABELA TCLAUS_CONTR_RURAL
           EXEC SQL
               INCLUDE CRURB0O0
           END-EXEC.
      *                                                                *
      *    TABELA TINSTT_CREDT_BACEN
           EXEC SQL
               INCLUDE CRURB0TF
           END-EXEC.
      *                                                                *
      *    TABELA TBNEFC_RURAL
           EXEC SQL
               INCLUDE CRURB055
           END-EXEC.
      *                                                                *
      *    TABELA TEMPTO_RENDA_AGROP
           EXEC SQL
               INCLUDE CRURB068
           END-EXEC.
EL0817*                                                                *
.     *    TABELA TFICHA_CAD_RURAL
.          EXEC SQL
.              INCLUDE CRURB069
EL0817     END-EXEC.
BSI1  *                                                                *
BSI1  *    TABELA TIMOV_CREDT_RURAL
BSI1       EXEC SQL
BSI1           INCLUDE CRURB0B1
BSI1       END-EXEC.
      *                                                                *
BR1218*    TABELA TIMOV_CREDT_RURAL
BR1218     EXEC SQL
BR1218         INCLUDE CRURB038
BR1218     END-EXEC.
BR1218*                                                                *
BR1218*    TABELA TMODLD_RURAL_BACEN
BR1218     EXEC SQL
BR1218         INCLUDE CRURB0SZ
BR1218     END-EXEC.
BR1218*                                                                *
ID6703*    TABELA TCLASF_EMPTO_RURAL
ID6703     EXEC SQL
ID6703         INCLUDE CRURB056
ID6703     END-EXEC.
STE001*                                                                *
.     *    TABELA TOUTRO_TRATO_SOLO
.          EXEC SQL
.              INCLUDE CRURB0X1
STE001     END-EXEC.
STE004*                                                                *
.     *    TABELA TOUTRO_CONFM_BACEN
.          EXEC SQL
.              INCLUDE CRURB01F
STE004     END-EXEC.
CAT408*----------------------------------------------------------------*
CAT408 01  FILLER                      PIC X(32)           VALUE
CAT408     '** INCLUDE CRURB0L5           **'.
CAT408*----------------------------------------------------------------*
CAT408     EXEC SQL
CAT408         INCLUDE CRURB0L5
CAT408     END-EXEC.
CAT408*
      * ***  CURSOR BENEFICIARIO                                       *
           EXEC SQL
            DECLARE CSR01-CRURB0MV-JN CURSOR FOR
               SELECT B.CPSSOA_JURID_CONTR
                     ,B.CTPO_CONTR_NEGOC
                     ,B.NSEQ_CONTR_NEGOC
                     ,B.CPSSOA_JURID
                     ,B.NSEQ_UND_ORGNZ
                     ,B.CTPO_RESP_UND
                     ,C.CPSSOA
                     ,C.CTPO_PRTCP_PSSOA
               FROM   DB2PRD.TRESP_UND_RURAL    B
                     ,DB2PRD.TPRTCP_PSSOA_RURAL C
               WHERE B.CPSSOA_JURID_CONTR = C.CPSSOA_JURID_CONTR
               AND   B.CTPO_CONTR_NEGOC   = C.CTPO_CONTR_NEGOC
               AND   B.NSEQ_CONTR_NEGOC   = C.NSEQ_CONTR_NEGOC
               AND   B.CPSSOA_JURID_CONTR = :CRURB0MV.CPSSOA-JURID-CONTR
               AND   B.CTPO_CONTR_NEGOC   = :CRURB0MV.CTPO-CONTR-NEGOC
               AND   B.NSEQ_CONTR_NEGOC   = :CRURB0MV.NSEQ-CONTR-NEGOC
               AND  (C.CTPO_PRTCP_PSSOA   = 20
               OR    C.CTPO_PRTCP_PSSOA   = 13)
               AND   B.CTPO_RESP_UND      = :CRURB0MV.CTPO-RESP-UND
           END-EXEC.
      *
      * ***  CURSOR COMPONENTES CONTRATO/ORCAMENTO                     *
           EXEC SQL
            DECLARE CSR02-CRURB0L7-JN CURSOR FOR
               SELECT A.CPSSOA_JURID_CONTR
                     ,A.CTPO_CONTR_NEGOC
                     ,A.NSEQ_CONTR_NEGOC
                     ,B.CORCAM_CONTR_RURAL
                     ,B.PTX_JURO_EMPTO
BR0918               ,B.CINDCD_ECONM_NUMRC
                     ,B.CEMPTO_RURAL_BACEN
EL0715               ,B.CMODLD_RURAL
BR0621               ,B.CCETFC_ORGNC_EMPTO
BR0621               ,B.EURL_CETFC_ORGNC
BR0621               ,B.CRSTRB_EMPTO_RURAL
BR0621               ,B.EURL_RSTRB_EMPTO
BR0621               ,B.PENERG_RENOV_EMPTO
               FROM   DB2PRD.TCOMPO_CONTR_CREDT A
                     ,DB2PRD.TORCAM_CONTR_CREDT B
               WHERE A.CORCAM_CONTR_RURAL = B.CORCAM_CONTR_RURAL
               AND   A.CPSSOA_JURID_CONTR = :CRURB0L7.CPSSOA-JURID-CONTR
               AND   A.CTPO_CONTR_NEGOC   = :CRURB0L7.CTPO-CONTR-NEGOC
               AND   A.NSEQ_CONTR_NEGOC   = :CRURB0L7.NSEQ-CONTR-NEGOC
               ORDER BY B.CORCAM_CONTR_RURAL ASC
           END-EXEC.
      *
      * ***  CURSOR LANCAMENTOS PARCELAS                               *
           EXEC SQL
            DECLARE CSR04-CRURB0LZ    CURSOR FOR
               SELECT CPSSOA_JURID_CONTR
                     ,CTPO_CONTR_NEGOC
                     ,NSEQ_CONTR_NEGOC
                     ,NPCELA_CONTR_RURAL
                     ,CLCTO_CONTR_RURAL
                     ,VLCTO_PCELA_EMPTO
               FROM   DB2PRD.TLCTO_PCELA_RURAL
               WHERE CPSSOA_JURID_CONTR = :CRURB0LZ.CPSSOA-JURID-CONTR
               AND   CTPO_CONTR_NEGOC   = :CRURB0LZ.CTPO-CONTR-NEGOC
               AND   NSEQ_CONTR_NEGOC   = :CRURB0LZ.NSEQ-CONTR-NEGOC
ID5270         AND   CLCTO_CONTR_RURAL  = 23
               ORDER BY CLCTO_CONTR_RURAL ASC
           END-EXEC.
      *
      * ***  CURSOR PARCELAS                                           *
           EXEC SQL
            DECLARE CSR06-CRURB0MK-JN   CURSOR FOR
               SELECT A.NPCELA_CONTR_RURAL
                     ,A.DVCTO_PCELA_CONTR
                     ,B.CLCTO_CONTR_RURAL
                     ,B.VLCTO_PCELA_EMPTO
               FROM   DB2PRD.TPCELA_PGTO_CONTR A
                     ,DB2PRD.TLCTO_PCELA_RURAL B
               WHERE A.CPSSOA_JURID_CONTR = B.CPSSOA_JURID_CONTR
               AND   A.CTPO_CONTR_NEGOC   = B.CTPO_CONTR_NEGOC
               AND   A.NSEQ_CONTR_NEGOC   = B.NSEQ_CONTR_NEGOC
               AND   A.NPCELA_CONTR_RURAL = B.NPCELA_CONTR_RURAL
               AND   A.CPSSOA_JURID_CONTR = :CRURB0MK.CPSSOA-JURID-CONTR
               AND   A.CTPO_CONTR_NEGOC   = :CRURB0MK.CTPO-CONTR-NEGOC
               AND   A.NSEQ_CONTR_NEGOC   = :CRURB0MK.NSEQ-CONTR-NEGOC
ID5270         AND   B.CLCTO_CONTR_RURAL  = 23
               ORDER BY A.NPCELA_CONTR_RURAL, B.CLCTO_CONTR_RURAL ASC
           END-EXEC.
      *
      * ***  CURSOR PROPRIETARIOS                                      *
           EXEC SQL
            DECLARE CSR05-CRURB0C4    CURSOR FOR
               SELECT CPSSOA_PPRIE_IMOV
               FROM   DB2PRD.TPPRIE_CREDT_RURAL
               WHERE CIMOV_CREDT_RURAL  = :CRURB0C4.CIMOV-CREDT-RURAL
ID6695         AND   PPRTCP_PPRIE_RURAL > 0
               ORDER BY CPSSOA_PPRIE_IMOV
           END-EXEC.
WP1021*
WP1021* ***  CURSOR SOLO / CICLO / MANEJO                              *
WP1021     EXEC SQL
WP1021      DECLARE CSR09-CRURB0VJ    CURSOR FOR
WP1021         SELECT CTPO_SOLO_AGCLT
WP1021              , CCICLO_CTVDA
WP1021              , CMANUT_SOLO
WP1021         FROM   DB2PRD.TSOLO_IMOV_BENEF
WP1021         WHERE CORCAM_CONTR_RURAL = :CRURB0VJ.CORCAM-CONTR-RURAL
WP1021     END-EXEC.
      *
      * ***  CURSOR CLAUSULA                                           *
           EXEC SQL
            DECLARE CSR08-CRURB0O0    CURSOR FOR
               SELECT CDOCTO_CREDT_RURAL
               FROM   DB2PRD.TCLAUS_CONTR_RURAL
               WHERE CPSSOA_JURID_CONTR  = :CRURB0O0.CPSSOA-JURID-CONTR
               AND   CTPO_CONTR_NEGOC    = :CRURB0O0.CTPO-CONTR-NEGOC
               AND   NSEQ_CONTR_NEGOC    = :CRURB0O0.NSEQ-CONTR-NEGOC
               AND   CPSSOA_JURID       >= :CRURB0O0.CPSSOA-JURID
               AND   CPRODT_SERVC_OPER  >= :CRURB0O0.CPRODT-SERVC-OPER
               AND   NCLAUS_CONTR_PRODT >= :CRURB0O0.NCLAUS-CONTR-PRODT
               AND   NVRSAO_CLAUS_CONTR >= :CRURB0O0.NVRSAO-CLAUS-CONTR
           END-EXEC.
      *
CET        EXEC SQL
CET          DECLARE CSR01-CRURB0LZ-JN CURSOR FOR
CET           SELECT B.DVCTO_PCELA_CONTR
CET                 ,A.VLCTO_PCELA_EMPTO
CET             FROM DB2PRD.TLCTO_PCELA_RURAL A
CET                 ,DB2PRD.TPCELA_PGTO_CONTR B
CET            WHERE B.CPSSOA_JURID_CONTR = :CRURB0MK.CPSSOA-JURID-CONTR
CET              AND B.CTPO_CONTR_NEGOC   = :CRURB0MK.CTPO-CONTR-NEGOC
CET              AND B.NSEQ_CONTR_NEGOC   = :CRURB0MK.NSEQ-CONTR-NEGOC
CET              AND A.CPSSOA_JURID_CONTR = B.CPSSOA_JURID_CONTR
CET              AND A.CTPO_CONTR_NEGOC   = B.CTPO_CONTR_NEGOC
CET              AND A.NSEQ_CONTR_NEGOC   = B.NSEQ_CONTR_NEGOC
CET              AND A.NPCELA_CONTR_RURAL = B.NPCELA_CONTR_RURAL
CET        END-EXEC.
CET   *
      *    TABELA TSIST_PROD_BACEN
RO1603     EXEC SQL
RO1603          INCLUDE CRURB0T2
RO1603     END-EXEC.
      *
STE003*
.     *    TABELA TCORND_IMOV_BENEF
.          EXEC SQL
.              INCLUDE CRURB0V4
STE003     END-EXEC.
STE003*
STE003     EXEC SQL
.            DECLARE CSR01-CRURB0V4 CURSOR FOR
.             SELECT MLATTD_CORND_IMOV
.                   ,MLOGTD_CORND_IMOV
.                   ,MALTTD_CORND_IMOV
.               FROM  DB2PRD.TCORND_IMOV_BENEF
.              WHERE  CORCAM_CONTR_RURAL = :CRURB0V4.CORCAM-CONTR-RURAL
.                AND  CIMOV_CREDT_RURAL  = :CRURB0V4.CIMOV-CREDT-RURAL
.                AND  NSEQ_GLEBA_IMOV    = :CRURB0V4.NSEQ-GLEBA-IMOV
.                AND  CTPO_CORND_GEOGR   = 1
.               WITH  UR
.          END-EXEC.
STE003
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*** FIM DA WORKING CRUR3CY1 ***'.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01 DFHCOMMAREA.
          COPY CRURWIY1.
      *
      *================================================================*
       PROCEDURE DIVISION              USING DFHCOMMAREA.
      *================================================================*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR CONTER AS PRINCIPAIS SECTIONS.          *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

TESTEM     MOVE '0000-INICIAR'             TO WRK-PARAGRAFO
TESTEM     MOVE SPACES                     TO WRK-RLIN-MSGEM-TEXT
TESTEM     MOVE 'ACESSO AO PROGRAMA CRUR3CY1'
TESTEM                                     TO WRK-RLIN-MSGEM-TEXT
TESTEM     PERFORM 9500-ERRO-LIVRE

           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR.
           PERFORM 3000-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELA INICIALIZACAO DO PROGRAMA.             *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE FRWKGERR-REGISTRO
                      FRWKGHEA-REGISTRO
                      FRWKGCIC-REGISTRO
                      FRWKGMOD-REGISTRO
                      CRURWIY1-BLOCO-SAIDA
                      CRURWIY1-BLOCO-RETORNO
ID6260                WRK-ENVIO-NUMCTRLIF
ID6260                WRK-DSPRZ-CORDN
ID6260                WRK-SEQ-ENVIO-RCOR
ID6734                WRK-AREA-CRURW08V
      *
           MOVE SPACES                 TO WRK-B09A-SIS.
           MOVE SPACES                 TO WRK-B09A-SIS2.
      *
           PERFORM 1100-CONSISTIR-ENTRADA
           .
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELA CONSISTENCIA DOS DADOS DE ENTRADA.     *
      *----------------------------------------------------------------*
       1100-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*
      *
           IF  (CRURWIY1-E-CPSSOA-JURID-CONTR NOT NUMERIC)
           OR  (CRURWIY1-E-CPSSOA-JURID-CONTR EQUAL ZEROS)
               MOVE 08                 TO CRURWIY1-COD-RETORNO
               MOVE '0010'             TO CRURWIY1-COD-ERRO
               MOVE 'CRUR0104'         TO CRURWIY1-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF
      *
      *
           IF  (CRURWIY1-E-CTPO-CONTR-NEGOC NOT NUMERIC)
           OR  (CRURWIY1-E-CTPO-CONTR-NEGOC EQUAL ZEROS)
               MOVE 08                 TO CRURWIY1-COD-RETORNO
               MOVE '0020'             TO CRURWIY1-COD-ERRO
               MOVE 'CRUR0105'         TO CRURWIY1-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF
      *
           IF  (CRURWIY1-E-NSEQ-CONTR-NEGOC NOT NUMERIC)
           OR  (CRURWIY1-E-NSEQ-CONTR-NEGOC EQUAL ZEROS)
               MOVE 08                 TO CRURWIY1-COD-RETORNO
               MOVE '0030'             TO CRURWIY1-COD-ERRO
               MOVE 'CRUR0106'         TO CRURWIY1-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF
      *
           IF  (CRURWIY1-E-CPSSOA-BNEFC NOT NUMERIC)
           OR  (CRURWIY1-E-CPSSOA-BNEFC EQUAL ZEROS)
               MOVE 08                 TO CRURWIY1-COD-RETORNO
               MOVE '0040'             TO CRURWIY1-COD-ERRO
               MOVE 'CRUR0160'         TO CRURWIY1-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF
      *
           IF  (CRURWIY1-E-CPSSOA-JURID NOT NUMERIC)
           OR  (CRURWIY1-E-CPSSOA-JURID EQUAL ZEROS)
               MOVE 08                 TO CRURWIY1-COD-RETORNO
               MOVE '0050'             TO CRURWIY1-COD-ERRO
               MOVE 'CRUR0083'         TO CRURWIY1-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF
      *
           IF  (CRURWIY1-E-NSEQ-UND-ORGNZ NOT NUMERIC)
           OR  (CRURWIY1-E-NSEQ-UND-ORGNZ EQUAL ZEROS)
               MOVE 08                 TO CRURWIY1-COD-RETORNO
               MOVE '0060'             TO CRURWIY1-COD-ERRO
               MOVE 'CRUR1649'         TO CRURWIY1-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELO PROCESSAMENTO DO PROGRAMA.             *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
              SET :WRK-TIMESTAMP       = CURRENT_TIMESTAMP
           END-EXEC
      *
           EXEC SQL
              SET :WRK-DATA-ATUAL      = CURRENT DATE
           END-EXEC
      *
           INITIALIZE                  CRURB0V3

           PERFORM 2300-FORMATA-DADOS
           PERFORM 5500-TRATAR-INCLUSAO
EG1117
EG1117     PERFORM UNTIL WRK-CONTINUA  NOT EQUAL 'S'
EG1117         PERFORM 7000-TRATAR-CONTINUACAO
7C0120         PERFORM 5510-TRATAR-INCLUSAO-CONT
EG1117     END-PERFORM.
EG1117
           PERFORM 8300-TRATAR-RETORNO
           .
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR OBTER O MAIOR NUMERO DE ENVIO        *
      *----------------------------------------------------------------*
       2100-SELECIONAR-MAXIMO          SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CRURB0U2
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
      *
           MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
                                       TO CPSSOA-JURID-CONTR
                                       OF CRURB0U2
           MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
                                       TO CTPO-CONTR-NEGOC
                                       OF CRURB0U2
           MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
                                       TO NSEQ-CONTR-NEGOC
                                       OF CRURB0U2
      *
           EXEC SQL
             SELECT MAX(NENVIO_INFO_RECOR)
               INTO :CRURB0U2.NENVIO-INFO-RECOR
               FROM DB2PRD.TCTRL_ENVIO_INFO
             WHERE CPSSOA_JURID_CONTR  =  :CRURB0U2.CPSSOA-JURID-CONTR
               AND CTPO_CONTR_NEGOC    =  :CRURB0U2.CTPO-CONTR-NEGOC
               AND NSEQ_CONTR_NEGOC    =  :CRURB0U2.NSEQ-CONTR-NEGOC
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               SET  DB2-SELECT         TO TRUE
               MOVE 'TCTRL_ENVIO_INFO' TO FRWKGDB2-NOME-TABELA
               MOVE '2100-SELECIONAR-MAXIMO'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0040'             TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF SQLCODE                  EQUAL -305
              MOVE  1                  TO WRK-NENVIO-INFO-RECOR
           ELSE
              MOVE NENVIO-INFO-RECOR   OF CRURB0U2
                                       TO WRK-NENVIO-INFO-RECOR
              ADD   1                  TO WRK-NENVIO-INFO-RECOR

           END-IF
           .
      *
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR BUSCAR DADOS DA AGENCIA.                *
      *----------------------------------------------------------------*
       2131-BUSCAR-DADOS-AGENCIA       SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE WRK-AREA-UORG122B
      *
           MOVE CRURWIY1-E-CPSSOA-JURID
                                       TO UORGW22B-E-CPSSOA-JURID
           MOVE CRURWIY1-E-NSEQ-UND-ORGNZ
                                       TO UORGW22B-E-NSEQ-UND-ORGNZ
      *
           PERFORM 5100-ACESSAR-UORG122B
      *
           MOVE UORGW22B-10-CCNPJ-DEPDC(2:8)
                                       TO CRURY1-CNPJ-ENT-RESPONS(1:8)
           MOVE UORGW22B-10-CLJ-CNPJ-DEPDC
                                       TO CRURY1-CNPJ-ENT-RESPONS(9:4)
           MOVE UORGW22B-10-CDIG-CNPJ-DEPDC
7C5.10                                 TO CRURY1-CNPJ-ENT-RESPONS(13:2)
RC    *   -MOVE UORGW22B-07-CUND-ORGNZ
RC    *                            -TO WRK-CJUNC-DEPDC
           .
      *
      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
EL0915*----------------------------------------------------------------*
.     *    ROTINA PARA BUSCAR VALOR FINANCIADO POR EMPREENDIMENTO      *
.     *----------------------------------------------------------------*
.      2150-OBTER-INFO-NOTA       SECTION.
.     *----------------------------------------------------------------*
.     *
.          INITIALIZE                  CRURWWJI-BLOCO-ENTRADA
.                                      WRK-NOCOR-GRP-PPSTA-NOTA
.                                      WRK-IND10
.                                      REPLACING ALPHANUMERIC BY SPACES
.                                                     NUMERIC BY ZEROS
.     *
.          MOVE 15                     TO CRURWWJI-E-MAX-OCORR
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE 1                      TO CRURWWJI-E-NVRSAO-GRP-FLUXO
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE 310                    TO CRURWWJI-E-CGRP-INFO-FLUXO
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE FNEBWBZI-S-CPSSOA-PPSTA-NEGOC(1)
.                                      TO CRURWWJI-E-CPSSOA-JURID-CONTR
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE FNEBWBZI-S-CCLASF-PPSTA-NEGOC(1)
.                                      TO CRURWWJI-E-CTPO-CONTR-NEGOC
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE FNEBWBZI-S-NSEQ-PPSTA-NEGOC(1)
.                                      TO CRURWWJI-E-NSEQ-CONTR-NEGOC
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE WRK-NOCOR-GRP-PPSTA-NOTA
.                                      TO CRURWWJI-E-NOCOR-GRP-PPSTA
.                                      OF WRK-AREA-CRUR3WJL
.     *
.          EXEC CICS LINK
.              PROGRAM  (WRK-CRUR3WJL)
.              COMMAREA (WRK-AREA-CRUR3WJL)
.              LENGTH          (LENGTH OF WRK-AREA-CRUR3WJL)
.              NOHANDLE
.          END-EXEC
.     *
.          IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
.              MOVE '0120'             TO CRURWIY1-COD-ERRO
.              MOVE '2150-OBTER-INFO-NOTA'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9400-ERRO-CICS
.          END-IF
.     *
.          EVALUATE CRURW00W-COD-RETORNO
.                                      OF WRK-AREA-CRUR3WJL
.            WHEN 00
.            WHEN 01
.                  PERFORM 2180-TRATAR-NOTA
.                  VARYING WRK-IND10   FROM 1 BY 1
.                    UNTIL WRK-IND10   GREATER CRURWWJI-S-MAX-OCORR
.     *
.                 IF (CRURWWJI-S-IND-PAGINACAO
.                                      EQUAL 'S')
.                     CONTINUE
.                  ELSE
.                     MOVE 'S'         TO WRK-CRUR3WJL-FIM1
.                 END-IF
.     *
.            WHEN OTHER
.                 MOVE '0130'          TO CRURWIY1-COD-ERRO
.                 MOVE WRK-CRUR3WJL    TO FRWKGMOD-NOME-MODULO
.                 MOVE 'CRUR1294'      TO CRURWIY1-COD-MENSAGEM
.                 MOVE CRURW00W-COD-RETORNO
.                                      OF WRK-AREA-CRUR3WJL
.                                      TO FRWKGMOD-COD-RETORNO
.                 MOVE CRURW00W-COD-ERRO
.                                      OF WRK-AREA-CRUR3WJL
.                                      TO FRWKGMOD-COD-ERRO
.                 MOVE CRURW00W-COD-MENSAGEM
.                                      OF WRK-AREA-CRUR3WJL
.                                      TO FRWKGMOD-COD-MENSAGEM
.                 MOVE '2150-OBTER-INFO-NOTA'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.                 PERFORM 9300-ERRO-MODULO
.          END-EVALUATE
.     *
.          .
.     *
.     *----------------------------------------------------------------*
.      2150-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *
.     *----------------------------------------------------------------*
.     *    ROTINA PARA TRATAR NOTAS
.     *----------------------------------------------------------------*
.      2180-TRATAR-NOTA                SECTION.
.     *----------------------------------------------------------------*
.     *
.          MOVE CRURWWJI-S-WCONTD-GRP-PPSTA (WRK-IND10)
.                                      TO CRURW13V-REGISTROS
.     *
.          IF CRURW06V-CEMPTO-RURAL    EQUAL CRURW13V-CEMPTO-RURAL
.              MOVE CRURW13V-QEMPTO-NOTA-PROTR
.                                      TO WRK-QEMPTO-NOTA-PROTR
.              COMPUTE WRK-QEMPTO-NOTA-PROTR-TOTAL =
.                      WRK-QEMPTO-NOTA-PROTR-TOTAL +
.                      WRK-QEMPTO-NOTA-PROTR
.          END-IF
.     *
.          IF (CRURWWJI-S-IND-PAGINACAO
.                                       EQUAL 'S')
.              MOVE CRURWWJI-S-NOCOR-GRP-PGN
.                                      TO WRK-NOCOR-GRP-PPSTA-NOTA
.          END-IF
.     *
.          .
.     *
.     *----------------------------------------------------------------*
.      2180-99-FIM.                    EXIT.
EL0915*----------------------------------------------------------------*
      *
EL1115*----------------------------------------------------------------*
.     *    ROTINA PARA BUSCAR VALOR FINANCIADO POR EMPREENDIMENTO      *
.     *----------------------------------------------------------------*
.      2200-OBTER-REC-PROPRIO         SECTION.
.     *----------------------------------------------------------------*
.     *
.          INITIALIZE                  CRURWWJI-BLOCO-ENTRADA
.                                      WRK-IND9
.                                      REPLACING ALPHANUMERIC BY SPACES
.                                                     NUMERIC BY ZEROS
.     *
.          MOVE 15                     TO CRURWWJI-E-MAX-OCORR
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE 1                      TO CRURWWJI-E-NVRSAO-GRP-FLUXO
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE 318                    TO CRURWWJI-E-CGRP-INFO-FLUXO
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE FNEBWBZI-S-CPSSOA-PPSTA-NEGOC(1)
.                                      TO CRURWWJI-E-CPSSOA-JURID-CONTR
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE FNEBWBZI-S-CCLASF-PPSTA-NEGOC(1)
.                                      TO CRURWWJI-E-CTPO-CONTR-NEGOC
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE FNEBWBZI-S-NSEQ-PPSTA-NEGOC(1)
.                                      TO CRURWWJI-E-NSEQ-CONTR-NEGOC
.                                      OF WRK-AREA-CRUR3WJL
.          MOVE WRK-NOCOR-GRP-PPSTA-REC-PRO
.                                      TO CRURWWJI-E-NOCOR-GRP-PPSTA
.                                      OF WRK-AREA-CRUR3WJL
.     *
.          EXEC CICS LINK
.              PROGRAM  (WRK-CRUR3WJL)
.              COMMAREA (WRK-AREA-CRUR3WJL)
.              LENGTH          (LENGTH OF WRK-AREA-CRUR3WJL)
.              NOHANDLE
.          END-EXEC
.     *
.          IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
.              MOVE '0125'             TO CRURWIY1-COD-ERRO
.              MOVE '2200-OBTER-REC-PROPRIO'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9400-ERRO-CICS
.          END-IF
.     *
.          EVALUATE CRURW00W-COD-RETORNO
.                                      OF WRK-AREA-CRUR3WJL
.            WHEN 00
.            WHEN 01
.                  PERFORM 2210-TRATAR-ORCAMENTO
.                  VARYING WRK-IND9    FROM 1 BY 1
.                    UNTIL WRK-IND9    GREATER CRURWWJI-S-MAX-OCORR
.     *
.                 IF (CRURWWJI-S-IND-PAGINACAO
.                                      EQUAL 'S')
.                     CONTINUE
.                 ELSE
.                     MOVE 'S'         TO WRK-CRUR3WJL-FIM1
.                 END-IF
.     *
.            WHEN OTHER
.                 MOVE '0135'          TO CRURWIY1-COD-ERRO
.                 MOVE WRK-CRUR3WJL    TO FRWKGMOD-NOME-MODULO
.                 MOVE 'CRUR1294'      TO CRURWIY1-COD-MENSAGEM
.                 MOVE CRURW00W-COD-RETORNO
.                                      OF WRK-AREA-CRUR3WJL
.                                      TO FRWKGMOD-COD-RETORNO
.                 MOVE CRURW00W-COD-ERRO
.                                      OF WRK-AREA-CRUR3WJL
.                                      TO FRWKGMOD-COD-ERRO
.                 MOVE CRURW00W-COD-MENSAGEM
.                                      OF WRK-AREA-CRUR3WJL
.                                      TO FRWKGMOD-COD-MENSAGEM
.                 MOVE '2200-OBTER-REC-PROPRIO'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.                 PERFORM 9300-ERRO-MODULO
.          END-EVALUATE
.     *
.          .
.     *
.     *----------------------------------------------------------------*
.      2200-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *
.     *----------------------------------------------------------------*
.     *    ROTINA PARA TRATAR ORCAMENTO
.     *----------------------------------------------------------------*
.      2210-TRATAR-ORCAMENTO           SECTION.
.     *----------------------------------------------------------------*
.     *
.          MOVE CRURWWJI-S-WCONTD-GRP-PPSTA (WRK-IND9)
.                                      TO CRURW04V-REGISTROS
.     *
.          MOVE CRURW04V-VORCAM-CONTR-CREDT
.                                      TO WRK-VORCAM-CONTR-CREDT
.          MOVE CRURW04V-VFINAM-EMPTO-CREDT
.                                      TO WRK-VFINAM-EMPTO-CREDT
.     *
.          COMPUTE WRK-VORCAM-CONTR-CREDT-TOTAL =
.                  WRK-VORCAM-CONTR-CREDT-TOTAL +
.                  WRK-VORCAM-CONTR-CREDT
.     *
.          COMPUTE WRK-VFINAM-EMPTO-CREDT-TOTAL =
.                  WRK-VFINAM-EMPTO-CREDT-TOTAL +
.                  WRK-VFINAM-EMPTO-CREDT
.     *
ED2502     IF CRURW04V-CFASE-ORCAM-RURAL  EQUAL 2 OR
ID6098        CRURW04V-RPARTE-ORCAM-RURAL EQUAL 'PLANTIO'
BSI1           STRING CRURW04V-DPREVT-LIBRC-CONTR(7:4)
BSI1                  CRURW04V-DPREVT-LIBRC-CONTR(4:2)
BSI1                  CRURW04V-DPREVT-LIBRC-CONTR(1:2)
BSI1                  DELIMITED BY SIZE INTO WRK-DT-PLANTIO-INI
BSI1           STRING CRURW04V-DFIM-PREVT-UTILZ(7:4)
BSI1                  CRURW04V-DFIM-PREVT-UTILZ(4:2)
BSI1                  CRURW04V-DFIM-PREVT-UTILZ(1:2)
BSI1                  DELIMITED BY SIZE INTO WRK-DT-PLANTIO-FIM
.          ELSE
.              IF CRURW04V-CFASE-ORCAM-RURAL  EQUAL 7 OR
ID6098            CRURW04V-RPARTE-ORCAM-RURAL EQUAL 'COLHEITA'
BSI1              STRING CRURW04V-DPREVT-LIBRC-CONTR(7:4)
BSI1                     CRURW04V-DPREVT-LIBRC-CONTR(4:2)
BSI1                     CRURW04V-DPREVT-LIBRC-CONTR(1:2)
BSI1                     DELIMITED BY SIZE INTO WRK-DT-COLHEITA-INI
BSI1              STRING CRURW04V-DFIM-PREVT-UTILZ(7:4)
BSI1                     CRURW04V-DFIM-PREVT-UTILZ(4:2)
BSI1                     CRURW04V-DFIM-PREVT-UTILZ(1:2)
BSI1                     DELIMITED BY SIZE INTO WRK-DT-COLHEITA-FIM
.              END-IF
.          END-IF
ED2502
.          IF (CRURWWJI-S-IND-PAGINACAO
.                                      EQUAL 'S')
.              MOVE CRURWWJI-S-NOCOR-GRP-PGN
.                                      TO WRK-NOCOR-GRP-PPSTA-REC-PRO
.          END-IF
.     *
.          .
.     *
.     *----------------------------------------------------------------*
.      2210-99-FIM.                    EXIT.
EL1115*----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * FORMATA DADOS ENVIO PRA RCOR7000 CRURWAY1                      *
      *---------------------------------------------------------------*
       2300-FORMATA-DADOS              SECTION.
      *---------------------------------------------------------------*
      *
EL0715     INITIALIZE                  CRURB068
           MOVE 'COR0001  '            TO CRURY1-CODMSG
           MOVE SPACES                 TO CRURY1-NUMCTRLIF
ID6799     MOVE 1                      TO WRK-POSICAO.
ID6799     MOVE ZEROS                  TO WRK-TAM.
      *
      *
      * RECUPERA CNPJ AGENCIA
           PERFORM 2131-BUSCAR-DADOS-AGENCIA
           MOVE ALL '*'                TO CRURY1-CNPJ-PART
           MOVE ALL '*'                TO CRURY1-NUMREF-BCCOROR
      *
           PERFORM 2400-OBTER-DADOS-CRURB008
           MOVE WRK-FORMA-AAAA         TO WRK-ANO-CALE
           MOVE ZEROS                  TO WRK-NUM-FILL
           MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
                                       TO WRK-CCDULA
           MOVE WRK-CEDULA-RECOR       TO CRURY1-NUMCEDL-CRED-RURALIF
      *
      * FORMATA VALOR
           MOVE VCONTR-RURAL           OF CRURB008
                                       TO WRK-VLR15
           MOVE WRK-VLR15-SS         TO WRK-VLR15-PARC
           MOVE WRK-VLR1               TO CRURY1-VLR-TOTOP
      *
      * FORMATA DATA EMS
           MOVE WRK-TIMESTAMP(1:4)     TO WRK-DATADIA-AMD-X(1:4)
           MOVE WRK-TIMESTAMP(6:2)     TO WRK-DATADIA-AMD-X(5:2)
           MOVE WRK-TIMESTAMP(9:2)     TO WRK-DATADIA-AMD-X(7:2)
      *
           MOVE WRK-DATADIA-AMD-N      TO  CRURY1-DTEMS
      *
           PERFORM 2500-OBTER-DADOS-CRURB0MK
           MOVE DVCTO-PCELA-CONTR      OF CRURB0MK
                                       TO WRK-TAM10-CHAR
           MOVE WRK-TAM10-NUM(1:2)     TO CRURY1-DTVENC(7:2)
           MOVE WRK-TAM10-NUM(4:2)     TO CRURY1-DTVENC(5:2)
           MOVE WRK-TAM10-NUM(7:4)     TO CRURY1-DTVENC(1:4)
CAT410     MOVE ALL '*'                TO CRURY1-COD-BASE-PRORROG.
      *
           PERFORM 3200-SELECIONAR-INSTIT
           MOVE CINSTT-SIST-BACEN      OF CRURB0TF
                                       TO WRK-VAR2-NUM
           MOVE WRK-VAR2-CHAR          TO CRURY1-TP-INSTNTO-CRED
      *
      * OBTER PROPOSTA DO CONTRATO
           PERFORM 3422-BUSCAR-PROPOSTA
      * OBTER GRUPOS DE IFORMACAO
           PERFORM 3420-BUSCAR-GRUPOS
      *
           PERFORM 3212-BUSCA-PORTE-BACEN
           MOVE CPORTE-BNEFC-BACEN     OF CRURB055
                                       TO WRK-PORT-NUM
           MOVE WRK-PORT-CHAR          TO CRURY1-TP-CATG-EMIT
      *
           MOVE CRURY1-COR0001-1       TO WRK-B09A-SIS.
           ADD LENGTH OF CRURY1-COR0001-1
                                       TO WRK-TAM.
           COMPUTE WRK-POSICAO         = WRK-TAM + WRK-1.
      *
           MOVE CRURY1-COR0001-1       TO WRK-B09A-SIS2.
           ADD LENGTH OF CRURY1-COR0001-1
                                       TO WRK-TAM2.
           COMPUTE WRK-POSICAO2        = WRK-TAM2 + WRK-1.
      *
      * FORMATA GRUPO EMIT
           MOVE 'N'                    TO  WRK-CODE100-B0MV.
           MOVE 1                      TO WRK-IND1.
           PERFORM 3110-FORMATA-GRUPO-EMIT.
      *
      * FORMATA GRUPO DESTCFIN
           MOVE 1                      TO WRK-IND1.
           PERFORM 3120-FORMATA-GRUPO-DESTCFIN
211025**     UNTIL WRK-IND1            GREATER  WRK-50.
211025       UNTIL WRK-IND1            GREATER  WRK-60.
      *
      *** FORMATAR GRUPO RENEGC
           MOVE 1 TO WRK-IND2.
           PERFORM 3121-FORMATA-GRUPO-RENEGC
             UNTIL WRK-IND2            GREATER  WRK-999.
      *
           PERFORM 3320-OBTER-DADOS-ORIGE
 P-01      MOVE CRURW00V-CREC-BCO-CNTRL
 P-01                                  TO WRK-VAR5-NUM
 P-01      MOVE WRK-VAR4-CHAR-5        TO CRURY1-TP-FNTE-REC
MJ0924*
MJ0924*    VERIFICAR FONTE DE RECURSO PARAMETRIZADO
MJ0924     PERFORM 2310-VER-PARAMETRO
MJ0924     IF  SQLCODE                 EQUAL ZEROS
MJ0924         PERFORM 2320-VERIFICA-FONTE-REC
MJ0924     END-IF
      *
           MOVE WRK-CLIN-SIST-BACEN    TO WRK-COM-SINAL-04-N
           MOVE WRK-SEM-SINAL-04-X     TO CRURY1-CODPROG-LINHA-CRED
      *
7C5.10***  FORMATACAO DO SUBPROGRAMA ALTERADA DO GRUPO 13 P/ GRUPO 5
      *
AS0615*** FORMATAR SUBPROGRAMA
AS0615     PERFORM 3160-FORMATAR-SUBPROGRAMA
      *
ID6151
ID6151     MOVE ZEROS                  TO WRK-IMOV-N-SC.
ID6151
ED2804*** FORMATAR MUNICIPIO
 .         PERFORM 6400-OBTER-MUN-IMOV
ED2804*
ED2105     IF CRURY1-COD-MUNIC          EQUAL ZEROS
 .    ********OBTER IMOVEL DO DETALHE DO PRODUTO
 .            PERFORM 6310-BUSTAR-GRP-309
 .            IF CRURW12V-CMUN         NOT EQUAL ZEROS
 .               MOVE CRURW12V-CMUN    TO WRK-IMOV-N-SC
 .               MOVE WRK-IMOV-N-SICOR-SC
 .                                     TO CRURY1-COD-MUNIC
 .            ELSE
                PERFORM 6410-ACESSAR-CRURB056
ID6703          IF  CRURW03V-CFNALD-RURAL EQUAL  2  AND
ID6703              WRK-ACHOU-CLASF       EQUAL 'S'
ID6703*    -------OBTER IMOVEL DO GRUPO DE CLASSIFICACAO
ID6703              PERFORM 6420-ACESSAR-CRUR3WJL-327
ID6703              MOVE CRURW08V-CMUN TO CRURY1-COD-MUNIC
ID6703          ELSE
 .                  MOVE ZEROS         TO CRURY1-COD-MUNIC
ID6703          END-IF
 .            END-IF
ED2105     END-IF
      *
           MOVE CRURY1-COR0001-5       TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF CRURY1-COR0001-5).
           ADD  LENGTH OF CRURY1-COR0001-5
                                       TO WRK-TAM.
           ADD  LENGTH OF CRURY1-COR0001-5
                                       TO WRK-POSICAO.

RLOS       COMPUTE WRK-NEW-POS  = (WRK-POSICAO - 6)
      *
           MOVE CRURY1-COR0001-5       TO WRK-B09A-SIS2(WRK-POSICAO2:
                                       LENGTH OF CRURY1-COR0001-5).
           ADD  LENGTH OF CRURY1-COR0001-5
                                       TO WRK-TAM2.
           ADD  LENGTH OF CRURY1-COR0001-5
                                       TO WRK-POSICAO2.
RLOS       COMPUTE WRK-NEW-POS2 = (WRK-POSICAO2 - 6)

      *
ED0405*** FORMATAR COORDENADAS
EG1117     MOVE 1                      TO WRK-INDC
STE003*    *OVE 'S'                    *O WRK-BUSCAR-CORDN
 .         PERFORM 3123-FORMATA-GRUPO-PTPOLG
ED0405
      *** FORMATAR CAMPO CONSCD
           MOVE 1 TO WRK-IND2.
           PERFORM 3124-FORMATA-GRUPO-CONSCD
      *
      *** CONTINUA FORMATAR GRUPO DESTCFIN
           MOVE 1     TO WRK-IND1.
      *
      *** COR0001-9
      *
      * FORMATAR VALOR
      *    V A L O R  DAS  P A R C E L A S
           PERFORM 3612-FORMATA-PARCELAS
      *
           MOVE  WRK-VLCTO-PCELA-ACUM  TO WRK-VAR17-NUMS
           MOVE  WRK-VAR17-NUMSS       TO WRK-VAR19-NUM
           MOVE  WRK-VAR19-CHAR        TO CRURY1-VLR-PARCL-CRED.

EL1115     MOVE 'N'                    TO WRK-CRUR3WJL-FIM1
.     *
.          PERFORM 2200-OBTER-REC-PROPRIO
.                                      UNTIL WRK-CRUR3WJL-FIM1 EQUAL 'S'
.     *
.          COMPUTE WRK-VLR-PARCL-REC-PROP       =
.                  WRK-VORCAM-CONTR-CREDT-TOTAL -
.                  WRK-VFINAM-EMPTO-CREDT-TOTAL
.     *
.          MOVE WRK-VLR-PARCL-REC-PROP TO WRK-VAR17-NUMS
.          MOVE WRK-VAR17-NUMSS        TO WRK-VAR19-NUM
.          MOVE WRK-VAR19-CHAR         TO CRURY1-VLR-PARCL-REC-PROP
.     *
EL1115     IF (WRK-VLR-PARCL-REC-PROP  EQUAL ZEROS)
               MOVE  ALL '*'           TO CRURY1-VLR-PARCL-REC-PROP
EL1115     END-IF
      *
           MOVE  ALL '*'               TO CRURY1-VLR-PARCL-SERVICOS.
      *
           PERFORM 3405-FORMATA-TORCAM
      *
      *** FORMATA GRUPO PROPRIETARIOS
           MOVE 1                      TO WRK-IND2.
           MOVE 1                      TO WRK-IND1.
           PERFORM 3125-FORMATA-GRUPO-PROP
             UNTIL WRK-IND2            GREATER  WRK-10.
      *
      *** CONTINUA FORMATAR GRUPO DESTCFIN
           MOVE CRURY1-COD-EMPNMNT     TO WRK-COD-EMPNMNT
      *
           IF WRK-MODALIDADE           EQUAL '01' OR '26'
              MOVE  9                  TO CRURY1-TP-GAR-EMPNMNT
           ELSE
              MOVE  0                  TO CRURY1-TP-GAR-EMPNMNT
           END-IF
      *
EG0617***  REGRA BACEN DE 30/05/2017
EG0617***  DESCONTINUADO CODIGO 0 - NAO SE APLICA. SUBSTITUINDO POR
EG0617***  9 - SEM ADESAO A SEGURO.
EG0617     IF  CRURY1-TP-GAR-EMPNMNT   EQUAL ZEROS
EG0617        MOVE  9                  TO CRURY1-TP-GAR-EMPNMNT
EG0617     END-IF.
EG0617*
           IF(CTPO-SEGUR-CONTT         OF CRURB0V3 EQUAL 1)
              MOVE 3                   TO CRURY1-TP-GAR-EMPNMNT
           ELSE
              IF(CTPO-SEGUR-CONTT      OF CRURB0V3 EQUAL 2)
                 MOVE 1                TO CRURY1-TP-GAR-EMPNMNT
              END-IF
           END-IF
      *
           MOVE  ALL '*'               TO CRURY1-CNPJBASE-INST-MUTNTE.
           MOVE  ALL '*'               TO CRURY1-NUMREF-BCCOR-INVESTMT.
           MOVE  ALL '*'               TO CRURY1-VLR-PARCL-INVESTMT.
      *
      * RECEITA BRUTA
BR1218     PERFORM 3729-ACESSA-CRURB0L5
BR1218*
BR1218     PERFORM 3415-ACESSA-CRURB0SZ
BR1218*
BR1218
BR1218     IF   (CRURW03V-CFNALD-RURAL EQUAL 1)
BR1218     AND  (CMODLD-SIST-BACEN OF CRURB0SZ EQUAL 26 OR 31 OR 01 OR
BR1218                                               35 OR 37 OR 39 OR
BR1218                                               55 OR 60 OR 63 OR
BR1218                                               65 OR 67 OR 73 OR
BR1218                                               75 OR 77 OR 38)
BR1218     AND  (CTPO-BNEFC-BACEN  OF CRURB0L5
BR1218                            NOT EQUAL '002' AND '009')
BR1218          CONTINUE
BR1218     ELSE
BR1218          MOVE ZEROS             TO VEMPTO-RENDA-AGROP OF CRURB068
BR1218     END-IF
BR1218
           IF  VEMPTO-RENDA-AGROP      OF CRURB068
                                       EQUAL ZEROS
               MOVE  ALL '*'           TO CRURY1-VLR-RECBRUTA-ESPRDEMP
           ELSE
               MOVE ZEROS              TO WRK-VLR-RECBRUTA-N
               IF WRK-AREA-FINAN       NOT EQUAL ZEROS
                  COMPUTE  WRK-VLR-RECBRUTA-ESPR ROUNDED
                     =(WRK-AREA-FINAN
                     * WRK-QPROTV-IMOV-ACUM
                     * VEMPTO-RENDA-AGROP OF CRURB068)
BSI999            COMPUTE  WRK-VLR-RECBRUTA-ESPR ROUNDED
BSI999               =(WRK-QPROTV-IMOV-ACUM-AREA
BSI999               * VEMPTO-RENDA-AGROP OF CRURB068)
               ELSE
                  COMPUTE  WRK-VLR-RECBRUTA-ESPR ROUNDED
                     =(WRK-QTDE-FINAN
                     * WRK-QPROTV-IMOV-ACUM
                     * VEMPTO-RENDA-AGROP OF CRURB068)
BSI999            COMPUTE  WRK-VLR-RECBRUTA-ESPR ROUNDED
BSI999               =(WRK-QPROTV-IMOV-ACUM-ITEM
BSI999               * VEMPTO-RENDA-AGROP OF CRURB068)
               END-IF
      *
      *    REMOVER COMPACTACAO
               MOVE  WRK-VLR-RECBRUTA-ESPR
                                       TO WRK-VLR-RECBRUTA-ESPR-SC
      *
      *    REMOCAO DE SINAL E TRANSFORMACAO PARA CHAR
               MOVE WRK-VLR-RECBRUTA-ESPR-SC
                                       TO WRK-VLR-RECBRUTA-N
               MOVE  WRK-VLR-RECBRUTA-A
                                       TO CRURY1-VLR-RECBRUTA-ESPRDEMP
           END-IF
      *
BR1018     IF  CRURY1-VLR-RECBRUTA-ESPRDEMP NOT NUMERIC OR
BR1018         CRURY1-VLR-RECBRUTA-ESPRDEMP EQUAL ZEROS
BR1018         IF   (CRURW03V-VLR-RECEITA-BRUTA IS NUMERIC AND
BR1018               CRURW03V-VLR-RECEITA-BRUTA GREATER ZEROS)
BR1018           MOVE CRURW03V-VLR-RECEITA-BRUTA
BR1018                                 TO WRK-VLR-RECBRUTA-N
BR1018           MOVE WRK-VLR-RECBRUTA-A TO CRURY1-VLR-RECBRUTA-ESPRDEMP
BR1018         END-IF
BR1018     END-IF
BR1018*
BR1218     IF  CRURY1-VLR-RECBRUTA-ESPRDEMP EQUAL ZEROS
BR1218         MOVE ALL '*'            TO CRURY1-VLR-RECBRUTA-ESPRDEMP
BR1218     END-IF
BR1018*
           MOVE CRURY1-COR0001-11      TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF CRURY1-COR0001-11).
           ADD  LENGTH OF CRURY1-COR0001-11 TO WRK-TAM.
           ADD  LENGTH OF CRURY1-COR0001-11 TO WRK-POSICAO.
      *
           MOVE CRURY1-COR0001-11      TO WRK-B09A-SIS2(WRK-POSICAO2:
                                       LENGTH OF CRURY1-COR0001-11).
           ADD  LENGTH OF CRURY1-COR0001-11 TO WRK-TAM2.
           ADD  LENGTH OF CRURY1-COR0001-11 TO WRK-POSICAO2.
      *
      ***
      *** FORMATA GRUPO PARCL
           MOVE 'N'              TO  WRK-CODE100-B0MK.
           MOVE 1                      TO WRK-IND2.
           PERFORM 3127-FORMATA-GRUPO-PARCL
             UNTIL WRK-IND2            GREATER  WRK-999.
      *
AS1202*** FORMATAR GRUPO LIBERACAO
BR4.11****-PERFORM 3170-FORMATA-GRUPO-LIBRC

AS1202*** FORMATA DADOS DE DATA DE PLANTIO E/OU COLHEITA
           PERFORM 3180-FORMATA-PLANTIO-COLHEITA
      *
CAT408*** FORMATA GRUPO DESCLASSIFICACAO
BR4.11****-PERFORM 3190-FORMATA-GRUPO-DESCLAS
      *
QT1120     PERFORM 3190-FORMATA-GRUPO-DESCLAS
      *
BR0521*** FORMATA CODIGO STN DE EQUALIZACAO
BR0521     PERFORM 3800-FORMATA-COD-STN
      *
BR0521*** FORMATA CERTIFICACAO ORGANICA
BR0521     PERFORM 3810-FORMATA-CERTF-ORGNCA
      *
BR0621*** FORMATA RASTREABILIDADE
BR0521     PERFORM 3820-FORMATA-RASTLDD-EMP
      *
BR0621*** FORMATA PERCENTUAL DE ENERGIA RENOVAVEL
BR0521     PERFORM 3830-FORMATA-PENERG-RENOV
      *
WP1021*** FORMATAR SOLO / CULTIVO / MANEJO
WP1021*
WP1021     PERFORM 6430-OBTER-SOLO-CULTIVO.
WP1021     PERFORM 3840-FORMATA-SOLO-CULTIVO.
WP1021*
STE001* --- ENVIO DOS INDICADORES
STE001     IF PREDC-JURO-AMBTL        OF CRURB008
.                                     GREATER ZEROS
.             MOVE  PREDC-JURO-AMBTL  OF CRURB008
.                                     TO WRK-03V2-COM-S
.             MOVE  WRK-03V2-SEM-S
.                                     TO WRK-PERC-CAR-N
.             MOVE  WRK-PERCENTUAL-CAR-X
.                                     TO CRURY1-PERC-BONUS-CAR
.          ELSE
.             MOVE  ALL '*'           TO CRURY1-PERC-BONUS-CAR
.          END-IF.
.
7C5.10     MOVE  ALL '*'              TO CRURY1-PERC-BONUS-SUST-X.
.
.          MOVE CRURY1-COR0001-21A     TO WRK-B09A-SIS(WRK-POSICAO:
.                                      LENGTH OF CRURY1-COR0001-21A).
.          ADD  LENGTH OF CRURY1-COR0001-21A TO WRK-TAM.
STE001     ADD  LENGTH OF CRURY1-COR0001-21A TO WRK-POSICAO.
      *
7C5.10*--- MONTAR GRUPO DE INDICADORES (FNEB)
      *
7C5.10     PERFORM 2650-FORMATA-INDICADORES
.     *
.          IF WRK-TEM-INDICADOR       EQUAL  'N'
.             MOVE ALL'*'             TO CRURY1-TP-CONFDD-X
.
.             MOVE CRURY1-COR0001-21B TO WRK-B09A-SIS(WRK-POSICAO:
.                                     LENGTH OF CRURY1-COR0001-21B)
.             ADD  LENGTH OF CRURY1-COR0001-21B TO WRK-TAM
.             ADD  LENGTH OF CRURY1-COR0001-21B TO WRK-POSICAO
7C5.10     END-IF.
      *
7C5.10*--- MONTAR GRUPO DE LIBERACAO
.     *
.          MOVE ALL'*'                 TO CRURY1-DTLIBRC
.                                         CRURY1-VLR-LIBRC.
.
.          MOVE CRURY1-COR0001-21C     TO WRK-B09A-SIS(WRK-POSICAO:
.                                      LENGTH OF CRURY1-COR0001-21C).
.          ADD  LENGTH OF CRURY1-COR0001-21C TO WRK-TAM.
7C5.10     ADD  LENGTH OF CRURY1-COR0001-21C TO WRK-POSICAO.
      *
7C5.10*--- MONTAR GRUPO COOPERADO / INTEGRADO
.     *
.          MOVE ALL'*'                 TO CRURY1-TP-PESSOA-COOP
.                                         CRURY1-CNPJ-CPF-COOP
.                                         CRURY1-VLR-PARC-COOP
.                                         CRURY1-CODPROG-LINHA-COOP.
.
.          MOVE CRURY1-COR0001-21D     TO WRK-B09A-SIS(WRK-POSICAO:
.                                      LENGTH OF CRURY1-COR0001-21D)
.          ADD  LENGTH OF CRURY1-COR0001-21D TO WRK-TAM
7C5.10     ADD  LENGTH OF CRURY1-COR0001-21D TO WRK-POSICAO.
      *
CAT408*** FORMATA DATA DE MOVIMENTO
CAT408     PERFORM 3150-FORMATA-DTMOVTO
           .
      *
      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
MJ0924*----------------------------------------------------------------*
MJ0924*    VERIFICA O PARAMETRO DE CONTINGENCIA                        *
MJ0924*----------------------------------------------------------------*
MJ0924 2310-VER-PARAMETRO              SECTION.
MJ0924*----------------------------------------------------------------*
MJ0924
MJ0924     INITIALIZE CRURB038
MJ0924
MJ0924     MOVE 'FONTEREC'             TO IREDZD-PARM-CREDT OF CRURB038
MJ0924
MJ0924     EXEC SQL
MJ0924          SELECT RPARM_CREDT_RURAL
MJ0924            INTO :CRURB038.RPARM-CREDT-RURAL
MJ0924                      :WRK-RPARM-CREDT-RURAL-NULL
MJ0924            FROM  DB2PRD.TPARM_CREDT_RURAL
MJ0924           WHERE  IREDZD_PARM_CREDT = :CRURB038.IREDZD-PARM-CREDT
MJ0924             AND  CSIT_REG_RURAL    = 'A'
MJ0924           FETCH  FIRST  1  ROWS  ONLY
MJ0924            WITH  UR
MJ0924     END-EXEC
MJ0924
MJ0924     IF (SQLCODE             NOT EQUAL ZEROS AND +100)
MJ0924     OR (SQLWARN0                EQUAL 'W')
MJ0924         SET  DB2-SELECT         TO TRUE
MJ0924         MOVE 'TPARM_CREDT_RURAL'
MJ0924                                 TO FRWKGDB2-NOME-TABELA
MJ0924         MOVE '2310-VER-PARAMETRO'
MJ0924                                 TO FRWKGHEA-IDEN-PARAGRAFO
MJ0924         MOVE '2310'             TO FRWKGDB2-LOCAL
MJ0924         PERFORM 9000-ERRO-DB2
MJ0924     END-IF
MJ0924*
MJ0924     IF  SQLCODE                 EQUAL +100
MJ0924         MOVE SPACES             TO RPARM-CREDT-RURAL-TEXT
MJ0924                                                      OF CRURB038
MJ0924         GO                      TO 2310-99-FIM
MJ0924     END-IF
MJ0924*
MJ0924     IF  WRK-RPARM-CREDT-RURAL-NULL LESS ZEROS
MJ0924         MOVE SPACES             TO RPARM-CREDT-RURAL-TEXT
MJ0924                                                      OF CRURB038
MJ0924     END-IF
MJ0924     .
MJ0924*----------------------------------------------------------------*
MJ0924 2310-99-FIM.                    EXIT.
MJ0924*----------------------------------------------------------------*
MJ0924*                                                                *
MJ0924*----------------------------------------------------------------*
MJ0924* MONTA FONTE DE RECURSOS PARA RECURSOS ADICIONAIS               *
MJ0924*----------------------------------------------------------------*
MJ0924 2320-VERIFICA-FONTE-REC         SECTION.
MJ0924*----------------------------------------------------------------*
MJ0924*
MJ0924     IF  RPARM-CREDT-RURAL-TEXT  OF CRURB038 EQUAL SPACES
MJ0924         GO TO 2320-99-FIM
MJ0924     END-IF
MJ0924*
MJ0924     MOVE RPARM-CREDT-RURAL-TEXT OF CRURB038
MJ0924                                 TO WRK-LISTA-ORIGENS.
MJ0924*
MJ0924     PERFORM VARYING IND-3 FROM 1 BY 1
MJ0924               UNTIL IND-3 GREATER 28
MJ0924                  OR WRK-CORIGEM-PARM(IND-3) EQUAL SPACES
MJ0924        IF WRK-CORIGEM-PARM(IND-3)    IS NUMERIC AND
MJ0924           WRK-CFONTE-REC-PARM(IND-3) IS NUMERIC
MJ0924           MOVE WRK-CORIGEM-PARM-N(IND-3)
MJ0924                                 TO WRK-CORIGEM-AUX
MJ0924           IF  WRK-CORIGEM-AUX EQUAL CORIGE-REC-CREDT OF CRURB008
MJ0924               MOVE WRK-CFONTE-REC-PARM(IND-3)
MJ0924                                 TO CRURY1-TP-FNTE-REC
MJ0924           END-IF
MJ0924        END-IF
MJ0924     END-PERFORM
MJ0924*
MJ0924     .
MJ0924*----------------------------------------------------------------*
MJ0924 2320-99-FIM.                    EXIT.
MJ0924*----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR OBTER DADOS DO CONTRATO                 *
      *----------------------------------------------------------------*
       2400-OBTER-DADOS-CRURB008       SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
                                       TO CPSSOA-JURID-CONTR
                                       OF CRURB008
           MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
                                       TO CTPO-CONTR-NEGOC
                                       OF CRURB008
           MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
                                       TO NSEQ-CONTR-NEGOC
                                       OF CRURB008
      *
           EXEC SQL
             SELECT VCONTR_RURAL,
                    CORIGE_REC_CREDT,
                    DEFTVC_CONTR_RURAL,
STE001              CINDCD_CONFM_FLORS,
STE001              CINDCD_CONFM_IBAMA,
STE001              CINDCD_CONFM_CSVAO,
STE001              PREDC_JURO_AMBTL,
STE004              COUTRO_TPO_CONFM,
STE004              CINDCD_CONFM_RURAL
               INTO :CRURB008.VCONTR-RURAL,
                    :CRURB008.CORIGE-REC-CREDT,
                    :CRURB008.DEFTVC-CONTR-RURAL,
STE001              :CRURB008.CINDCD-CONFM-FLORS
STE001                  :WRK-CINDCD-CONFM-FLORS-N,
STE001              :CRURB008.CINDCD-CONFM-IBAMA
STE001                  :WRK-CINDCD-CONFM-IBAMA-N,
STE001              :CRURB008.CINDCD-CONFM-CSVAO
STE001                  :WRK-CINDCD-CONFM-CSVAO-N,
STE001              :CRURB008.PREDC-JURO-AMBTL
STE001                  :WRK-PREDC-JURO-AMBTL-N,
STE004              :CRURB008.COUTRO-TPO-CONFM
STE004                  :WRK-COUTRO-TPO-CONFM-N,
STE004              :CRURB008.CINDCD-CONFM-RURAL
STE004                  :WRK-CINDCD-CONFM-RURAL-N
               FROM DB2PRD.TCONTR_CREDT_RURAL
             WHERE CPSSOA_JURID_CONTR  =  :CRURB008.CPSSOA-JURID-CONTR
               AND CTPO_CONTR_NEGOC    =  :CRURB008.CTPO-CONTR-NEGOC
               AND NSEQ_CONTR_NEGOC    =  :CRURB008.NSEQ-CONTR-NEGOC
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET  DB2-SELECT         TO TRUE
               MOVE 'TCTRL_CREDIT_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2400-OBTER-DADOS-CRURB008'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0050'             TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
STE001*
STE001     IF  WRK-CINDCD-CONFM-FLORS-N LESS ZEROS
STE001         MOVE ZEROS               TO CINDCD-CONFM-FLORS
STE001                                  OF CRURB008
STE001     END-IF
STE001*
STE001     IF  WRK-CINDCD-CONFM-IBAMA-N LESS ZEROS
STE001         MOVE ZEROS               TO CINDCD-CONFM-IBAMA
STE001                                  OF CRURB008
STE001     END-IF
STE001*
STE001     IF  WRK-CINDCD-CONFM-CSVAO-N LESS ZEROS
STE001         MOVE ZEROS               TO CINDCD-CONFM-CSVAO
STE001                                  OF CRURB008
STE001     END-IF
STE001*
STE001     IF  WRK-PREDC-JURO-AMBTL-N   LESS ZEROS
STE001         MOVE ZEROS               TO PREDC-JURO-AMBTL
STE001                                  OF CRURB008
STE001     END-IF
STE001*
STE004     IF  WRK-CINDCD-CONFM-RURAL-N LESS ZEROS
STE004         MOVE SPACES              TO CINDCD-CONFM-RURAL
STE004                                  OF CRURB008
STE004     END-IF
STE004*
STE004     IF  WRK-COUTRO-TPO-CONFM-N   LESS ZEROS
STE004         MOVE ZEROS               TO COUTRO-TPO-CONFM
STE004                                  OF CRURB008
STE004     END-IF
STE004*
STE004     IF PREDC-JURO-AMBTL        OF CRURB008
.                                     GREATER ZEROS
.             MOVE  PREDC-JURO-AMBTL  OF CRURB008
.                                     TO WRK-03V2-COM-S
.             MOVE  WRK-03V2-SEM-S
.                                     TO WRK-PERC-CAR-N
.             MOVE  WRK-PERCENTUAL-CAR-X
.                                     TO CRURY1-PERC-BONUS-CAR
.          ELSE
.             MOVE  ALL '*'           TO CRURY1-PERC-BONUS-CAR
.          END-IF.
.
7C5.10     MOVE  ALL '*'              TO CRURY1-PERC-BONUS-SUST-X.
.
7C5.10*----IF CINDCD-CONFM-IBAMA      OF-CRURB008
.     *                               GREATER-ZEROS
.     *      -MOVE  CINDCD-CONFM-IBAMA
.     *        -OF  CRURB008
.     *        -TO  WRK-INDICADOR-TELA
.     *      -PERFORM 5051-TRATAR-DE-PARA
.     *      -MOVE  WRK-INDICADOR-TELA-X
.     *                              -TO-CRURY1-IND-CONF-IBAMA
.     *   -ELSE
.     *      -MOVE  ALL '*'          -TO-CRURY1-IND-CONF-IBAMA
.     *   -END-IF.
.
.     *   -IF CINDCD-CONFM-CSVAO      OF-CRURB008
.     *                              -GREATER ZEROS
.     *      -MOVE  CINDCD-CONFM-CSVAO
.     *        -OF  CRURB008
.     *        -TO  WRK-INDICADOR-TELA
.     *      -PERFORM 5051-TRATAR-DE-PARA
.     *      -MOVE  WRK-INDICADOR-TELA-X
.     *                              -TO-CRURY1-IND-CONF-UC
.     *   -ELSE
.     *      -MOVE  ALL '*'          -TO-CRURY1-IND-CONF-UC
.     *   -END-IF.
.     *
.
.     *   -IF CINDCD-CONFM-FLORS      OF-CRURB008
.     *                              -GREATER-ZEROS
.     *      -MOVE  CINDCD-CONFM-FLORS
.     *        -OF  CRURB008
.     *        -TO  WRK-INDICADOR-TELA
.     *      -PERFORM 5051-TRATAR-DE-PARA
.     *      -MOVE  WRK-INDICADOR-TELA-X
.     *                              -TO-CRURY1-IND-CONF-FLOR-PUBL
.     *   -ELSE
.     *      -MOVE  ALL '*'          -TO-CRURY1-IND-CONF-FLOR-PUBL
.     *   -END-IF.
.
7C5.10*   -IF CINDCD-CONFM-RURAL     -OF-CRURB008
.     *                              -NOT-EQUAL-SPACES
.     *      -MOVE  CINDCD-CONFM-RURAL
.     *        -OF  CRURB008
.     *                              -TO-CRURY1-TPO-CONF
.     *   -ELSE
.     *      -MOVE  ALL-'*'          -TO-CRURY1-TPO-CONF
.     *   -END-IF.
.     *   -IF COUTRO-TPO-CONFM       -OF-CRURB008
.     *                              -NOT-EQUAL-ZEROS
.     *      -PERFORM 2501-OBTER-DADOS-CRURB01F
.     *   -ELSE
.     *      -MOVE  ALL '*'          -TO-CRURY1-IND-CONF-IMOV
7C5.10*   -END-IF.
STE004
STE004     MOVE  ALL '*'              TO CRURY1-IND-CPF-CNPJ
           .
      *
      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *                                                                *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR OBTER ULTIMA PARCELA DO CONTRATO        *
      *----------------------------------------------------------------*
       2500-OBTER-DADOS-CRURB0MK       SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
                                       TO CPSSOA-JURID-CONTR
                                       OF CRURB0MK
           MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
                                       TO CTPO-CONTR-NEGOC
                                       OF CRURB0MK
           MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
                                       TO NSEQ-CONTR-NEGOC
                                       OF CRURB0MK
      *
           EXEC SQL
             SELECT NPCELA_CONTR_RURAL,
                    DVCTO_PCELA_CONTR
               INTO :CRURB0MK.NPCELA-CONTR-RURAL
                   ,:CRURB0MK.DVCTO-PCELA-CONTR
               FROM DB2PRD.TPCELA_PGTO_CONTR
             WHERE CPSSOA_JURID_CONTR  =  :CRURB0MK.CPSSOA-JURID-CONTR
               AND CTPO_CONTR_NEGOC    =  :CRURB0MK.CTPO-CONTR-NEGOC
               AND NSEQ_CONTR_NEGOC    =  :CRURB0MK.NSEQ-CONTR-NEGOC
             ORDER BY NPCELA_CONTR_RURAL DESC
             FETCH FIRST 1 ROWS ONLY
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET  DB2-SELECT         TO TRUE
               MOVE 'TPCELA_PGTO_CONTR'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2500-OBTER-DADOS-CRURB0MK'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0050'             TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
STE004*----------------------------------------------------------------*
.     * ROTINA RESPONSAVEL POR OBTER TIPO DE CONFORMIDADE              *
.     *----------------------------------------------------------------*
.      2501-OBTER-DADOS-CRURB01F       SECTION.
.     *----------------------------------------------------------------*
.          MOVE COUTRO-TPO-CONFM       OF CRURB008
.                                      TO COUTRO-TPO-CONFM
.                                      OF CRURB01F
.     *
.          EXEC SQL
.            SELECT VALUE(COUTRO_CONFM_BACEN, 0)
.              INTO :CRURB01F.COUTRO-CONFM-BACEN
.              FROM DB2PRD.TOUTRO_CONFM_BACEN
.            WHERE COUTRO_TPO_CONFM = :CRURB01F.COUTRO-TPO-CONFM
.            FETCH FIRST 1 ROWS ONLY
.          END-EXEC
.     *
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
.              SET  DB2-SELECT         TO TRUE
.              MOVE 'TOUTRO_CONFM_BACEN'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '2501-OBTER-DADOS-CRURB01F'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              MOVE '0050'             TO FRWKGDB2-LOCAL
.              PERFORM 9000-ERRO-DB2
.          END-IF
.     *
.          IF (SQLCODE                 EQUAL +100)
.              MOVE ZEROS              TO COUTRO-CONFM-BACEN
.                                      OF CRURB01F
.          END-IF
.     *
7C5.10*   -IF-SQLCODE                  EQUAL-ZEROS
.     *      -MOVE COUTRO-CONFM-BACEN  OF-CRURB01F
.     *                               -TO WRK-S9-2
.     *      -MOVE WRK-9-2            -TO WRK-9-2-X
.     *      -MOVE WRK-X-2
.     *                               -TO CRURY1-IND-CONF-IMOV
.     *   -ELSE
.     *      -MOVE ALL '*'            -TO CRURY1-IND-CONF-IMOV
7C5.10*   -END-IF
           .
.     *----------------------------------------------------------------*
.      2501-99-FIM.                    EXIT.
STE004*----------------------------------------------------------------*
CET   *
CET   *----------------------------------------------------------------*
CET   * ROTINA RESPONSAVEL PELA FINALIZACAO DO PROGRAMA.               *
CET   *----------------------------------------------------------------*
CET    2600-OBTER-CET                  SECTION.
CET   *----------------------------------------------------------------*
CET   *
CET        MOVE ZEROS                  TO CRURWCAA-S-CET-ANO
CET        GO TO 2600-99-FIM
CET        PERFORM 2610-OBTER-LIBERACOES
CET        PERFORM 2620-OBTER-PAGAMENTOS
CET        PERFORM 2630-OBTER-SEGUROS
CET   *
CET        PERFORM 2640-ACESSAR-CRUR3CCI
CET        .
CET   *----------------------------------------------------------------*
CET    2600-99-FIM.                    EXIT.
CET   *----------------------------------------------------------------*
CET   *
CET   *----------------------------------------------------------------*
CET    2610-OBTER-LIBERACOES           SECTION.
CET   *----------------------------------------------------------------*
CET   *
CET        INITIALIZE CRURWXVI-BLOCO-ENTRADA
CET   *
CET        MOVE 50                     TO CRURWXVI-E-MAX-OCOR
CET        MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
CET                                    TO CRURWXVI-E-CPSSOA-JURID-CTA
CET        MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
CET                                    TO CRURWXVI-E-CTPO-NEGOC-CTA
CET        MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
CET                                    TO CRURWXVI-E-NSEQ-NEGOC-CTA
CET        MOVE 'I'                    TO CRURWXVI-INDICADOR-PAGINACAO
CET   *
CET        EXEC CICS LINK
CET             PROGRAM  (WRK-CRUR3XVL)
CET             COMMAREA (WRK-AREA-CRUR3XVL)
CET             LENGTH   (LENGTH OF WRK-AREA-CRUR3XVL)
CET             NOHANDLE
CET        END-EXEC
CET   *
CET        IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
CET            MOVE '0120'             TO CRURWIY1-COD-ERRO
CET                                    OF DFHCOMMAREA
CET            MOVE '2610-OBTER-LIBERACOES'
CET                                    TO FRWKGHEA-IDEN-PARAGRAFO
CET            PERFORM 9400-ERRO-CICS
CET        END-IF
CET   *
CET        EVALUATE CRURW00W-COD-RETORNO
CET                                    OF WRK-AREA-CRUR3XVL
CET            WHEN 00
CET            WHEN 01
CET                 CONTINUE
CET            WHEN OTHER
CET                 MOVE CRURW00W-COD-MENSAGEM
CET                                    OF WRK-AREA-CRUR3XVL
CET                                    TO CRURWIY1-COD-MENSAGEM
CET                                    OF DFHCOMMAREA
CET                 MOVE WRK-CRUR3XVL  TO FRWKGMOD-NOME-MODULO
CET                 MOVE CRURW00W-BLOCO-RETORNO
CET                                    OF WRK-AREA-CRUR3XVL
CET                                    TO FRWKGMOD-BLOCO-RETORNO
CET                 MOVE '0130'        TO CRURWIY1-COD-ERRO
CET                                    OF DFHCOMMAREA
CET                 MOVE '2610-OBTER-LIBERACOES'
CET                                    TO FRWKGHEA-IDEN-PARAGRAFO
CET                 PERFORM 9300-ERRO-MODULO
CET        END-EVALUATE.
CET   *
CET   *----------------------------------------------------------------*
CET    2610-99-FIM.                    EXIT.
CET   *----------------------------------------------------------------*
CET   *
CET   *----------------------------------------------------------------*
CET    2620-OBTER-PAGAMENTOS           SECTION.
CET   *----------------------------------------------------------------*
CET   *
CET        PERFORM 2621-OPEN-CSR01-CRURB0LZ-JN
CET   *
CET        PERFORM 2622-FETCH-CSR01-CRURB0LZ-JN
CET                            UNTIL WRK-CSR-LZ-FIM EQUAL 'S'
CET                               OR IND-4          EQUAL 50
CET   *
CET        PERFORM 2623-CLOSE-CSR01-CRURB0LZ-JN
CET        .
CET   *
CET   *----------------------------------------------------------------*
CET    2620-99-FIM.                    EXIT.
CET   *----------------------------------------------------------------*
CET   *
CET   *----------------------------------------------------------------*
CET   *    ROTINA RESPONSAVEL POR ABRIR O CURSOR CSR01-CRURB0LZ-JN     *
CET   *----------------------------------------------------------------*
CET    2621-OPEN-CSR01-CRURB0LZ-JN     SECTION.
CET   *----------------------------------------------------------------*
CET   *
CET        INITIALIZE                  CRURB0LZ
CET                                    CRURB0MK
CET                                    REPLACING ALPHANUMERIC BY SPACES
CET                                                   NUMERIC BY ZEROS
CET   *
CET        MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
CET                                    TO CPSSOA-JURID-CONTR OF CRURB0MK
CET        MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
CET                                    TO CTPO-CONTR-NEGOC   OF CRURB0MK
CET        MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
CET                                    TO NSEQ-CONTR-NEGOC   OF CRURB0MK
CET   *
CET        EXEC SQL
CET             OPEN CSR01-CRURB0LZ-JN
CET        END-EXEC
CET   *
CET        IF (SQLCODE                 NOT EQUAL ZEROS) OR
CET           (SQLWARN0                EQUAL 'W')
CET            SET DB2-OPEN            TO TRUE
CET            MOVE '0140'             TO CRURWIY1-COD-ERRO
CET                                    OF DFHCOMMAREA
CET            MOVE 'TLCTO_PCELA_RURAL'
CET                                    TO FRWKGDB2-NOME-TABELA
CET            MOVE '2621-OPEN-CSR01-CRURB0LZ-JN'
CET                                    TO FRWKGHEA-IDEN-PARAGRAFO
CET            PERFORM 9000-ERRO-DB2
CET        END-IF.
CET   *
CET   *----------------------------------------------------------------*
CET    2621-99-FIM.                    EXIT.
CET   *----------------------------------------------------------------*
CET   *
CET   *----------------------------------------------------------------*
CET   *    ROTINA RESPONSAVEL POR LER O CURSOR CSR01-CRURB0LZ-JN       *
CET   *----------------------------------------------------------------*
CET    2622-FETCH-CSR01-CRURB0LZ-JN    SECTION.
CET   *----------------------------------------------------------------*
CET   *
CET        EXEC SQL
CET            FETCH CSR01-CRURB0LZ-JN
CET            INTO :CRURB0MK.DVCTO-PCELA-CONTR
CET                ,:CRURB0LZ.VLCTO-PCELA-EMPTO
CET        END-EXEC
CET   *
CET        IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
CET           (SQLWARN0                EQUAL 'W')
CET            SET DB2-FETCH           TO TRUE
CET            MOVE '0150'             TO CRURWIY1-COD-ERRO
CET                                    OF DFHCOMMAREA
CET            MOVE 'TLCTO_PCELA_RURAL'
CET                                    TO FRWKGDB2-NOME-TABELA
CET            MOVE '2622-FETCH-CSR01-CRURB0LZ-JN'
CET                                    TO FRWKGHEA-IDEN-PARAGRAFO
CET            PERFORM 9000-ERRO-DB2
CET        END-IF
CET   *
CET        IF (SQLCODE                 EQUAL +100)
CET            MOVE 'S'                TO WRK-CSR-LZ-FIM
CET        ELSE
CET            ADD 1                   TO IND-4
CET            MOVE VLCTO-PCELA-EMPTO  TO WRK-13V2-COM-S
CET            MOVE WRK-13V2-SEM-S     TO WRK-VLCTO-PCELA(IND-4)
CET            MOVE DVCTO-PCELA-CONTR  OF CRURB0MK
CET                                    TO WRK-DATA-PCELA(IND-4)
CET        END-IF.
CET   *
CET   *----------------------------------------------------------------*
CET    2622-99-FIM.                    EXIT.
CET   *----------------------------------------------------------------*
CET   *
CET   *----------------------------------------------------------------*
CET   *    ROTINA RESPONSAVEL FOR FECHAR O CURSOR CSR01-CRURB0LZ-JN    *
CET   *----------------------------------------------------------------*
CET    2623-CLOSE-CSR01-CRURB0LZ-JN    SECTION.
CET   *----------------------------------------------------------------*
CET   *
CET        EXEC SQL
CET            CLOSE CSR01-CRURB0LZ-JN
CET        END-EXEC
CET   *
CET        IF (SQLCODE                 NOT EQUAL ZEROS) OR
CET           (SQLWARN0                EQUAL 'W')
CET            SET DB2-CLOSE           TO TRUE
CET            MOVE '0160'             TO CRURWIY1-COD-ERRO
CET                                    OF DFHCOMMAREA
CET            MOVE 'TLCTO_PCELA_RURAL'
CET                                    TO FRWKGDB2-NOME-TABELA
CET            MOVE '2623-CLOSE-CSR01-CRURB0LZ-JN'
CET                                    TO FRWKGHEA-IDEN-PARAGRAFO
CET            PERFORM 9000-ERRO-DB2
CET        END-IF.
CET   *
CET   *----------------------------------------------------------------*
CET    2623-99-FIM.                    EXIT.
CET   *----------------------------------------------------------------*
CET   *
CET   *----------------------------------------------------------------*
CET    2630-OBTER-SEGUROS              SECTION.
CET   *----------------------------------------------------------------*
CET   *
CET        MOVE 'I'                    TO WRK-IND-PAG-ORCAM
CET        MOVE ZEROS                  TO IND-S
CET        PERFORM 2632-OBTER-GRP-318
CET          UNTIL WRK-FIM-ORCAM    EQUAL 'S'
CET        .
CET   *
CET   *----------------------------------------------------------------*
CET    2630-99-FIM.                    EXIT.
CET   *----------------------------------------------------------------*
CET   *
CET   *----------------------------------------------------------------*
CET    2632-OBTER-GRP-318              SECTION.
CET   *----------------------------------------------------------------*
CET   *
CET        INITIALIZE                  FNEBW3PI-BLOCO-ENTRADA
CET                                    OF WRK-AREA-FNEB33PL
CET                                    FNEBW000-BLOCO-RETORNO
CET                                    OF WRK-AREA-FNEB33PL
CET   *
CET        MOVE 15                     TO FNEBW3PI-E-MAX-OCORR
CET        MOVE 1                      TO FNEBW3PI-E-NVRSAO-GRP-FLUXO
CET        MOVE 318                    TO FNEBW3PI-E-CGRP-INFO-FLUXO
CET   *
CET        MOVE FNEBWBZI-S-CPSSOA-PPSTA-NEGOC(1)
CET                                    TO FNEBW3PI-E-CPSSOA-JURID-CONTR
CET   *
CET        MOVE FNEBWBZI-S-CCLASF-PPSTA-NEGOC(1)
CET                                    TO FNEBW3PI-E-CTPO-CONTR-NEGOC
CET   *
CET        MOVE FNEBWBZI-S-NSEQ-PPSTA-NEGOC  (1)
CET                                    TO FNEBW3PI-E-NSEQ-CONTR-NEGOC
CET   *
CET        MOVE WRK-IND-PAG-ORCAM      TO FNEBW3PI-INDICADOR-PAGINACAO
CET   *
CET        EXEC CICS LINK
CET            PROGRAM  (WRK-FNEB33PL)
CET            COMMAREA (WRK-AREA-FNEB33PL)
CET            LENGTH   (LENGTH        OF WRK-AREA-FNEB33PL)
CET            NOHANDLE
CET        END-EXEC
CET   *
CET        IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
CET            MOVE '0190'             TO CRURWIY1-COD-ERRO
CET                                    OF DFHCOMMAREA
CET            MOVE '2632-OBTER-GRP-318'
CET                                    TO FRWKGHEA-IDEN-PARAGRAFO
CET            PERFORM 9400-ERRO-CICS
CET        END-IF
CET   *
CET        EVALUATE FNEBW000-COD-RETORNO
CET                                    OF WRK-AREA-FNEB33PL
CET          WHEN 00
CET              MOVE 'S'              TO WRK-FIM-ORCAM
CET          WHEN 01
CET               MOVE 'S'             TO WRK-IND-PAG-ORCAM
CET               MOVE 'N'             TO WRK-FIM-ORCAM
CET          WHEN OTHER
CET            IF FNEBW000-COD-MENSAGEM OF WRK-AREA-FNEB33PL
CET                                    EQUAL 'FNEB0025'
CET               CONTINUE
CET            ELSE
CET               MOVE FNEBW000-COD-MENSAGEM
CET                                    OF WRK-AREA-FNEB33PL
CET                                    TO CRURWIY1-COD-MENSAGEM
CET                                    OF DFHCOMMAREA
CET               MOVE WRK-FNEB33PL    TO FRWKGMOD-NOME-MODULO
CET               MOVE FNEBW000-BLOCO-RETORNO
CET                                    OF WRK-AREA-FNEB33PL
CET                                    TO FRWKGMOD-BLOCO-RETORNO
CET               MOVE '0200'          TO CRURWIY1-COD-ERRO
CET                                    OF DFHCOMMAREA
CET               MOVE '2632-OBTER-GRP-318'
CET                                    TO FRWKGHEA-IDEN-PARAGRAFO
CET               PERFORM 9300-ERRO-MODULO
CET            END-IF
CET        END-EVALUATE
CET   *
CET        PERFORM VARYING IND-1 FROM 1 BY 1
CET                  UNTIL IND-1 GREATER FNEBW3PI-S-MAX-OCORR
CET                     OR IND-1 GREATER 15
CET           MOVE FNEBW3PI-S-WCONTD-GRP-PPSTA(IND-1)
CET                                    TO CRURW04V-REGISTROS
CET   *
CET           MOVE ZEROS               TO WRK-COUNT-SEG
CET           INSPECT CRURW04V-RPARTE-ORCAM-RURAL
CET                   TALLYING WRK-COUNT-SEG FOR ALL 'SEGURO'
CET   *
CET           IF  WRK-COUNT-SEG             GREATER ZEROS
CET           AND CRURW04V-VFINAM-EMPTO-CREDT EQUAL WRK-VAPOLC-SS
CET           AND CRURW04V-CTPO-LIBRC-CONTR   EQUAL 1
CET               ADD 1                TO IND-S
CET               MOVE CRURW04V-VFINAM-EMPTO-CREDT
CET                                    TO WRK-TAB-SEG-VLR (IND-S)
CET               MOVE CRURW04V-DPREVT-LIBRC-CONTR
CET                                    TO WRK-TAB-SEG-DATA (IND-S)
CET           END-IF
CET           IF  CRURW04V-RPARTE-ORCAM-RURAL   EQUAL
CET                                         'SEGURO SWISS RE'
CET               MOVE CRURW04V-VFINAM-EMPTO-CREDT
CET                                    TO WRK-VLR-SWISS
CET               MOVE 'S'             TO WRK-IND-FINANC
CET           END-IF
CET   *
CET           IF  CRURW04V-RPARTE-ORCAM-RURAL   EQUAL
CET                                   'SEGURO SWISS RE (A VISTA)'
CET               MOVE CRURW04V-VFINAM-EMPTO-CREDT
CET                                    TO WRK-VLR-SWISS
CET               MOVE 'N'             TO WRK-IND-FINANC
CET           END-IF
CET        END-PERFORM
CET        .
CET   *
CET   *----------------------------------------------------------------*
CET    2632-99-FIM.                    EXIT.
CET   *----------------------------------------------------------------*
CET   *
CET   *----------------------------------------------------------------*
CET    2640-ACESSAR-CRUR3CCI           SECTION.
CET   *----------------------------------------------------------------*
CET   *
CET        PERFORM 2641-MONTA-ENT-CRUR3CCI
TESTEM     MOVE '2640-ACESSAR-CRUR3CCI'    TO WRK-PARAGRAFO
TESTEM     MOVE SPACES                     TO WRK-RLIN-MSGEM-TEXT
TESTEM     MOVE CRURWCAA-BLOCO-ENTRADA(1:400)
TESTEM                                     TO WRK-RLIN-MSGEM-TEXT
TESTEM     PERFORM 9500-ERRO-LIVRE
CET   *
CET        EXEC CICS LINK
CET             PROGRAM  (WRK-CRUR3CCI)
CET             COMMAREA (WRK-AREA-CRUR3CCI)
CET             LENGTH   (LENGTH OF WRK-AREA-CRUR3CCI)
CET             NOHANDLE
CET        END-EXEC
CET   *
CET        IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
CET            MOVE '0210'             TO CRURWIY1-COD-ERRO
CET                                    OF DFHCOMMAREA
CET            MOVE '2640-ACESSAR-CRUR3CCI'
CET                                    TO FRWKGHEA-IDEN-PARAGRAFO
CET            PERFORM 9400-ERRO-CICS
CET        END-IF
CET   *
TESTEM     MOVE '2640-ACESSAR-CRUR3CCI'    TO WRK-PARAGRAFO
TESTEM     MOVE SPACES                     TO WRK-RLIN-MSGEM-TEXT
TESTEM     MOVE CRURWCAA-S-BLOCO-SAIDA(1:400)
TESTEM                              TO WRK-RLIN-MSGEM-TEXT
TESTEM     PERFORM 9500-ERRO-LIVRE
CET        EVALUATE CRURW00W-COD-RETORNO
CET                                    OF WRK-AREA-CRUR3CCI
CET            WHEN 00
CET            WHEN 01
CET                 CONTINUE
CET            WHEN OTHER
CET                 MOVE CRURW00W-COD-MENSAGEM
CET                                    OF WRK-AREA-CRUR3CCI
CET                                    TO CRURWIY1-COD-MENSAGEM
CET                                    OF DFHCOMMAREA
CET                 MOVE WRK-CRUR3CCI  TO FRWKGMOD-NOME-MODULO
CET                 MOVE CRURW00W-BLOCO-RETORNO
CET                                    OF WRK-AREA-CRUR3CCI
CET                                    TO FRWKGMOD-BLOCO-RETORNO
CET                 MOVE '0220'        TO CRURWIY1-COD-ERRO
CET                                    OF DFHCOMMAREA
CET                 MOVE '2640-ACESSAR-CRUR3CCI'
CET                                    TO FRWKGHEA-IDEN-PARAGRAFO
CET                 PERFORM 9300-ERRO-MODULO
CET        END-EVALUATE.
CET   *
CET   *----------------------------------------------------------------*
CET    2640-99-FIM.                    EXIT.
CET   *----------------------------------------------------------------*
CET   *
CET   *----------------------------------------------------------------*
CET    2641-MONTA-ENT-CRUR3CCI         SECTION.
CET   *----------------------------------------------------------------*
CET   *
CET        PERFORM 2642-OBTER-FONTE-REC
CET
CET        IF  CFONTE-BACEN-RURAL OF CRURB037 EQUAL 2
CET            MOVE 365                TO CRURWCAA-E-BASE-CALCULO
CET        ELSE
CET            MOVE 365                TO CRURWCAA-E-BASE-CALCULO
CET        END-IF
CET
CET        MOVE PTX-JURO-EMPTO         OF CRURB0MB
CET                                    TO CRURWCAA-E-TXA-JUR-ANO
CET
CET        MOVE ZEROS                  TO IND-2
CET   *
CET        PERFORM VARYING IND-1 FROM 1 BY 1
CET                  UNTIL IND-1 GREATER CRURWXVI-NUM-CONSULTAS
CET           ADD  1                   TO IND-2
CET           MOVE 'L'                 TO CRURWCAA-E-TP-EVENTO(IND-2)
CET           MOVE CRURWXVI-S-DATA-LIBRC-CONTR (IND-1)
CET                                    TO CRURWCAA-E-DATA     (IND-2)
CET           MOVE CRURWXVI-S-VALOR-LIBRC-CONTR(IND-1)
CET                                    TO CRURWCAA-E-VALOR    (IND-2)
CET        END-PERFORM
CET   *
CET        PERFORM VARYING IND-1 FROM 1 BY 1
CET                  UNTIL IND-1 GREATER IND-4
CET           ADD  1                   TO IND-2
CET           MOVE 'A'                 TO CRURWCAA-E-TP-EVENTO(IND-2)
CET           MOVE WRK-VLCTO-PCELA(IND-1)
CET                                    TO CRURWCAA-E-VALOR    (IND-2)
CET           MOVE WRK-DATA-PCELA (IND-1)
CET                                    TO CRURWCAA-E-DATA     (IND-2)
CET        END-PERFORM
CET   *
CET        PERFORM VARYING IND-1 FROM 1 BY 1
CET                  UNTIL IND-1 GREATER IND-S
CET           ADD  1                   TO IND-2
CET           MOVE 'S'                 TO CRURWCAA-E-TP-EVENTO(IND-2)
CET           MOVE WRK-TAB-SEG-VLR (IND-1)
CET                                    TO CRURWCAA-E-VALOR    (IND-2)
CET           MOVE WRK-TAB-SEG-DATA(IND-1)
CET                                    TO CRURWCAA-E-DATA     (IND-2)
CET        END-PERFORM
CET   *
CET        MOVE IND-2                  TO CRURWCAA-E-QTD-OCORRENCIAS
CET        .
CET   *
CET   *----------------------------------------------------------------*
CET    2641-99-FIM.                    EXIT.
CET   *----------------------------------------------------------------*
CET   *
CET   *----------------------------------------------------------------*
CET    2642-OBTER-FONTE-REC            SECTION.
CET   *----------------------------------------------------------------*
CET   *                                                                *
CET        INITIALIZE CRURB037
CET                   CRURB008
CET   *
CET        MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
CET                                    TO CPSSOA-JURID-CONTR OF CRURB008
CET        MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
CET                                    TO CTPO-CONTR-NEGOC   OF CRURB008
CET        MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
CET                                    TO NSEQ-CONTR-NEGOC   OF CRURB008
CET   *
CET        EXEC SQL
CET          SELECT A.CFONTE_BACEN_RURAL
CET           INTO :CRURB037.CFONTE-BACEN-RURAL
CET                     :WRK-CFONTE-BACEN-RURAL-N
CET            FROM DB2PRD.TORIGE_REC_RURAL A
CET            JOIN DB2PRD.TCONTR_CREDT_RURAL B
CET              ON A.CORIGE_REC_CREDT  =  B.CORIGE_REC_CREDT
CET           WHERE CPSSOA_JURID_CONTR  = :CRURB008.CPSSOA-JURID-CONTR
CET             AND CTPO_CONTR_NEGOC    = :CRURB008.CTPO-CONTR-NEGOC
CET             AND NSEQ_CONTR_NEGOC    = :CRURB008.NSEQ-CONTR-NEGOC
CET        END-EXEC
CET
CET        IF (SQLCODE             NOT EQUAL ZEROS AND +100)
CET        OR (SQLWARN0                EQUAL 'W')
CET            SET  DB2-SELECT         TO TRUE
CET            MOVE 'TORIGE_REC_RURAL' TO FRWKGDB2-NOME-TABELA
CET            MOVE '2642-OBTER-FONTE-REC'
CET                                    TO FRWKGHEA-IDEN-PARAGRAFO
CET            MOVE '0230'             TO CRURWIY1-COD-ERRO
CET                                    OF DFHCOMMAREA
CET            PERFORM 9000-ERRO-DB2
CET        END-IF
CET
CET        IF  SQLCODE                 EQUAL +100
CET            MOVE ZEROS              TO CFONTE-BACEN-RURAL OF CRURB037
CET        END-IF
CET   *
CET        IF  WRK-CFONTE-BACEN-RURAL-N LESS ZEROS
CET            MOVE ZEROS              TO CFONTE-BACEN-RURAL OF CRURB037
CET        END-IF
CET        .
CET   *
CET   *----------------------------------------------------------------*
CET    2642-99-FIM.                    EXIT.
CET   *----------------------------------------------------------------*
      *
7C5.10*----------------------------------------------------------------*
.      2650-FORMATA-INDICADORES        SECTION.
.     *----------------------------------------------------------------*
.     *
.          INITIALIZE                  FNEBW3PI-BLOCO-ENTRADA
.                                      OF WRK-AREA-FNEB33PL
.                                      FNEBW000-BLOCO-RETORNO
.                                      OF WRK-AREA-FNEB33PL
.     *
.          MOVE 'I'                    TO FNEBW3PI-INDICADOR-PAGINACAO
.          MOVE 01                     TO FNEBW3PI-E-MAX-OCORR
.          MOVE 1                      TO FNEBW3PI-E-NVRSAO-GRP-FLUXO
.          MOVE 326                    TO FNEBW3PI-E-CGRP-INFO-FLUXO
.     *
.          MOVE FNEBWBZI-S-CPSSOA-PPSTA-NEGOC(1)
.                                      TO FNEBW3PI-E-CPSSOA-JURID-CONTR
.     *
.          MOVE FNEBWBZI-S-CCLASF-PPSTA-NEGOC(1)
.                                      TO FNEBW3PI-E-CTPO-CONTR-NEGOC
.     *
.          MOVE FNEBWBZI-S-NSEQ-PPSTA-NEGOC  (1)
.                                      TO FNEBW3PI-E-NSEQ-CONTR-NEGOC
TESTEM     MOVE '2650-FORMATA-INDICADORES' TO WRK-PARAGRAFO
TESTEM     MOVE SPACES                     TO WRK-RLIN-MSGEM-TEXT
DEBUG      STRING
DEBUG             ' CPSSOA-PPSTA-NEGOC.: 'FNEBW3PI-E-CPSSOA-JURID-CONTR
DEBUG             ' CCLASF-PPSTA-NEGOC.: 'FNEBW3PI-E-CTPO-CONTR-NEGOC
DEBUG             ' NSEQ-PPSTA-NEGOC...: 'FNEBW3PI-E-NSEQ-CONTR-NEGOC
DEBUG             DELIMITED BY SIZE           INTO WRK-RLIN-MSGEM-TEXT
TESTEM     PERFORM 9500-ERRO-LIVRE
.     *
.          EXEC CICS LINK
.              PROGRAM  (WRK-FNEB33PL)
.              COMMAREA (WRK-AREA-FNEB33PL)
.              LENGTH   (LENGTH        OF WRK-AREA-FNEB33PL)
.              NOHANDLE
.          END-EXEC
.     *
.          IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
.              MOVE '0190'             TO CRURWIY1-COD-ERRO
.                                      OF DFHCOMMAREA
.              MOVE '2650-FORMATA-INDICADORES'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9400-ERRO-CICS
.          END-IF
.     *
.          EVALUATE FNEBW000-COD-RETORNO
.                                      OF WRK-AREA-FNEB33PL
.            WHEN 00
.            WHEN 01
.                  MOVE FNEBW3PI-S-WCONTD-GRP-PPSTA(1)
.                                      TO WRK-AREA-CRURW07V
.            WHEN OTHER
.              IF FNEBW000-COD-MENSAGEM OF WRK-AREA-FNEB33PL
.                                      EQUAL 'FNEB0025'
.                 MOVE ZEROS           TO FNEBW3PI-S-MAX-OCORR
.              ELSE
.                 MOVE FNEBW000-COD-MENSAGEM
.                                      OF WRK-AREA-FNEB33PL
.                                      TO CRURWIY1-COD-MENSAGEM
.                                      OF DFHCOMMAREA
.                 MOVE WRK-FNEB33PL    TO FRWKGMOD-NOME-MODULO
.                 MOVE FNEBW000-BLOCO-RETORNO
.                                      OF WRK-AREA-FNEB33PL
.                                      TO FRWKGMOD-BLOCO-RETORNO
.                 MOVE '0200'          TO CRURWIY1-COD-ERRO
.                                      OF DFHCOMMAREA
.                 MOVE '2650-FORMATA-INDICADORES'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.                 PERFORM 9300-ERRO-MODULO
.              END-IF
.          END-EVALUATE
.     *
.          MOVE 'N'                    TO WRK-TEM-INDICADOR
.     *
TESTEM     MOVE '2651-FORMATA-INDICADORES'
TESTEM                                     TO WRK-PARAGRAFO
TESTEM     MOVE SPACES                     TO WRK-RLIN-MSGEM-TEXT
TESTEM     MOVE CRURW07V-TIPO-CONFNOVO-N(1)
TESTEM                                     TO WRK-RLIN-MSGEM-TEXT
TESTEM     PERFORM 9500-ERRO-LIVRE

.          PERFORM VARYING IND-1 FROM 1 BY 1
.                  UNTIL   IND-1 GREATER 20
.            IF CRURW07V-TIPO-CONFNOVO-N(IND-1)  NUMERIC
.               MOVE CRURW07V-TIPO-CONFNOVO-N(IND-1) (2:2)
.                                        TO CRURY1-TP-CONFDD-X
.
.               MOVE CRURY1-COR0001-21B  TO WRK-B09A-SIS(WRK-POSICAO:
.                                      LENGTH OF CRURY1-COR0001-21B)
.               ADD  LENGTH OF CRURY1-COR0001-21B TO WRK-TAM
.               ADD  LENGTH OF CRURY1-COR0001-21B TO WRK-POSICAO
.               MOVE 'S'                 TO WRK-TEM-INDICADOR
.            END-IF
.          END-PERFORM
.          .
.     *
.     *----------------------------------------------------------------*
.      2650-99-FIM.                    EXIT.
7C5.10*----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELA FINALIZACAO DO PROGRAMA.               *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS
               RETURN
           END-EXEC
           .
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DO CURSOR CSR01-CRURB0MV                    *
      *    20 - TITULAR CONTRATO
      *    13 - BENEFICIARIOS
      *    15 - OPERADORA
      *----------------------------------------------------------------*
       3011-OPEN-CSR01-CRURB0MV-JN     SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
                                       TO CPSSOA-JURID-CONTR
                                       OF CRURB0MV
           MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
                                       TO CTPO-CONTR-NEGOC
                                       OF CRURB0MV
           MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
                                       TO NSEQ-CONTR-NEGOC
                                       OF CRURB0MV
           MOVE 15                     TO CTPO-RESP-UND
                                       OF CRURB0MV
      *
           EXEC SQL
               OPEN CSR01-CRURB0MV-JN
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-OPEN            TO TRUE
               MOVE '0010'             TO FRWKGDB2-LOCAL
               MOVE 'TRESP_UND_RURAL'  TO FRWKGDB2-NOME-TABELA
               MOVE '3011-OPEN-CSR01-CRURB0MV-JN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3011-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DO CURSOR CSR06-CRURB0MK-JN
      *----------------------------------------------------------------*
       3021-OPEN-CSR06-CRURB0MK-JN     SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
                                       TO CPSSOA-JURID-CONTR
                                       OF CRURB0MK
           MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
                                       TO CTPO-CONTR-NEGOC
                                       OF CRURB0MK
           MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
                                       TO NSEQ-CONTR-NEGOC
                                       OF CRURB0MK
      *
           EXEC SQL
               OPEN CSR06-CRURB0MK-JN
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-OPEN            TO TRUE
               MOVE '0099'             TO FRWKGDB2-LOCAL
               MOVE 'TPCELA_PGTO_CONTR'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3021-OPEN-CSR06-CRURB0MK-JN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3021-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FETCH CSR06-CRURB0MK-JN
      *----------------------------------------------------------------*
       3022-FETCH-CSR06-CRURB0MK-JN    SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
             FETCH CSR06-CRURB0MK-JN INTO
                   :CRURB0MK.NPCELA-CONTR-RURAL
                  ,:CRURB0MK.DVCTO-PCELA-CONTR
                  ,:CRURB0LZ.CLCTO-CONTR-RURAL
                  ,:CRURB0LZ.VLCTO-PCELA-EMPTO
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-FETCH           TO TRUE
               MOVE '0100'             TO FRWKGDB2-LOCAL
               MOVE 'PCELA_PGTO_CONTR' TO FRWKGDB2-NOME-TABELA
               MOVE '3022-FETCH-CSR06-CRURB0MK-JN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF (SQLCODE                 EQUAL +100)
              MOVE 'S'                 TO  WRK-CODE100-B0MK
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3022-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      * ROTINA DE FECHAMENTO DO CURSOR CSR06-CRURB0MK-JN
      *----------------------------------------------------------------*
       3024-FECHAR-CSR06-CRURB0MK-JN  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               CLOSE CSR06-CRURB0MK-JN
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '0101'             TO FRWKGDB2-LOCAL
               MOVE 'TPCELA_PGTO_CONTR'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3024-FECHAR-CSR06-CRURB0MK-JN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3024-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       3110-FORMATA-GRUPO-EMIT  SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 3011-OPEN-CSR01-CRURB0MV-JN.
           PERFORM 3311-FETCH-CSR01-CRURB0MV-JN.
           PERFORM UNTIL (WRK-IND1     GREATER  WRK-999)
                      OR (WRK-CODE100-B0MV
                                       EQUAL 'S')
               PERFORM 3132-BUSCAR-DADOS-CLUB
               PERFORM 3311-FETCH-CSR01-CRURB0MV-JN
           END-PERFORM
      *
           PERFORM 3312-FECHAR-CSR01-CRURB0MV-JN
      *
           .
      *
      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       3120-FORMATA-GRUPO-DESTCFIN     SECTION.
      *---------------------------------------------------------------*
      *
      *****************************************************************
211025***** O GRUPO DESTCFIN PODE SER FORMATADO DE 1 ATE 60 VEZES
      ***** NO LEGADO O GRUPO E FORMATADO APENAS 1 VEZ
      *****************************************************************
      *** FORMATAR GRUPO DESTCFIN
           MOVE CRURY1-COR0001-3       TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF CRURY1-COR0001-3).
           ADD  LENGTH OF CRURY1-COR0001-3
                                       TO WRK-TAM.
           ADD  LENGTH OF CRURY1-COR0001-3
                                       TO WRK-POSICAO.
      *
           MOVE CRURY1-COR0001-3       TO WRK-B09A-SIS2(WRK-POSICAO2:
                                       LENGTH OF CRURY1-COR0001-3).
           ADD  LENGTH OF CRURY1-COR0001-3
                                       TO WRK-TAM2.
           ADD  LENGTH OF CRURY1-COR0001-3
                                       TO WRK-POSICAO2.
      *
211025**  XADD 51                      TO WRK-IND1.
211025     ADD 61                      TO WRK-IND1.
      *
      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       3121-FORMATA-GRUPO-RENEGC       SECTION.
      *---------------------------------------------------------------*
      *
           IF WRK-IND2  EQUAL  1
              MOVE  ALL '*'            TO CRURY1-NUMREF-BCCOR-RENEGC
              MOVE  ALL '*'            TO CRURY1-VLR-RENEGC
BSI1          MOVE  ALL '*'            TO CRURY1-COD-BASE-RENEG
              MOVE CRURY1-COR0001-4    TO
                   WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF CRURY1-COR0001-4)
              ADD LENGTH OF CRURY1-COR0001-4 TO WRK-TAM
              ADD LENGTH OF CRURY1-COR0001-4 TO WRK-POSICAO
              ADD 999                  TO WRK-IND2
              MOVE CRURY1-COR0001-4    TO WRK-B09A-SIS2(WRK-POSICAO2:
                                       LENGTH OF CRURY1-COR0001-4)
              ADD LENGTH OF CRURY1-COR0001-4 TO WRK-TAM2
              ADD LENGTH OF CRURY1-COR0001-4 TO WRK-POSICAO2
           ELSE
              ADD 999                  TO WRK-IND2
           END-IF.
      *
      *----------------------------------------------------------------*
       3121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *---------------------------------------------------------------*
       3123-FORMATA-GRUPO-PTPOLG       SECTION.
      *---------------------------------------------------------------*
      *
ED0405     MOVE 1                      TO WRK-IND8
 .         MOVE 'I'                    TO WRK-IND-PAG
 .         MOVE 'N'                    TO WRK-COORDN-FIM
 .         MOVE 'S'                    TO WRK-COORDN-EXIST
BR4.11****-MOVE ALL '0'                TO CRURY1-COD-MUN-GLEB
 .    *
ID6260*    DESPREZAR COORDENADAS GEODESICAS JA FORMATADAS
ID6260     IF  WRK-CONTINUA            EQUAL 'S'
ID6260         MOVE ZEROS              TO WRK-INDCORDN
ID6260         PERFORM UNTIL WRK-INDCORDN  GREATER WRK-DSPRZ-CORDN
ID6260                 OR WRK-DSPRZ-CORDN  EQUAL ZEROS
ID6260           PERFORM 6510-BUSCAR-CORDN
                 MOVE FNEBW3PI-S-MAX-OCORR TO WRK-MAX-OCORR
ID6260           ADD  WRK-MAX-OCORR        TO WRK-INDCORDN
ID6260         END-PERFORM
ID6260
ID6260         IF  FNEBW3PI-S-MAX-OCORR  GREATER ZEROS
ID6260         AND WRK-INDCORDN          GREATER ZEROS
ID6344             MOVE 15                    TO WRK-MAX-OCORR
ID6260             DIVIDE WRK-DSPRZ-CORDN  BY WRK-MAX-OCORR
ID6260             GIVING WRK-QTDE-CHAMADA
ID6260             REMAINDER WRK-QTDE-6510
ID6260
ID6260             ADD 1               TO WRK-QTDE-6510
ID6260         ELSE
ID6260             MOVE 1              TO WRK-QTDE-6510
ID6260         END-IF
ID6260     END-IF
ID6260
 .    *    OBTER COORDENADAS GEODESICAS
ID6260     MOVE ZEROS                  TO WRK-INDCORDN.
ID6344     MOVE 'N'                    TO WRK-COORDN-FIM.
      *
AO2307
AO2307     PERFORM 6500-BUSTAR-GRP-335
AO2307               UNTIL WRK-COORDN-FIM          EQUAL 'S'
7C2511*                 OR WRK-INDCORDN            GREATER 90
7C2511                  OR WRK-INDCORDN            GREATER 350
AO2307                  OR WRK-COORDN-EXIST        EQUAL 'N'
7C2511                  OR WRK-NUM-POLIGN          GREATER 1
      *
ID6799*   XIF  WRK-INDCORDN            GREATER 90
7C2511     IF  WRK-INDCORDN            GREATER 350
7C2511     OR  WRK-NUM-POLIGN          GREATER 1
ID6799         IF  WRK-COORDN-FIM      NOT EQUAL 'S'  OR
ID6799             WRK-AINDA-TEM       EQUAL     'S'
7C2511             MOVE ZEROS          TO WRK-NUM-POLIGN
ID6799             MOVE 'S'            TO WRK-CONTINUA
ID6799         ELSE
ID6799             MOVE 'N'            TO WRK-CONTINUA
ID6799         END-IF
ID6799     ELSE
ID6799         MOVE 'N'                TO WRK-CONTINUA
ID6799     END-IF.
EG1117
ID6260     ADD WRK-INDCORDN            TO WRK-DSPRZ-CORDN
ID6260
 .         IF WRK-COORDN-EXIST         EQUAL 'N'
BR4.11*******-MOVE ALL '*'             TO CRURY1-COD-MUN-GLEB
 .    *--> FORMATANDO IDENTIFICACAO DA GLEBA
7C2511*      XMOVE  ALL '*'            TO CRURY1-IDENTC-GLEBA
7C7511*                                   CRURY1-AREA-NAO-CULT
7C2511*      XMOVE CRURY1-COR0001-6    TO
7C2511*            WRK-B09A-SIS(WRK-POSICAO:
7C2511*                                LENGTH OF CRURY1-COR0001-6)
7C2511*      XADD LENGTH OF CRURY1-COR0001-6  TO WRK-TAM
7C2511*      XADD LENGTH OF CRURY1-COR0001-6  TO WRK-POSICAO
7C2511*      XMOVE 1                   TO WRK-IND3
7C2511*      XMOVE CRURY1-COR0001-6    TO WRK-B09A-SIS2(WRK-POSICAO2:
7C2511*                                LENGTH OF CRURY1-COR0001-6)
7C2511*      XADD LENGTH OF CRURY1-COR0001-6  TO WRK-TAM2
7C2511*      XADD LENGTH OF CRURY1-COR0001-6  TO WRK-POSICAO2

 .    *--> FORMATANDO COORDENADAS GEODESICAS
7C2511*      XMOVE ALL '*'             TO CRURY1-LONG-PONTO
7C2511*      XMOVE ALL '*'             TO CRURY1-LAT-PONTO
7C2511*      XMOVE ALL '*'             TO CRURY1-ALTT-PONTO
 .    *
 .    *    MOVIMENTAR OCORRENCIA
7C2511*      XMOVE CRURY1-COR0001-7    TO WRK-B09A-SIS(WRK-POSICAO:
7C2511*                                LENGTH OF CRURY1-COR0001-7)
7C2511*      XADD LENGTH OF CRURY1-COR0001-7 TO WRK-TAM
7C2511*      XADD LENGTH OF CRURY1-COR0001-7 TO WRK-POSICAO
 .    *
7C2511*      XMOVE CRURY1-COR0001-7    TO WRK-B09A-SIS2(WRK-POSICAO2:
7C2511*                                LENGTH OF CRURY1-COR0001-7)
7C2511*      XADD LENGTH OF CRURY1-COR0001-7 TO WRK-TAM2
7C2511*      XADD LENGTH OF CRURY1-COR0001-7 TO WRK-POSICAO2
 .    *
 .            ADD 1                    TO WRK-INDCORDN
 .         END-IF
      *
AS1202* MOVIMENTACAO DA AREA NAO CULTIVADA
ID6151     IF  WRK-AREA-NCULT          EQUAL 'S'
ID6151* MOVIMENTACAO DA AREA NAO CULTIVADA
ID6151         MOVE WRK-CIMOV-CREDT-RURAL-ANT
ID6151                                     TO WRK-CIMOV-CREDT-RURAL
ID6151         PERFORM 3140-SELECT-CRURB0B1
BR4.11********-MOVE CMUN OF CRURB0B1   TO WRK-CMUN-NUM
BR4.11********-MOVE WRK-CMUN-NUM-6     TO CRURY1-COD-MUN-GLEB
7C2511*       -MOVE ALL '*'            TO CRURY1-AREA-NAO-CULT
ID6799         MOVE 'S'                TO WRK-AREA-NCULT
ID6151     END-IF
 .    *
7C2511*   XMOVE CRURY1-COR0001-7A      TO WRK-B09A-SIS(WRK-POSICAO:
7C2511*                                LENGTH OF CRURY1-COR0001-7A)
7C2511*   XADD  LENGTH OF CRURY1-COR0001-7A
7C2511*                                TO WRK-TAM
7C2511*   XADD  LENGTH OF CRURY1-COR0001-7A
7C2511*                                TO WRK-POSICAO
      *
7C2511*   XIF CRURY1-IDENTC-GLEBA(1:1) NOT EQUAL '*'
7C2511*      XMOVE '******'            TO WRK-B09A-SIS(WRK-NEW-POS:6)
7C2511*      XMOVE '******'            TO WRK-B09A-SIS2(WRK-NEW-POS2:6)
7C2511*    END-IF
STE001***  TRATAMENTO GLEBA EXCLUIDA
STE001     PERFORM 3183-TRATAR-GLEBA-EXCLUIDA.

7C2511     IF WRK-NUM-POLIGN           LESS 2
7C2511     AND WRK-CONTINUA            NOT EQUAL 'S'
7C2511        PERFORM  6501-MONTAR-POLIGONO
7C2511     END-IF.
      *
      *----------------------------------------------------------------*
       3123-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       3124-FORMATA-GRUPO-CONSCD       SECTION.
      *---------------------------------------------------------------*
      *
EL0506*    OBTER CONSORCIO
 .    *
 .         MOVE 'I'                    TO WRK-IND-PAG
 .    *
 .         PERFORM 6600-BUSTAR-GRP-308
 .           UNTIL WRK-CONSOR-FIM      EQUAL 'S'
 .              OR WRK-INDCONSOR       GREATER 999
EL0506          OR WRK-CONSOR-EXIST    EQUAL 'N'
      *
           IF WRK-INDCONSOR            EQUAL ZEROS
              MOVE  ALL '*'            TO CRURY1-COD-PRODT-CONSCD
              MOVE CRURY1-COR0001-8    TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF CRURY1-COR0001-8)
              ADD LENGTH OF CRURY1-COR0001-8 TO WRK-TAM
              ADD LENGTH OF CRURY1-COR0001-8 TO WRK-POSICAO
              MOVE CRURY1-COR0001-8    TO WRK-B09A-SIS2(WRK-POSICAO2:
                                       LENGTH OF CRURY1-COR0001-8)
              ADD LENGTH OF CRURY1-COR0001-8 TO WRK-TAM2
              ADD LENGTH OF CRURY1-COR0001-8 TO WRK-POSICAO2
           END-IF.
      *
      *----------------------------------------------------------------*
       3124-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       3125-FORMATA-GRUPO-PROP         SECTION.
      *---------------------------------------------------------------*
      *
           MOVE 'N'                    TO  WRK-CODE100-B0L7.
      *
           PERFORM 3410-OPEN-CSR02-CRURB0L7-JN.
           PERFORM 3411-FETCH-CSR02-CRURB0L7-JN
           PERFORM UNTIL (WRK-IND1     GREATER  WRK-10)
                 OR (WRK-CODE100-B0L7  EQUAL 'S')
                   PERFORM 3425-BUSCAR-IMOVEL
                   PERFORM 3411-FETCH-CSR02-CRURB0L7-JN
           END-PERFORM
      *
ID6695     MOVE  01                    TO WRK-IND-P01
ID6695     PERFORM 3733-COR0001-PROP
ID6695         UNTIL WRK-CCPF-CNPJ(WRK-IND-P01)
ID6695                                 EQUAL ZEROS
ID6695            OR WRK-IND-P01       GREATER 30
      *
           MOVE CRURY1-COR0001-10-F    TO WRK-B09A-SIS2(WRK-POSICAO2:
                                       LENGTH OF CRURY1-COR0001-10-F)
           ADD  LENGTH OF CRURY1-COR0001-10-F TO WRK-TAM2
           ADD  LENGTH OF CRURY1-COR0001-10-F TO WRK-POSICAO2
      *
           ADD 11                      TO WRK-IND2
      *
           PERFORM 3412-FECHAR-CSR02-CRURB0L7-JN
           .
      *----------------------------------------------------------------*
       3125-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    SELECIONA NA FICHA CADASTRAL O CODIGO DO PORTE RURAL        *
      *----------------------------------------------------------------*
       3126-RECEITA-BRUTA              SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CRURB068
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
      *
EL0817*    M*VE CRURWIY1-E-CPSSOA-BNEFC
EL0817     MOVE WRK-CPSSOA
                                       TO CPSSOA
                                       OF CRURB068
      * **** 1- ATIVO ****
           MOVE 1                      TO CFICHA-CAD-RURAL
                                       OF CRURB068
           MOVE CRURW06V-CEMPTO-RURAL  TO CEMPTO-RURAL
                                       OF CRURB068
      *
           MOVE CRURW12V-CVARDE-RURAL-BACEN
                                       TO CVARDE-RURAL-BACEN
                                       OF CRURB068
      *
           MOVE CRURW12V-CCESTA-SAFRA-BACEN
                                       TO CCESTA-SAFRA-BACEN
                                       OF CRURB068
      *
           EXEC SQL
             SELECT  QEMPTO_RENDA_AGROP
                    ,VEMPTO_RENDA_AGROP
                    ,PRBATE_EMPTO_RENDA
               INTO :CRURB068.QEMPTO-RENDA-AGROP
                   ,:CRURB068.VEMPTO-RENDA-AGROP
                   ,:CRURB068.PRBATE-EMPTO-RENDA
                       :WRK-PRBATE-EMPTO-NULL
             FROM   DB2PRD.TEMPTO_RENDA_AGROP
             WHERE  CPSSOA             = :CRURB068.CPSSOA
             AND    CFICHA_CAD_RURAL   = :CRURB068.CFICHA-CAD-RURAL
             AND    CEMPTO_RURAL       = :CRURB068.CEMPTO-RURAL
             AND    CVARDE_RURAL_BACEN = :CRURB068.CVARDE-RURAL-BACEN
             AND  ((CCESTA_SAFRA_BACEN = :CRURB068.CCESTA-SAFRA-BACEN)
              OR   (IFNULL(CCESTA_SAFRA_BACEN, 0) = 0))
             ORDER BY IFNULL(CCESTA_SAFRA_BACEN, 0) DESC
             FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET  DB2-SELECT         TO TRUE
               MOVE 'TEMPTO_RENDA_AGROP'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3126-RECEITA-BRUTA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0047'             TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF WRK-PRBATE-EMPTO-NULL    LESS ZEROS
              MOVE ZEROS               TO PRBATE-EMPTO-RENDA
                                       OF CRURB068
           END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       3126-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       3127-FORMATA-GRUPO-PARCL         SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 3021-OPEN-CSR06-CRURB0MK-JN.
           PERFORM 3022-FETCH-CSR06-CRURB0MK-JN.
           MOVE NPCELA-CONTR-RURAL     OF CRURB0MK
                                       TO WRK-COM-SINAL-03
           MOVE WRK-SEM-SINAL-03       TO CRURY1-NUM-PARCL
           MOVE DVCTO-PCELA-CONTR      OF CRURB0MK
                                       TO WRK-TAM10-CHAR
           MOVE WRK-TAM10-NUM(1:2)     TO CRURY1-DT-PRVPGTO(7:2)
           MOVE WRK-TAM10-NUM(4:2)     TO CRURY1-DT-PRVPGTO(5:2)
           MOVE WRK-TAM10-NUM(7:4)     TO CRURY1-DT-PRVPGTO(1:4)
      *
           MOVE NPCELA-CONTR-RURAL     OF CRURB0MK
                                       TO WRK-COM-SINAL-03
           MOVE WRK-SEM-SINAL-03       TO WRK-NPCELA-ANT
           MOVE ZEROS                  TO WRK-VLCTO-PCELA-ACUM
      *
           PERFORM UNTIL (WRK-IND1          GREATER  WRK-999)
                      OR (WRK-CODE100-B0MK  EQUAL 'S')
                   PERFORM 3128-ACUM-GRAVA-PARCELAS
                   PERFORM 3022-FETCH-CSR06-CRURB0MK-JN
           END-PERFORM
      *
           MOVE  WRK-VLCTO-PCELA-ACUM  TO WRK-VAR17-NUMS
           MOVE  WRK-VAR17-NUMSS       TO WRK-VAR19-NUM
           MOVE  WRK-VAR19-CHAR        TO CRURY1-VLR-PRINC-PARCL
      *
           MOVE CRURY1-COR0001-12      TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF CRURY1-COR0001-12)
           ADD LENGTH OF CRURY1-COR0001-12  TO WRK-TAM
           ADD LENGTH OF CRURY1-COR0001-12  TO WRK-POSICAO
      *
           ADD 999                     TO WRK-IND2
      *
           PERFORM 3024-FECHAR-CSR06-CRURB0MK-JN
      *
           MOVE ALL '*'                TO CRURY1-GP-COR0001-PARCL.
           MOVE CRURY1-COR0001-12      TO WRK-B09A-SIS2(WRK-POSICAO2:
                                       LENGTH OF CRURY1-COR0001-12)
           ADD LENGTH OF CRURY1-COR0001-12  TO WRK-TAM2
           ADD LENGTH OF CRURY1-COR0001-12  TO WRK-POSICAO2
      *
           .
      *
      *----------------------------------------------------------------*
       3127-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       3128-ACUM-GRAVA-PARCELAS         SECTION.
      *---------------------------------------------------------------*
      *
           IF NPCELA-CONTR-RURAL       OF CRURB0MK
                                       EQUAL WRK-NPCELA-ANT
              COMPUTE WRK-VLCTO-PCELA-ACUM
                   = (WRK-VLCTO-PCELA-ACUM
                   + VLCTO-PCELA-EMPTO OF CRURB0LZ)
           ELSE
              MOVE  WRK-VLCTO-PCELA-ACUM
                                       TO WRK-VAR17-NUMS
              MOVE  WRK-VAR17-NUMSS    TO WRK-VAR19-NUM
              MOVE  WRK-VAR19-CHAR     TO CRURY1-VLR-PRINC-PARCL
      *
              MOVE CRURY1-COR0001-12   TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF CRURY1-COR0001-12)
              ADD LENGTH OF CRURY1-COR0001-12  TO WRK-TAM
              ADD LENGTH OF CRURY1-COR0001-12  TO WRK-POSICAO
              ADD 1                    TO WRK-IND1
      *
              MOVE NPCELA-CONTR-RURAL  OF CRURB0MK
                                       TO WRK-COM-SINAL-03
              MOVE WRK-SEM-SINAL-03    TO CRURY1-NUM-PARCL
              MOVE DVCTO-PCELA-CONTR   OF CRURB0MK
                                       TO WRK-TAM10-CHAR
              MOVE WRK-TAM10-NUM(1:2)  TO CRURY1-DT-PRVPGTO(7:2)
              MOVE WRK-TAM10-NUM(4:2)  TO CRURY1-DT-PRVPGTO(5:2)
              MOVE WRK-TAM10-NUM(7:4)  TO CRURY1-DT-PRVPGTO(1:4)
              MOVE NPCELA-CONTR-RURAL  OF CRURB0MK
                                       TO WRK-NPCELA-ANT
              MOVE VLCTO-PCELA-EMPTO   OF CRURB0LZ
                                       TO WRK-VLCTO-PCELA-ACUM
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3128-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    SELECIONA AREA OU QUANTIDADE DOS IMOVEIS FINANCIADOS        *
      *---------------------------------------C------------------------*
       3130-AREA-QTDE-FINAN            SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURW06V-CEMPTO-RURAL  TO CEMPTO-RURAL
                                       OF CRURB0B9
           MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
                                       TO CPSSOA-JURID-CONTR
                                       OF CRURB0L7
           MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
                                       TO CTPO-CONTR-NEGOC
                                       OF CRURB0L7
           MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
                                       TO NSEQ-CONTR-NEGOC
                                       OF CRURB0L7
      *
           EXEC SQL
             SELECT VALUE(SUM(A.MAREA_FINCD_RURAL), 0)
                   ,VALUE(SUM(A.QITEM_FINCD_RURAL), 0)
               INTO :WRK-AREA-FINAN
                   ,:WRK-QTDE-FINAN
               FROM DB2PRD.TBEM_IMOV_BENEF    AS A
                   ,DB2PRD.TORCAM_CONTR_CREDT AS B
                   ,DB2PRD.TCOMPO_CONTR_CREDT AS C
                   ,DB2PRD.TMODLD_RURAL       AS D
              WHERE A.CORCAM_CONTR_RURAL = B.CORCAM_CONTR_RURAL
                AND B.CORCAM_CONTR_RURAL = C.CORCAM_CONTR_RURAL
                AND B.CMODLD_RURAL       = D.CMODLD_RURAL
                AND D.CEMPTO_RURAL       = :CRURB0B9.CEMPTO-RURAL
                AND C.CPSSOA_JURID_CONTR = :CRURB0L7.CPSSOA-JURID-CONTR
                AND C.CTPO_CONTR_NEGOC   = :CRURB0L7.CTPO-CONTR-NEGOC
                AND C.NSEQ_CONTR_NEGOC   = :CRURB0L7.NSEQ-CONTR-NEGOC
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
               SET  DB2-SELECT         TO TRUE
               MOVE 'TBEM_IMOV_BENEF'  TO FRWKGDB2-NOME-TABELA
               MOVE '3130-AREA-QTDE-FINAN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '1047'             TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL TRATAR PROPRIETARIOS                     *
      *---------------------------------------------------------------*
       3131-TRATA-PROPRIET             SECTION.
      *---------------------------------------------------------------*
      *
           MOVE 'N'                    TO  WRK-CODE100-B0C4.
           MOVE 1                      TO  WRK-IND4.
      *
           COMPUTE WRK-IND3
                 =(WRK-IND3 + 1)
BSI1  *
STEOO5*    *OVE *RURW07V-CIMOV-CREDT-RURAL
STE005     MOVE CRURW27V-CIMOV-CREDT-RURAL
BSI1                                   TO WRK-CIMOV-CREDT-RURAL
BSI1       PERFORM 3140-SELECT-CRURB0B1
      *
ID6181     PERFORM 3145-VALIDA-CEP
      *
           PERFORM 3715-OPEN-CSR05-CRURB0C4
      *
           PERFORM 3720-FETCH-CSR05-CRURB0C4
      *
           PERFORM UNTIL (WRK-IND4     GREATER  WRK-10)
                      OR (WRK-CODE100-B0C4  EQUAL 'S')
               PERFORM 3726-MONTA-COR0001-PROPR
               PERFORM 3720-FETCH-CSR05-CRURB0C4
           END-PERFORM
      *
           PERFORM 3725-FECHAR-CSR05-CRURB0C4
      *
           MOVE CRURY1-COR0001-10-F    TO WRK-B09A-SIS2(WRK-POSICAO2:
                                       LENGTH OF CRURY1-COR0001-10-F)
           ADD  LENGTH OF CRURY1-COR0001-10-F TO WRK-TAM2
           ADD  LENGTH OF CRURY1-COR0001-10-F TO WRK-POSICAO2
      *
           .
      *
      *----------------------------------------------------------------*
       3131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR BUSCAR DADOS BENEFICIARIO.              *
      *----------------------------------------------------------------*
       3132-BUSCAR-DADOS-CLUB          SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE WRK-AREA-PSDC312C
      *
           PERFORM 3133-CONVERTER-USUARIO
      *
BPO        MOVE 2269651
                                       TO PSDCW12I-E-CEMPR
           MOVE CPSSOA                 OF CRURB0MS
                                       TO WRK-COM-SINAL-10
           MOVE WRK-SEM-SINAL-10       TO PSDCW12I-E-CD-CLUB
           MOVE PSDCWBSI-S-USUARIO-NUM TO PSDCW12I-E-CD-FUNC-BRADESCO
           MOVE 'AWB00001'             TO PSDCW12I-E-CD-TERMINAL
           MOVE CRURWIY1-E-DEPENDENCIA-OPER
                                       TO PSDCW12I-E-DEPTO
      *
           PERFORM 5300-ACESSAR-PSDC312C
      *
           MOVE PSDCW12I-S-CD-IDENT-PESSOA
                                       TO CRURY1-TP-PESSOA-EMIT
      *
           IF  PSDCW12I-S-CD-IDENT-PESSOA EQUAL 'F'
7C5.10         MOVE PSDCW12I-S-CCPF-CNPJ
.                                      TO CRURY1-CNPJ-CPF-EMIT(1:9)
.              MOVE PSDCW12I-S-CCTRL-CPF-CNPJ
.                                      TO CRURY1-CNPJ-CPF-EMIT(10:2)
7C5.10         MOVE SPACES             TO CRURY1-CNPJ-CPF-EMIT(12:3)
           ELSE
             IF  PSDCW12I-S-CD-IDENT-PESSOA EQUAL 'J'
                 MOVE PSDCW12I-S-CCPF-CNPJ(2:8)
                                       TO CRURY1-CNPJ-CPF-EMIT(1:8)
                 MOVE PSDCW12I-S-CD-FILIAL-CNPJ
                                       TO CRURY1-CNPJ-CPF-EMIT(9:4)
                 MOVE PSDCW12I-S-CCTRL-CPF-CNPJ
                                       TO CRURY1-CNPJ-CPF-EMIT(13:2)
             ELSE
                 MOVE ZEROS            TO CRURY1-CNPJ-CPF-EMIT
             END-IF
           END-IF
      *
EL0817     MOVE PSDCW12I-S-CCPF-CNPJ   TO WRK-CCPF-CNPJ-BENEF
EL0817     MOVE PSDCW12I-S-CD-FILIAL-CNPJ
EL0817                                 TO WRK-CD-FILIAL-CNPJ-BENEF
EL0817     MOVE PSDCW12I-S-CCTRL-CPF-CNPJ
EL0817                                 TO WRK-CCTRL-CPF-CNPJ-BENEF
      *
BS0217     IF CRURW03V-NDAP               EQUAL SPACES OR LOW-VALUES
7C5.10*-------MOVE-ALL-'*'                TO-CRURY1-DAP-EMIT
7C5.10        MOVE ALL '*'                TO CRURY1-CAF-BENF
BS0217     ELSE
7C5.10*-------MOVE-CRURW03V-NDAP          TO-CRURY1-DAP-EMIT
7C5.10        MOVE CRURW03V-NDAP          TO CRURY1-CAF-BENF
BS0217     END-IF.
CAT408
CAT408     MOVE CRURW03V-CTPO-BNEFC-RURAL
CAT408                                 TO CTPO-BNEFC-RURAL OF CRURB0L5
CAT408     PERFORM 3134-OBTER-CTPO-BNEFC-BACEN
CAT408     MOVE CTPO-BNEFC-BACEN       TO CRURY1-TPO-BENF
      *
           MOVE CRURY1-COR0001-2       TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF CRURY1-COR0001-2).
           ADD  LENGTH OF CRURY1-COR0001-2 TO WRK-TAM.
           ADD  LENGTH OF CRURY1-COR0001-2 TO WRK-POSICAO.
      *
           MOVE CRURY1-COR0001-2       TO WRK-B09A-SIS2(WRK-POSICAO2:
                                       LENGTH OF CRURY1-COR0001-2).
           ADD  LENGTH OF CRURY1-COR0001-2 TO WRK-TAM2.
           ADD  LENGTH OF CRURY1-COR0001-2 TO WRK-POSICAO2.
      *
           ADD 1                       TO WRK-IND1.
      *
      *----------------------------------------------------------------*
       3132-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR CONVERTER USUARIO.                      *
      *----------------------------------------------------------------*
       3133-CONVERTER-USUARIO          SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE WRK-AREA-PSDC3BSR
      *
           MOVE CRURWIY1-E-USUARIO     TO PSDCWBSI-E-USUARIO-ALF
      *
           PERFORM 5200-ACESSAR-PSDC3BSR
           .
      *
      *----------------------------------------------------------------*
       3133-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
CAT408*----------------------------------------------------------------*
CAT408 3134-OBTER-CTPO-BNEFC-BACEN     SECTION.
CAT408*----------------------------------------------------------------*
CAT408*
CAT408     EXEC SQL
CAT408       SELECT
CAT408          CTPO_BNEFC_BACEN
CAT408       INTO
CAT408         :CRURB0L5.CTPO-BNEFC-BACEN
CAT408       FROM  DB2PRD.TBNEFC_CREDT_RURAL
CAT408       WHERE CTPO_BNEFC_RURAL    =  :CRURB0L5.CTPO-BNEFC-RURAL
CAT408     END-EXEC
CAT408*
CAT408     IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
CAT408     OR (SQLWARN0                EQUAL 'W')
CAT408         SET  DB2-SELECT         TO TRUE
CAT408         MOVE 'TBNEFC_CREDT_RURAL' TO FRWKGDB2-NOME-TABELA
CAT408         MOVE '3134-OBTER-CTPO-BNEFC-BACEN'
CAT408                                 TO FRWKGHEA-IDEN-PARAGRAFO
CAT408         MOVE '3134'             TO FRWKGDB2-LOCAL
CAT408         PERFORM 9000-ERRO-DB2
CAT408     END-IF.
CAT408
CAT408     IF  SQLCODE                 EQUAL +100
CAT408         MOVE ALL '*'            TO CTPO-BNEFC-BACEN OF CRURB0L5
CAT408     END-IF.
CAT408*
CAT408*----------------------------------------------------------------*
CAT408 3134-99-FIM.                    EXIT.
CAT408*----------------------------------------------------------------*
CAT408*
BSI1  *----------------------------------------------------------------*
 ""    3140-SELECT-CRURB0B1            SECTION.
 ""   *----------------------------------------------------------------*
 ""   *
 ""        INITIALIZE CRURB0B1
 ""   *
 ""        MOVE WRK-CIMOV-CREDT-RURAL  TO CIMOV-CREDT-RURAL OF CRURB0B1
 ""   *
 ""        EXEC SQL
 ""             SELECT  CNIRF_IMOV_RURAL
 ""                    ,CCCIR
 ""                    ,CREG_AMBTL_RURAL
 ""                    ,CMUN
ID6181                 ,CCEP
ID6181                 ,CCEP_COMPL
BR0621                 ,COUTGA_AGUA_IMOV
BR0621                 ,PPRESV_AMBTL_PPRIE
BR0621                 ,DVALDD_OUTGA_AGUA
MJ1224                 ,CMATR_IMOV_RURAL
 ""               INTO :CRURB0B1.CNIRF-IMOV-RURAL
 ""                         :WRK-CNIRF-IMOV-RURAL-NULL
 ""                   ,:CRURB0B1.CCCIR
 ""                         :WRK-CCCIR-NULL
 ""                   ,:CRURB0B1.CREG-AMBTL-RURAL
 ""                         :WRK-CREG-AMBTL-RURAL-NULL
 ""                   ,:CRURB0B1.CMUN
ID6181                ,:CRURB0B1.CCEP
ID6181                ,:CRURB0B1.CCEP-COMPL
BR0621                ,:CRURB0B1.COUTGA-AGUA-IMOV
BR0621                      :WRK-COUTGA-AGUA-IMOV-NULL
BR0621                ,:CRURB0B1.PPRESV-AMBTL-PPRIE
BR0621                      :WRK-PPRESV-AMBTL-PPRIE-NULL
BR0621                ,:CRURB0B1.DVALDD-OUTGA-AGUA
BR0621                      :WRK-DVALDD-OUTGA-AGUA-NULL
MJ1224                ,:CRURB0B1.CMATR-IMOV-RURAL
MJ1224                      :WRK-CMATR-IMOV-RURAL-NULL
 ""               FROM  DB2PRD.TIMOV_CREDT_RURAL
 ""              WHERE  CIMOV_CREDT_RURAL = :CRURB0B1.CIMOV-CREDT-RURAL
 ""        END-EXEC
 ""   *
 ""        IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
 ""        OR (SQLWARN0                EQUAL 'W')
 ""            SET  DB2-SELECT         TO TRUE
 ""            MOVE 'TIMOV_CREDT_RURAL'
 ""                                    TO FRWKGDB2-NOME-TABELA
 ""            MOVE '3140-SELECT-CRURB0B1'
 ""                                    TO FRWKGHEA-IDEN-PARAGRAFO
 ""            MOVE '3140'             TO FRWKGDB2-LOCAL
 ""            PERFORM 9000-ERRO-DB2
 ""        END-IF.
 ""
 ""        IF  SQLCODE                 EQUAL +100
 ""            MOVE ALL '*'            TO CNIRF-IMOV-RURAL OF CRURB0B1
 ""                                       CCCIR            OF CRURB0B1
 ""                                       CREG-AMBTL-RURAL OF CRURB0B1
BR0621                                  COUTGA-AGUA-IMOV   OF CRURB0B1
BR0621                                  DVALDD-OUTGA-AGUA  OF CRURB0B1
BR0621         MOVE 999,99           TO PPRESV-AMBTL-PPRIE OF CRURB0B1
 ""        ELSE
MJ1224         IF  WRK-CMATR-IMOV-RURAL-NULL    LESS ZEROS
MJ1224             MOVE SPACES       TO CMATR-IMOV-RURAL OF CRURB0B1
MJ1224         END-IF
MJ1224*
 ""            IF  WRK-CNIRF-IMOV-RURAL-NULL    LESS  ZEROS OR
 ""                CNIRF-IMOV-RURAL OF CRURB0B1 EQUAL SPACES
 ""                MOVE ALL '*'        TO CNIRF-IMOV-RURAL OF CRURB0B1
 ""   **********LSE
 ""   **************XEC CICS
 ""   ********************F DEEDIT
 ""   *******************IELD(CNIRF-IMOV-RURAL OF CRURB0B1)
 ""   *******************ENGTH(LENGTH OF CNIRF-IMOV-RURAL OF CRURB0B1)
 ""   **************ND-EXEC
 ""            END-IF
 ""   *
MJ1224         PERFORM 3141-VER-NIRF-PARAM
 ""   *
 ""            IF  WRK-CCCIR-NULL               LESS  ZEROS  OR
 ""                CCCIR            OF CRURB0B1 EQUAL SPACES
 ""                MOVE ALL '*'        TO CCCIR            OF CRURB0B1
 ""            ELSE
 ""                EXEC CICS
 ""                     BIF DEEDIT
 ""                     FIELD(CCCIR OF CRURB0B1)
 ""                     LENGTH(LENGTH OF CCCIR OF CRURB0B1)
 ""                END-EXEC
 ""            END-IF
 ""   *
 ""            IF  WRK-CREG-AMBTL-RURAL-NULL    LESS  ZEROS  OR
 ""                CREG-AMBTL-RURAL OF CRURB0B1 EQUAL SPACES
 ""                MOVE ALL '*'        TO CREG-AMBTL-RURAL OF CRURB0B1
 ""            END-IF
 ""   *
BR0621         IF  WRK-COUTGA-AGUA-IMOV-NULL    LESS  ZEROS  OR
BR0621             COUTGA-AGUA-IMOV OF CRURB0B1 EQUAL SPACES
BR0621             MOVE ALL '*'        TO COUTGA-AGUA-IMOV OF CRURB0B1
BR0621         END-IF
      *
BR0621         IF  WRK-DVALDD-OUTGA-AGUA-NULL    LESS  ZEROS  OR
BR0621             DVALDD-OUTGA-AGUA OF CRURB0B1 EQUAL SPACES
BR0621             MOVE ALL '*'        TO DVALDD-OUTGA-AGUA OF CRURB0B1
BR0621         END-IF
      *
BR0621         IF  WRK-PPRESV-AMBTL-PPRIE-NULL    LESS  ZEROS  OR
BR0621             PPRESV-AMBTL-PPRIE OF CRURB0B1 EQUAL ZEROS
BR0621             MOVE 999,99        TO PPRESV-AMBTL-PPRIE OF CRURB0B1
BR0621         END-IF
 ""   *
 ""        END-IF.
 ""        .
 ""   *
 ""   *----------------------------------------------------------------*
 ""    3140-99-FIM.                    EXIT.
BSI1  *----------------------------------------------------------------*

MJ1224*----------------------------------------------------------------*
  ""  *    VERIFICA SE EXISTE PARAMETRO DE TROCA DE NIRF PARA O        *
  ""  *    CONTRATO ATUAL                                              *
  ""  *----------------------------------------------------------------*
  ""   3141-VER-NIRF-PARAM             SECTION.
  ""  *----------------------------------------------------------------*
  ""  *
  ""       INITIALIZE CRURWNIC-BLOCO-ENTRADA
  ""  *
  ""       MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
  ""                                   TO CRURWNIC-E-NSEQ-CONTR-NEGOC
  ""  *
  ""       EXEC CICS LINK
  ""            PROGRAM  (WRK-CRUR3NIR)
  ""            COMMAREA (WRK-AREA-CRUR3NIR)
  ""            LENGTH   (LENGTH OF WRK-AREA-CRUR3NIR)
  ""            NOHANDLE
  ""       END-EXEC
  ""  *
  ""       IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
  ""           MOVE '3141'             TO CRURWIY1-COD-ERRO
  ""                                   OF DFHCOMMAREA
  ""           MOVE '3141-VER-NIRF-PARAM'
  ""                                   TO FRWKGHEA-IDEN-PARAGRAFO
  ""           PERFORM 9400-ERRO-CICS
  ""       END-IF
  ""  *
  ""       EVALUATE CRURW00W-COD-RETORNO
  ""                                   OF WRK-AREA-CRUR3NIR
  ""           WHEN 00
  ""                CONTINUE
  ""           WHEN OTHER
  ""                GO TO 3141-99-FIM
  ""       END-EVALUATE.
  ""
  ""       PERFORM VARYING IND-NIRF FROM 1 BY 1
  ""                 UNTIL IND-NIRF GREATER CRURWNIC-S-QTD-OCORR
  ""                    OR IND-NIRF GREATER 20
  ""          IF  CRURWNIC-S-CMATR-IMOV-RURAL(IND-NIRF) EQUAL
  ""                                        CMATR-IMOV-RURAL OF CRURB0B1
  ""              STRING '0'
  ""                     CRURWNIC-S-CNIRF-IMOV-RURAL(IND-NIRF)
  ""                     DELIMITED BY SIZE INTO CNIRF-IMOV-RURAL
  ""                                         OF CRURB0B1
  ""          END-IF
  ""       END-PERFORM
  ""       .
  ""  *
  ""  *----------------------------------------------------------------*
  ""   3141-99-FIM.                    EXIT.
  ""  *----------------------------------------------------------------*
MJ1224
ID6181*----------------------------------------------------------------*
  ""  *    OBTEM AS INFORMACOES DE UF E MUNICIPIO                      *
  ""  *----------------------------------------------------------------*
  ""   3145-VALIDA-CEP                 SECTION.
  ""  *----------------------------------------------------------------*
  ""       INITIALIZE                  CEPNW01I-DADOS-ENTRADA
  ""                                   OF WRK-AREA-CEPN301R
  ""                                   CEPNW01I-INDICADOR-PAGINACAO
  ""                                   OF WRK-AREA-CEPN301R
  ""                                   CEPNW01I-CHAVE-INI
  ""                                   OF WRK-AREA-CEPN301R
  ""                                   CEPNW01I-CHAVE-FIM
  ""                                   OF WRK-AREA-CEPN301R
  ""                                   CEPNW00W-BLOCO-RETORNO
  ""                                   OF WRK-AREA-CEPN301R
  ""                                   REPLACING ALPHANUMERIC BY SPACES
  ""                                                  NUMERIC BY ZEROS
  ""  *
  ""  ******FORMATA BOOK CEPNW01I - BLOCO DE ENTRADA
  ""  *
  ""       MOVE 1                      TO CEPNW01I-E-MAX-OCORR
  ""                                   OF WRK-AREA-CEPN301R
  ""  *
  ""       MOVE CCEP       OF CRURB0B1 TO WRK-CEP-S5
  ""       MOVE WRK-CEP-N5             TO CEPNW01I-E-NUM-CEP
  ""                                   OF WRK-AREA-CEPN301R
  ""  *
  ""       MOVE CCEP-COMPL OF CRURB0B1 TO WRK-COMPL-CEP-S3
  ""       MOVE WRK-COMPL-CEP-X3       TO CEPNW01I-E-COMPL-CEP
  ""                                   OF WRK-AREA-CEPN301R
  ""  *
  ""       EXEC CICS LINK
  ""            PROGRAM  (WRK-CEPN301R)
  ""            COMMAREA (WRK-AREA-CEPN301R)
  ""            LENGTH   (LENGTH       OF WRK-AREA-CEPN301R)
  ""            NOHANDLE
  ""       END-EXEC
  ""  *
  ""       IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
  ""           MOVE '0210'             TO CRURWIY1-COD-ERRO
  ""           MOVE '3145-VALIDA-CEP'  TO FRWKGHEA-IDEN-PARAGRAFO
  ""           PERFORM 9400-ERRO-CICS
  ""       END-IF
  ""  *
  ""       EVALUATE CEPNW00W-COD-RETORNO
  ""                                   OF WRK-AREA-CEPN301R
  ""           WHEN ZEROS
  ""           WHEN 01
  ""                CONTINUE
  ""           WHEN OTHER
  ""                MOVE '0220'        TO CRURWIY1-COD-ERRO
  ""                                   OF DFHCOMMAREA
  ""                MOVE CEPNW00W-COD-MENSAGEM
  ""                                   OF WRK-AREA-CEPN301R
  ""                                   TO FRWKGMOD-COD-MENSAGEM
  ""                MOVE WRK-CEPN301R  TO FRWKGMOD-NOME-MODULO
  ""                MOVE CEPNW00W-BLOCO-RETORNO
  ""                                   OF WRK-AREA-CEPN301R
  ""                                   TO FRWKGMOD-BLOCO-RETORNO
  ""                MOVE '3145-VALIDA-CEP'
  ""                                   TO FRWKGHEA-IDEN-PARAGRAFO
  ""                PERFORM 9300-ERRO-MODULO
  ""       END-EVALUATE
  ""
  ""       IF (CEPNW01I-S-DADOS-N-ENCONTRADOS EQUAL 'S') OR
  ""          (CEPNW01I-S-UF(1) EQUAL SPACES OR LOW-VALUES)
  ""           MOVE '0230'             TO CRURWIY1-COD-ERRO
  ""                                   OF DFHCOMMAREA
  ""                                      FRWKGMOD-COD-ERRO
  ""           MOVE WRK-CEPN301R       TO FRWKGMOD-NOME-MODULO
  ""           MOVE 'CRUR2109'         TO CRURWIY1-COD-MENSAGEM
  ""                                   OF DFHCOMMAREA
  ""                                      FRWKGMOD-COD-MENSAGEM
  ""           MOVE 16                 TO FRWKGMOD-COD-RETORNO
  ""           MOVE '3425-BUSCAR-IMOVEL'
  ""                                   TO FRWKGHEA-IDEN-PARAGRAFO
  ""           PERFORM 9300-ERRO-MODULO
  ""       END-IF
  ""       .
  ""  *
  ""  *----------------------------------------------------------------*
  ""   3145-99-FIM.                    EXIT.
ID6181*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3150-FORMATA-DTMOVTO            SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
              SET :WRK-TIMESTAMP    = CURRENT_TIMESTAMP
           END-EXEC
      *
           MOVE WRK-TIMESTAMP(1:4)     TO  WRK-DATADIA-AMD-X(1:4)
           MOVE WRK-TIMESTAMP(6:2)     TO  WRK-DATADIA-AMD-X(5:2)
           MOVE WRK-TIMESTAMP(9:2)     TO  WRK-DATADIA-AMD-X(7:2)
      *
           MOVE WRK-DATADIA-AMD-N      TO  CRURY1-DTMOVTO
BR0521
CAT408     MOVE CRURY1-COR0001-22      TO WRK-B09A-SIS(WRK-POSICAO:
CAT408                                 LENGTH OF CRURY1-COR0001-22).
CAT408     ADD  LENGTH OF CRURY1-COR0001-22 TO WRK-TAM.
CAT408     ADD  LENGTH OF CRURY1-COR0001-22 TO WRK-POSICAO.
      *
      *----------------------------------------------------------------*
       3150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
AS0615*----------------------------------------------------------------*
.     *    ROTINA RESPONSAVEL POR FORMATAR O SUBPROGRAMA               *
.     *----------------------------------------------------------------*
.      3160-FORMATAR-SUBPROGRAMA       SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE '****'                 TO CRURY1-CODSUB-PROG
.     *
.     * *** LINHA DE CREDITO 01 - PRONAF CUSTEIO
PRONAF     IF WRK-CLIN-SIST-BACEN      EQUAL 1
PRONAF        MOVE '0001'              TO CRURY1-CODSUB-PROG
PRONAF     END-IF
PRONAF

IN-BRQ* *** CODIGO BACEN DA FINALIDADE - 4 INDUSTRIALIZACAO
IN-BRQ* *** CODIGO BACEN DA LINHA DE CREDITO - 0001 PRONAF
IN-BRQ     IF CRURY1-COD-EMPNMNT(2:1)  EQUAL '4' AND
IN-BRQ        WRK-CLIN-SIST-BACEN      EQUAL  1
IN-BRQ        MOVE '0053'              TO CRURY1-CODSUB-PROG
IN-BRQ     END-IF
.
.     * *** LINHA DE CREDITO 70 - FUNCAFE
.          IF WRK-CLIN-SIST-BACEN      EQUAL 70
AS0302         MOVE CRURW00V-CSUB-PROG-VINC
.                                      TO WRK-VAR5-NUM
AS0302         MOVE WRK-VAR4-CHAR-5    TO CRURY1-CODSUB-PROG
.          END-IF
.     *
MJ1221* *** LINHA DE CREDITO 156 - ABC - PROGRAMA PARA REDUï¿½--ï¿½--O D
MJ1221* *** EMISSï¿½--O DE GASES DE EFEITO ESTUFA NA AGROPECUï¿½--RIA
MJ1221     IF WRK-CLIN-SIST-BACEN      EQUAL 156
MJ1221        EVALUATE CRURY1-COD-EMPNMNT
MJ1221            WHEN '13105760989408'
MJ1221                 MOVE '0032'     TO CRURY1-CODSUB-PROG
MJ1221            WHEN '23906240217408'
MJ1221            WHEN '13906240217408'
MJ1221            WHEN '23900360000408'
MJ1221            WHEN '13900360000408'
MJ1221                 MOVE '0034'     TO CRURY1-CODSUB-PROG
MJ1221            WHEN '13123620152002'
MJ1221                 MOVE '0036'     TO CRURY1-CODSUB-PROG
MJ1221            WHEN '13123140000002'
MJ1221                 MOVE '0039'     TO CRURY1-CODSUB-PROG
MJ1221            WHEN '13121420000002'
MJ1221            WHEN '13121420000041'
MJ1221            WHEN '13121420000051'
MJ1221            WHEN '13120160000002'
MJ1221            WHEN '13121420505041'
MJ1221            WHEN '13121420505051'
MJ1221                 MOVE '0062'     TO CRURY1-CODSUB-PROG
MJ1221            WHEN '23707695108408'
MJ1221                 MOVE '0038'     TO CRURY1-CODSUB-PROG
MJ1221            WHEN '13123620452408'
MJ1221                 MOVE '0037'     TO CRURY1-CODSUB-PROG
MJ1221            WHEN '13906750000408'
MJ1221                 MOVE '0033'     TO CRURY1-CODSUB-PROG
MJ1221            WHEN OTHER
MJ1221                 CONTINUE
MJ1221        END-EVALUATE
MJ1221     END-IF.
MJ1221*
MJ1221* *** LINHA DE CREDITO 163 - PCA - PROGRAMA PARA CONSTRUCAO E
MJ1221* *** AMPLIACAO DE ARMAZENS
MJ1221     IF WRK-CLIN-SIST-BACEN      EQUAL 163
MJ1221        EVALUATE CRURY1-COD-EMPNMNT
MJ1221            WHEN '23703180110408'
MJ1221            WHEN '13703180110408'
MJ1221                 MOVE '0041'     TO CRURY1-CODSUB-PROG
MJ1221            WHEN OTHER
MJ1221                 CONTINUE
MJ1221        END-EVALUATE
MJ1221     END-IF.
MJ1221*
7C5.10*----MOVE-CRURY1-COR0001-13      TO-WRK-B09A-SIS(WRK-POSICAO:
.     *                                LENGTH-OF-CRURY1-COR0001-13).
.     *    ADD--LENGTH-OF CRURY1-COR0001-13-TO-WRK-TAM.
7C5.10*----ADD--LENGTH-OF CRURY1-COR0001-13-TO-WRK-POSICAO
.          .
.
.     *----------------------------------------------------------------*
.      3160-99-FIM.                    EXIT.
AS0615*----------------------------------------------------------------*
      *
BR4.11*---------------------------------------------------------------*
BR4.11*3170-FORMATA-GRUPO-LIBRC  SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*
BR4.11*   -MOVE ALL '*'       TO CRURY1-DLIBRC-RECUR
BR4.11*   -MOVE ALL '*'       TO CRURY1-VLR-LIBRC-REC
BR4.11*
BR4.11*   -MOVE CRURY1-COR0001-14 TO
BR4.11*          WRK-B09A-SIS(WRK-POSICAO:
BR4.11*                               LENGTH OF CRURY1-COR0001-14)
BR4.11*   -ADD LENGTH OF CRURY1-COR0001-14 TO WRK-TAM
BR4.11*   -ADD LENGTH OF CRURY1-COR0001-14 TO WRK-POSICAO
BR4.11*    .
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3170-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
      *
AS1202*---------------------------------------------------------------*
.      3180-FORMATA-PLANTIO-COLHEITA SECTION.
.     *---------------------------------------------------------------*
AS1202
BSI1       IF WRK-DT-PLANTIO-INI  EQUAL SPACES
.                              OR HIGH-VALUES
.                              OR LOW-VALUES
.              MOVE ALL '*'   TO CRURY1-DTINI-PLANT
.          ELSE
.              MOVE WRK-DT-PLANTIO-INI
.                             TO CRURY1-DTINI-PLANT
.          END-IF
.     *
.          IF WRK-DT-PLANTIO-FIM  EQUAL SPACES
.                              OR HIGH-VALUES
.                              OR LOW-VALUES
.              MOVE ALL '*'   TO CRURY1-DTFIM-PLANT
.          ELSE
.              MOVE WRK-DT-PLANTIO-FIM
.                             TO CRURY1-DTFIM-PLANT
.          END-IF
.     *
.          IF WRK-DT-COLHEITA-INI  EQUAL SPACES
.                               OR HIGH-VALUES
.                               OR LOW-VALUES
.              MOVE ALL '*'   TO CRURY1-DTINI-COLHT
.          ELSE
.              MOVE WRK-DT-COLHEITA-INI
.                             TO CRURY1-DTINI-COLHT
.          END-IF
.          IF WRK-DT-COLHEITA-FIM  EQUAL SPACES
.                               OR HIGH-VALUES
.                               OR LOW-VALUES
.              MOVE ALL '*'   TO CRURY1-DTFIM-COLHT
.          ELSE
.              MOVE WRK-DT-COLHEITA-FIM
.                             TO CRURY1-DTFIM-COLHT
BSI1       END-IF
      *
BR4.11****-MOVE ALL '*'       TO CRURY1-VLR-PROD-OBTD
AS1202*
.          MOVE CRURY1-COR0001-15 TO
.                WRK-B09A-SIS(WRK-POSICAO:
.                                     LENGTH OF CRURY1-COR0001-15)
.          ADD LENGTH OF CRURY1-COR0001-15 TO WRK-TAM
.          ADD LENGTH OF CRURY1-COR0001-15 TO WRK-POSICAO
      *
.          .
.     *----------------------------------------------------------------*
.      3180-99-FIM.                    EXIT.
AS1202*----------------------------------------------------------------*
STE001*---------------------------------------------------------------*
.      3183-TRATAR-GLEBA-EXCLUIDA    SECTION.
.     *---------------------------------------------------------------*
.
7C2511*    XMOVE CRURY1-COR0001-7B TO
7C2511*          WRK-B09A-SIS(WRK-POSICAO:
7C2511*                               LENGTH OF CRURY1-COR0001-7B)
7C2511*   XADD LENGTH OF CRURY1-COR0001-7B   TO WRK-TAM
7C2511*   XADD LENGTH OF CRURY1-COR0001-7B   TO WRK-POSICAO
STE001*
           PERFORM 3184-OBTER-AREA-NCULT

           PERFORM UNTIL IND-GLEBA-EXCL EQUAL IND-COORD-GLEBA
                 OR  WRK-FIM-GLEBA-EXCL EQUAL 'S'
               ADD 1                   TO IND-GLEBA-EXCL
               PERFORM 3185-FETCH-CSR01-CRURB0V4

            IF WRK-SEM-GLB-EXCL        EQUAL 'S' AND
               IND-PRIMEIRA            EQUAL ZEROS
7C2511*       XMOVE ALL '*'            TO CRURY1-LAT-PONTO-EXCL
7C2511*       XMOVE ALL '*'            TO CRURY1-LONG-PONTO-EXCL
7C2511*       XMOVE ALL '*'            TO CRURY1-ALTT-PONTO-EXCL
               MOVE 'S'                TO WRK-FIM-GLEBA-EXCL
            END-IF
      *
            IF WRK-SEM-GLB-EXCL        EQUAL 'N' AND
               IND-PRIMEIRA            GREATER ZEROS
      *
7C2511*       XMOVE MLATTD-CORND-IMOV  OF CRURB0V4
7C2511*                                TO WRK-LAT-PONTO-E-SAIDA
7C2511*       XMOVE WRK-LAT-SAIDA      TO CRURY1-LAT-PONTO-EXCL
7C2511*
7C2511*       XMOVE MLOGTD-CORND-IMOV  OF CRURB0V4
7C2511*                                TO WRK-LONG-PONTO-E-SAIDA
7C2511*       XMOVE WRK-LONG-SAIDA     TO CRURY1-LONG-PONTO-EXCL
7C2511*
7C2511*       XMOVE MALTTD-CORND-IMOV  OF CRURB0V4
7C2511*                                TO WRK-ALT-PONTO-E-SAIDA
7C2511*       XMOVE WRK-ALT-SAIDA      TO CRURY1-ALTT-PONTO-EXCL

              ADD 1                    TO WRK-QTD-NCULTVD
7C2511        MOVE MLATTD-CORND-IMOV   OF CRURB0V4
7C2511                                 TO WRK-LAT-PONTO-E-SAIDA
              IF MLATTD-CORND-IMOV     OF CRURB0V4
                                       LESS ZEROS
7C2511           MOVE WRK-LAT-SAIDA-9V7
7C2511                                 TO WRK-MASK-COORD
7C2511           MOVE WRK-MASK-COORD-X TO WRK-NCULTVD-LAT
7C2511                                   (WRK-QTD-NCULTVD)
7C2511           MOVE '-'              TO WRK-NCULTVD-LAT
7C2511                                   (WRK-QTD-NCULTVD)(1:1)
              ELSE
7C2511           MOVE WRK-LAT-SAIDA-9V7
7C2511                                 TO WRK-MASK-COORD
7C2511           MOVE WRK-MASK-COORD-X TO WRK-NCULTVD-LAT
7C2511                                   (WRK-QTD-NCULTVD)
7C2511        END-IF
7C2511        INSPECT WRK-NCULTVD-LAT (WRK-QTD-NCULTVD)
7C2511                                 REPLACING ALL ','
7C2511                                 BY '.'

7C2511        MOVE MLOGTD-CORND-IMOV   OF CRURB0V4
7C2511                                 TO WRK-LONG-PONTO-E-SAIDA
7C2511        IF MLOGTD-CORND-IMOV     OF CRURB0V4
7C2511                                 LESS ZEROS
7C2511           MOVE WRK-LONG-SAIDA-9V7
7C2511                                 TO WRK-MASK-COORD
7C2511           MOVE WRK-MASK-COORD-X TO WRK-NCULTVD-LON
7C2511                                   (WRK-QTD-NCULTVD)
7C2511           MOVE '-'              TO WRK-NCULTVD-LON
7C2511                                   (WRK-QTD-NCULTVD)(1:1)
7C2511        ELSE
7C2511           MOVE WRK-LONG-SAIDA-9V7
7C2511                                 TO WRK-MASK-COORD
7C2511           MOVE WRK-MASK-COORD-X TO WRK-NCULTVD-LON
7C2511                                   (WRK-QTD-NCULTVD)
              END-IF
            END-IF

7C2511      INSPECT WRK-NCULTVD-LON (WRK-QTD-NCULTVD)
7C2511                                 REPLACING ALL ','
7C2511                                 BY '.'

7C2511*    XMOVE CRURY1-COR0001-7BB TO
7C2511*           WRK-B09A-SIS(WRK-POSICAO:
7C2511*                                LENGTH OF CRURY1-COR0001-7BB)
7C2511*    XADD LENGTH OF CRURY1-COR0001-7BB   TO WRK-TAM
7C2511*    XADD LENGTH OF CRURY1-COR0001-7BB   TO WRK-POSICAO

           END-PERFORM.

7C2511*   XIF IND-GLEBA-EXCL           EQUAL ZEROS AND
7C2511*       IND-COORD-GLEBA          EQUAL ZEROS AND
7C2511*       WRK-FIM-GLEBA-EXCL       EQUAL 'N'
7C2511*       XMOVE ALL '*'            TO CRURY1-LAT-PONTO-EXCL
7C2511*       XMOVE ALL '*'            TO CRURY1-LONG-PONTO-EXCL
7C2511*       XMOVE ALL '*'            TO CRURY1-ALTT-PONTO-EXCL
7C2511*       XMOVE CRURY1-COR0001-7BB TO
7C2511*           WRK-B09A-SIS(WRK-POSICAO:
7C2511*                                LENGTH OF CRURY1-COR0001-7BB)
7C2511*       XADD LENGTH OF CRURY1-COR0001-7BB   TO WRK-TAM
7C2511*       XADD LENGTH OF CRURY1-COR0001-7BB   TO WRK-POSICAO
7C2511*    END-IF

           PERFORM 3186-CLOSE-CSR-CRURB0V4
      *
           MOVE 'N'                    TO WRK-SEM-GLB-EXCL.
           MOVE 'N'                    TO WRK-FIM-GLEBA-EXCL
           MOVE ZEROS                  TO IND-PRIMEIRA
                                          IND-GLEBA-EXCL.
.     *----------------------------------------------------------------*
.      3183-99-FIM.                    EXIT.
STE001*----------------------------------------------------------------*
STE003*----------------------------------------------------------------*
.      3184-OBTER-AREA-NCULT           SECTION.
.     *----------------------------------------------------------------*
.          MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
.                                      TO CPSSOA-JURID-CONTR
.                                      OF CRURB0L7.
.          MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
.                                      TO CTPO-CONTR-NEGOC
.                                      OF CRURB0L7.
.          MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
.                                      TO NSEQ-CONTR-NEGOC
.                                      OF CRURB0L7.
.     *
.          EXEC SQL
.              SELECT B.CORCAM_CONTR_RURAL
.               INTO :CRURB0MB.CORCAM-CONTR-RURAL
.              FROM   DB2PRD.TCOMPO_CONTR_CREDT A
.                    ,DB2PRD.TORCAM_CONTR_CREDT B
.              WHERE A.CORCAM_CONTR_RURAL = B.CORCAM_CONTR_RURAL
.              AND   A.CPSSOA_JURID_CONTR = :CRURB0L7.CPSSOA-JURID-CONTR
.              AND   A.CTPO_CONTR_NEGOC   = :CRURB0L7.CTPO-CONTR-NEGOC
.              AND   A.NSEQ_CONTR_NEGOC   = :CRURB0L7.NSEQ-CONTR-NEGOC
.              ORDER BY B.CORCAM_CONTR_RURAL ASC
.              FETCH  FIRST  1  ROWS  ONLY
.          END-EXEC.
.     *
.          IF (SQLCODE             NOT EQUAL ZEROS AND +100)
.          OR (SQLWARN0                EQUAL 'W')
.              SET  DB2-SELECT         TO TRUE
.              MOVE 'JN - MB / 0L7'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '3184-OBTER-AREA-NCULT - JN'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              MOVE '3184'             TO FRWKGDB2-LOCAL
.              PERFORM 9000-ERRO-DB2
.          END-IF.
.     *
.          IF  SQLCODE                 EQUAL +100
.              MOVE ZEROS              TO CORCAM-CONTR-RURAL
.                                      OF CRURB0MB
.              GO                      TO 3184-99-FIM
.          END-IF.
.     *
.          MOVE CORCAM-CONTR-RURAL     OF CRURB0MB
.                                      TO CORCAM-CONTR-RURAL
.                                      OF CRURB0V4.
STE003***  *OVE CRURW07V-CIMOV-CREDT-RURAL
STE003     MOVE WRK-CIMOV-CREDT-RURAL-ANT
.                                      TO CIMOV-CREDT-RURAL
.                                      OF CRURB0V4.

STE003     PERFORM 3187-TRATAR-GLEBA-ANT

STEFAB     IF  NSEQ-GLEBA-IMOV         OF CRURB0V4
.                                      NOT NUMERIC
.              MOVE ZEROS              TO NSEQ-GLEBA-IMOV
.                                      OF CRURB0V4
STEFAB     END-IF

STEFAB     IF  CORCAM-CONTR-RURAL      OF CRURB0V4
.                                      NOT NUMERIC
.              MOVE ZEROS              TO CORCAM-CONTR-RURAL
.                                      OF CRURB0V4
STEFAB     END-IF

STEFAB     IF  CIMOV-CREDT-RURAL       OF CRURB0V4
.                                      NOT NUMERIC
.              MOVE ZEROS              TO CIMOV-CREDT-RURAL
.                                      OF CRURB0V4
STEFAB     END-IF

.          EXEC SQL
.               OPEN CSR01-CRURB0V4
           END-EXEC
.
.          IF (SQLCODE                 NOT EQUAL ZEROS) OR
.             (SQLWARN0                EQUAL 'W')
.              SET DB2-OPEN            TO TRUE
.              MOVE '3184'             TO CRURWIY1-COD-ERRO
.                                      OF DFHCOMMAREA
.              MOVE 'OPEN - CSR01-CRURB0V4'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '3184 - OPEN - CSR'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9000-ERRO-DB2
.          END-IF.
.     *----------------------------------------------------------------*
STE003 3184-99-FIM.                    EXIT.
STE003*----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.     *    ROTINA RESPONSAVEL POR FETCH CSR06-CRURB0MK-JN
.     *----------------------------------------------------------------*
.      3185-FETCH-CSR01-CRURB0V4       SECTION.
.     *----------------------------------------------------------------*
.          EXEC SQL
.            FETCH CSR01-CRURB0V4 INTO
.                  :CRURB0V4.MLATTD-CORND-IMOV
.                 ,:CRURB0V4.MLOGTD-CORND-IMOV
.                 ,:CRURB0V4.MALTTD-CORND-IMOV
.          END-EXEC.
.     *
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0                EQUAL 'W')
.              SET DB2-FETCH           TO TRUE
.              MOVE '3185'             TO FRWKGDB2-LOCAL
.              MOVE 'CRURB0V4'         TO FRWKGDB2-NOME-TABELA
.              MOVE '3185-FETCH-CSR01-CRURB0V4'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9000-ERRO-DB2
.          END-IF
.     *
.          IF (SQLCODE                 EQUAL +100)
.             MOVE 'N'                 TO WRK-TEM-NCULT
              MOVE 'S'                 TO WRK-FIM-GLEBA-EXCL
.          ELSE
.             ADD 1                    TO IND-PRIMEIRA
.          END-IF.
.     *
.          IF WRK-TEM-NCULT            EQUAL 'N' AND
.             IND-QTD-GLEBA            GREATER ZEROS
.             MOVE 'S'                 TO WRK-FIM-GLB-EXCL
.          END-IF.
      *
.          IF WRK-TEM-NCULT            EQUAL 'N' AND
.             IND-PRIMEIRA             EQUAL ZEROS
.             MOVE 'S'                 TO WRK-SEM-GLB-EXCL
.          END-IF.
.     *----------------------------------------------------------------*
.      3185-99-FIM.                    EXIT.
STE003*----------------------------------------------------------------*
      *                                                                *
STE003*----------------------------------------------------------------*
.      3186-CLOSE-CSR-CRURB0V4         SECTION.
.     *----------------------------------------------------------------*
.          EXEC SQL
.               CLOSE CSR01-CRURB0V4
           END-EXEC
.
.          IF (SQLCODE                 NOT EQUAL ZEROS) OR
.             (SQLWARN0                EQUAL 'W')
.              SET DB2-CLOSE           TO TRUE
.              MOVE '3184'             TO CRURWIY1-COD-ERRO
.                                      OF DFHCOMMAREA
.              MOVE 'CLOSE - CSR01-CRURB0V4'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '3186 - CLOSE - CSR'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9000-ERRO-DB2
.          END-IF.
.     *----------------------------------------------------------------*
ATE003 3186-99-FIM.                    EXIT.
STE003*----------------------------------------------------------------*
.     *
STE003*----------------------------------------------------------------*
.      3187-TRATAR-GLEBA-ANT           SECTION.
.     *----------------------------------------------------------------*
.          EXEC SQL
.              SELECT NSEQ_GLEBA_IMOV
               INTO  :WRK-GLEBA-NOVA
.              FROM  DB2PRD.TCORND_IMOV_BENEF
.              WHERE  CORCAM_CONTR_RURAL = :CRURB0V4.CORCAM-CONTR-RURAL
.                AND  CIMOV_CREDT_RURAL  = :CRURB0V4.CIMOV-CREDT-RURAL
.                AND  CTPO_CORND_GEOGR   =  1
.              FETCH  FIRST  1  ROWS  ONLY
.          END-EXEC
.     *
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0                EQUAL 'W')
.              SET DB2-SELECT          TO TRUE
.              MOVE '3187'             TO CRURWIY1-COD-ERRO
.                                      OF DFHCOMMAREA
.              MOVE 'COUNT'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '3187 - SELECT '
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9000-ERRO-DB2
.          END-IF.
.     *
.          IF (SQLCODE                 EQUAL +100)
.              MOVE ZEROS              TO WRK-GLEBA-NOVA
.              GO TO 3187-99-FIM
.          END-IF.
.     *
7C2511     MOVE WRK-MDGLEBA-ANT        TO WRK-MDGLEBA-ANT-AUX
AKY                EXEC CICS
AKY                     BIF DEEDIT
AKY                     FIELD(WRK-MDGLEBA-ANT)
AKY                     LENGTH(LENGTH OF WRK-MDGLEBA-ANT)
AKY                END-EXEC
.     *
AKY        MOVE WRK-MDGLEBA-ANT
AKY          TO WRK-MDGLEBA-ANT-X
AKY        MOVE WRK-MDGLEBA-ANT-N
AKY          TO NSEQ-GLEBA-IMOV
AKY          OF CRURB0V4.
.     *
.          MOVE WRK-CIMOV-CREDT-RURAL-ANT
.                                      TO CIMOV-CREDT-RURAL
.                                      OF CRURB0V4.
.          MOVE CORCAM-CONTR-RURAL     OF CRURB0MB
.                                      TO CORCAM-CONTR-RURAL
.                                      OF CRURB0V4.
AKY******  *OVE WRK-GLEBA-NOVA
AKY******                              *O NSEQ-GLEBA-IMOV
AKY******                              *F CRURB0V4.
.
.          EXEC SQL
.              SELECT COUNT(*)
.              INTO  :IND-COORD-GLEBA-2
.              FROM  DB2PRD.TCORND_IMOV_BENEF
.              WHERE  CORCAM_CONTR_RURAL = :CRURB0V4.CORCAM-CONTR-RURAL
.                AND  CIMOV_CREDT_RURAL  = :CRURB0V4.CIMOV-CREDT-RURAL
.                AND  NSEQ_GLEBA_IMOV    = :CRURB0V4.NSEQ-GLEBA-IMOV
.                AND  CTPO_CORND_GEOGR   =  1
.                AND  NCORND_IMOV_BENEF >= 5000
.          END-EXEC
.     *
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0                EQUAL 'W')
.              SET DB2-OPEN            TO TRUE
.              MOVE '3187'             TO CRURWIY1-COD-ERRO
.                                      OF DFHCOMMAREA
.              MOVE 'COUNT'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '3187 - COUNT '
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9000-ERRO-DB2
.          END-IF.
.     *
.          IF (SQLCODE                 EQUAL +100)
.              MOVE ZEROS              TO IND-COORD-GLEBA-2
.          END-IF.
.     *
.          MOVE IND-COORD-GLEBA-2      TO IND-COORD-GLEBA.
7C2511     MOVE WRK-MDGLEBA-ANT-AUX    TO WRK-MDGLEBA-ANT.
.     *----------------------------------------------------------------*
.      3187-99-FIM.                    EXIT.
STE003*----------------------------------------------------------------*
BR4.11*---------------------------------------------------------------*
BR4.11*3190-FORMATA-GRUPO-DESCLAS      SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*
BR4.11*   -MOVE ALL '*'       TO CRURY1-DT-DESCLASS
BR4.11*   -MOVE ALL '*'       TO CRURY1-VLR-DESCLASS
BR4.11*   -MOVE ALL '*'       TO CRURY1-MTV-DESCLASS
BR4.11*
BR4.11*   -MOVE CRURY1-COR0001-16 TO
BR4.11*          WRK-B09A-SIS(WRK-POSICAO:
BR4.11*                               LENGTH OF CRURY1-COR0001-16)
BR4.11*   -ADD LENGTH OF CRURY1-COR0001-16 TO WRK-TAM
BR4.11*   -ADD LENGTH OF CRURY1-COR0001-16 TO WRK-POSICAO
BR4.11*    .
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3190-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
QT1120*---------------------------------------------------------------*
QT1120 3190-FORMATA-GRUPO-DESCLAS      SECTION.
QT1120*---------------------------------------------------------------*
QT1120
QT1120     MOVE ALL '*'       TO CRURY1-GP-COR0001-DESCLASS
QT1120
QT1120     MOVE CRURY1-COR0001-16 TO
QT1120            WRK-B09A-SIS(WRK-POSICAO:
QT1120                                 LENGTH OF CRURY1-COR0001-16)
QT1120     ADD LENGTH OF CRURY1-COR0001-16 TO WRK-TAM
QT1120     ADD LENGTH OF CRURY1-COR0001-16 TO WRK-POSICAO

TESTEM     MOVE '3190-FORMATA-GRUPO'
TESTEM                                     TO WRK-PARAGRAFO
TESTEM     MOVE SPACES                     TO WRK-RLIN-MSGEM-TEXT
TESTEM     MOVE CRURY1-COR0001-16
TESTEM                                     TO WRK-RLIN-MSGEM-TEXT
TESTEM     PERFORM 9500-ERRO-LIVRE
QT1120     .
QT1120
QT1120*----------------------------------------------------------------*
QT1120 3190-99-FIM.                    EXIT.
QT1120*----------------------------------------------------------------*
CAT408*
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR OBTER INSTT RURAL                    *
      *----------------------------------------------------------------*
       3200-SELECIONAR-INSTIT          SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CRURB061
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
           INITIALIZE                  CRURB0TF
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
      *
           PERFORM 3220-CLAUSULA-CONTR
      *
           MOVE CDOCTO-CREDT-RURAL     OF CRURB0O0
                                       TO CDOCTO-CREDT-RURAL
                                       OF CRURB061
      *
           EXEC SQL
             SELECT  B.CINSTT_SIST_BACEN
               INTO :CRURB0TF.CINSTT-SIST-BACEN
             FROM   DB2PRD.TDOCTO_CREDT_RURAL A
                   ,DB2PRD.TINSTT_CREDT_BACEN B
             WHERE A.CINSTT_RURAL_BACEN  = B.CINSTT_RURAL_BACEN
             AND   A.CDOCTO_CREDT_RURAL  = :CRURB061.CDOCTO-CREDT-RURAL
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET  DB2-SELECT         TO TRUE
               MOVE 'TINSTT_CREDT_BACEN'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3200-SELECIONAR-INSTIT'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0045'             TO FRWKGDB2-LOCAL
TES1  *       -PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    SELECIONA O CODIGO PORTE BACEN - TIPO CATEGORIA EMITENTE    *
      *----------------------------------------------------------------*
       3212-BUSCA-PORTE-BACEN          SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CRURB055
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
      *
ID5687     MOVE CRURW00V-CPORTE-BNEFC-RURAL
                                       TO CPORTE-BNEFC-RURAL OF CRURB055
      *
           EXEC SQL
             SELECT  CPORTE_BNEFC_BACEN
               INTO :CRURB055.CPORTE-BNEFC-BACEN
               FROM  DB2PRD.TBNEFC_RURAL
              WHERE  CPORTE_BNEFC_RURAL = :CRURB055.CPORTE-BNEFC-RURAL
                AND  CSIT_REG_RURAL     = 'A'
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET  DB2-SELECT         TO TRUE
               MOVE 'TBNEFC_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3212-BUSCA-PORTE-BACEN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0048'             TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3212-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR OBTER DOCUMENTO DA CLAUSULA          *
      *---------------------------------------------------------------*
       3220-CLAUSULA-CONTR             SECTION.
      *---------------------------------------------------------------*
      *
           PERFORM 3230-OPEN-CSR08-CRURB0O0
           PERFORM 3235-FETCH-CSR08-CRURB0O0
           PERFORM UNTIL WRK-CODE100-B0L8    EQUAL 'S'
               PERFORM 3235-FETCH-CSR08-CRURB0O0
           END-PERFORM
      *
           PERFORM 3240-FECHAR-CSR08-CRURB0O0
           .
      *
      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DO CURSOR OPEN-CSR08-CRURB0O0
      *----------------------------------------------------------------*
       3230-OPEN-CSR08-CRURB0O0        SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CRURB0O0
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
           MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
                                       TO CPSSOA-JURID-CONTR
                                       OF CRURB0O0
           MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
                                       TO CTPO-CONTR-NEGOC
                                       OF CRURB0O0
           MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
                                       TO NSEQ-CONTR-NEGOC
                                       OF CRURB0O0
      *
           EXEC SQL
               OPEN CSR08-CRURB0O0
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-OPEN            TO TRUE
               MOVE '0150'             TO FRWKGDB2-LOCAL
               MOVE 'TCLAUS_CONTR_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3230-OPEN-CSR08-CRURB0O0'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FETCH CSR08-CRURB0O0
      *----------------------------------------------------------------*
       3235-FETCH-CSR08-CRURB0O0       SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
             FETCH CSR08-CRURB0O0 INTO
                   :CRURB0O0.CDOCTO-CREDT-RURAL
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-FETCH           TO TRUE
               MOVE '0151'             TO FRWKGDB2-LOCAL
               MOVE 'TCLAUS_CONTR_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3235-FETCH-CSR08-CRURB0O0'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF (SQLCODE                 EQUAL ZEROS OR +100)
              MOVE 'S'                 TO WRK-CODE100-B0L8
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3235-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      * ROTINA DE FECHAMENTO DO CURSOR CSR08-CRURB0O0
      *----------------------------------------------------------------*
       3240-FECHAR-CSR08-CRURB0O0     SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               CLOSE CSR08-CRURB0O0
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '0152'             TO FRWKGDB2-LOCAL
               MOVE 'TCLAUS_CONTR_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3240-FECHAR-CSR08-CRURB0O0'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FETCH CRURB0MV                       *
      *----------------------------------------------------------------*
       3311-FETCH-CSR01-CRURB0MV-JN    SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
             FETCH CSR01-CRURB0MV-JN INTO
                   :CRURB0MV.CPSSOA-JURID-CONTR
                  ,:CRURB0MV.CTPO-CONTR-NEGOC
                  ,:CRURB0MV.NSEQ-CONTR-NEGOC
                  ,:CRURB0MV.CPSSOA-JURID
                  ,:CRURB0MV.NSEQ-UND-ORGNZ
                  ,:CRURB0MV.CTPO-RESP-UND
                  ,:CRURB0MS.CPSSOA
                  ,:CRURB0MS.CTPO-PRTCP-PSSOA
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-FETCH           TO TRUE
               MOVE '0066'             TO FRWKGDB2-LOCAL
               MOVE 'TRESP_UND_RURAL'  TO FRWKGDB2-NOME-TABELA
               MOVE '3311-FETCH-CSR01-CRURB0MV-JN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF (SQLCODE                 EQUAL +100)
              MOVE 'S'                 TO  WRK-CODE100-B0MV
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      * ROTINA DE FECHAMENTO DO CURSOR.                                *
      *----------------------------------------------------------------*
       3312-FECHAR-CSR01-CRURB0MV-JN   SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               CLOSE CSR01-CRURB0MV-JN
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '0067'             TO FRWKGDB2-LOCAL
               MOVE 'TRESP_UND_RURAL'  TO FRWKGDB2-NOME-TABELA
               MOVE '3312-FECHAR-CSR01-CRURB0MV-JN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3312-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * OBTER DADOS DA FONTE RECURSO                                   *
      *----------------------------------------------------------------*
       3320-OBTER-DADOS-ORIGE          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CORIGE-REC-CREDT       OF CRURB008
                                       TO CORIGE-REC-CREDT
                                       OF CRURB037
      *
           EXEC SQL
             SELECT A.CFONTE_BACEN_RURAL
                   ,B.CLIN_SIST_BACEN
               INTO :CRURB037.CFONTE-BACEN-RURAL
                       :WRK-CFONTE-BACENR-NULL
                   ,:WRK-CLIN-SIST-BACEN
               FROM DB2PRD.TORIGE_REC_RURAL AS A
                   ,DB2PRD.TLIN_CREDT_BACEN AS B
             WHERE A.CORIGE_REC_CREDT   = :CRURB037.CORIGE-REC-CREDT
               AND A.CLIN_RURAL_BACEN   = B.CLIN_RURAL_BACEN
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET  DB2-SELECT         TO TRUE
               MOVE 'TORIGE_REC_RURAL' TO FRWKGDB2-NOME-TABELA
               MOVE '3320-OBTER-DADOS-ORIGE'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0068'             TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF WRK-CFONTE-BACENR-NULL   LESS  ZEROS
              MOVE ZEROS               TO  CFONTE-BACEN-RURAL
                                       OF CRURB037
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       3405-FORMATA-TORCAM      SECTION.
      *---------------------------------------------------------------*
      *
BR1218     MOVE 'N'                    TO WRK-CODE100-B0L7
BR1218*
           PERFORM 3410-OPEN-CSR02-CRURB0L7-JN.
           PERFORM 3411-FETCH-CSR02-CRURB0L7-JN
      *
           PERFORM UNTIL (WRK-IND1     GREATER  WRK-999)
                      OR (WRK-CODE100-B0L7
                                       EQUAL 'S')
EL0715             PERFORM 3421-OBTER-EXIGE-IMOVEL
      *
                   PERFORM 3413-MONTA-COR0001-9
      *
                   PERFORM 3411-FETCH-CSR02-CRURB0L7-JN
           END-PERFORM
      *
           PERFORM 3412-FECHAR-CSR02-CRURB0L7-JN
      *
           .
      *
      *----------------------------------------------------------------*
       3405-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DO CURSOR CSR02-CRURB0L7-JN                 *
      *----------------------------------------------------------------*
       3410-OPEN-CSR02-CRURB0L7-JN     SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
                                       TO CPSSOA-JURID-CONTR
                                       OF CRURB0L7
           MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
                                       TO CTPO-CONTR-NEGOC
                                       OF CRURB0L7
           MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
                                       TO NSEQ-CONTR-NEGOC
                                       OF CRURB0L7
      *
           EXEC SQL
               OPEN CSR02-CRURB0L7-JN
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-OPEN            TO TRUE
               MOVE '0034'             TO FRWKGDB2-LOCAL
               MOVE 'TCOMPO_CONTR_CREDT'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3410-OPEN-CSR02-CRURB0L7-JN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
BR1218     MOVE 'S'                    TO WRK-CSR02-ABERTO
           .
      *
      *----------------------------------------------------------------*
       3410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *                                                                *
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FETCH CRURB0L7                       *
      *----------------------------------------------------------------*
       3411-FETCH-CSR02-CRURB0L7-JN    SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
             FETCH CSR02-CRURB0L7-JN INTO
                   :CRURB0L7.CPSSOA-JURID-CONTR
                  ,:CRURB0L7.CTPO-CONTR-NEGOC
                  ,:CRURB0L7.NSEQ-CONTR-NEGOC
                  ,:CRURB0MB.CORCAM-CONTR-RURAL
                  ,:CRURB0MB.PTX-JURO-EMPTO
BR0918            ,:CRURB0MB.CINDCD-ECONM-NUMRC
                  ,:CRURB0MB.CEMPTO-RURAL-BACEN
EL0715            ,:CRURB0MB.CMODLD-RURAL
BR0621            ,:CRURB0MB.CCETFC-ORGNC-EMPTO
BR0621                  :WRK-CCETFC-ORGNC-EMPTO-NULL
BR0621            ,:CRURB0MB.EURL-CETFC-ORGNC
BR0621                  :WRK-EURL-CETFC-ORGNC-NULL
BR0621            ,:CRURB0MB.CRSTRB-EMPTO-RURAL
BR0621                  :WRK-CRSTRB-EMPTO-RURAL-NULL
BR0621            ,:CRURB0MB.EURL-RSTRB-EMPTO
BR0621                  :WRK-EURL-RSTRB-EMPTO-NULL
BR0621            ,:CRURB0MB.PENERG-RENOV-EMPTO
BR0621                  :WRK-PENERG-RENOV-EMPTO-NULL
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-FETCH           TO TRUE
               MOVE '0035'             TO FRWKGDB2-LOCAL
               MOVE 'TCOMPO_CONTR_CREDT'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3411-FETCH-CSR02-CRURB0L7-JN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF (SQLCODE                 EQUAL +100)
              MOVE 'S'                 TO  WRK-CODE100-B0L7
BR0621        GO TO 3411-99-FIM
           END-IF
      *
BR0621     IF  WRK-CCETFC-ORGNC-EMPTO-NULL LESS ZEROS
.              MOVE SPACES         TO CCETFC-ORGNC-EMPTO OF CRURB0MB
.          END-IF
.          IF  WRK-EURL-CETFC-ORGNC-NULL   LESS ZEROS
.              MOVE SPACES         TO EURL-CETFC-ORGNC-TEXT
.                                                        OF CRURB0MB
.          END-IF
.          IF  WRK-CRSTRB-EMPTO-RURAL-NULL LESS ZEROS
.              MOVE SPACES         TO CRSTRB-EMPTO-RURAL OF CRURB0MB
.          END-IF
.          IF  WRK-EURL-RSTRB-EMPTO-NULL   LESS ZEROS
.              MOVE SPACES         TO EURL-RSTRB-EMPTO-TEXT
.                                                        OF CRURB0MB
.          END-IF
.          IF  WRK-PENERG-RENOV-EMPTO-NULL LESS ZEROS
.              MOVE ZEROS          TO PENERG-RENOV-EMPTO OF CRURB0MB
BR0621     END-IF
      *
           .
      *
      *----------------------------------------------------------------*
       3411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      * ROTINA DE FECHAMENTO DO CURSOR CSR02-CRURB0L7-JN
      *----------------------------------------------------------------*
       3412-FECHAR-CSR02-CRURB0L7-JN  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               CLOSE CSR02-CRURB0L7-JN
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '0036'             TO FRWKGDB2-LOCAL
               MOVE 'TCOMPO_CONTR-CREDT'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3412-FECHAR-CSR02-CRURB0L7-JN'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
BR1218     MOVE 'N'                    TO WRK-CSR02-ABERTO
           .
      *
      *----------------------------------------------------------------*
       3412-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * MONTA DADOS COR0001-9
      *----------------------------------------------------------------*
       3413-MONTA-COR0001-9          SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE CRURWI42-REGISTRO
      *
           MOVE CEMPTO-RURAL-BACEN     OF CRURB0MB
                                       TO WRK-COM-SINAL-09
           MOVE WRK-SEM-SINAL-09       TO CRURWI42-E-CEMPTO-RURAL-BACEN
      *
           PERFORM 3520-ACESSAR-CRUR3C42
      *
           MOVE CRURWI42-S-CODIGO-BACEN
                                       TO CRURY1-COD-EMPNMNT
      *
ED2804*    OBTER SISTEMA DE PRODUCAO
 .         PERFORM 6300-OBTER-SIST-PROD
 .    *
 .         MOVE PTX-JURO-EMPTO         OF CRURB0MB
BR0918                                 TO CRURY1-PERC-JUROS-ENCARFIN
      *
WMM   * ALTERAR CAMPO TIPO ENCARGO COMPL. DE '*' PARA 9
WMM        MOVE  9                     TO CRURY1-TP-ENCARG-FIN-COMPL

BR0918     IF CINDCD-ECONM-NUMRC       OF CRURB0MB NOT EQUAL ZEROS
POS           MOVE 4                   TO CRURY1-TP-ENCARG-FIN-COMPL
BR0918     END-IF.

           MOVE  ALL '*'               TO CRURY1-PERC-RSC-STN
           MOVE  ALL '*'               TO CRURY1-PERC-RSC-FUND-CONSCL.
      *
      *
4SYS21*****XF CRURW63C-S-CEXIGE-IMOV-BENEF
4SYS21     IF CRURWFGI-S-CEXIGE-IMOV-BENEF
EL0715                                 EQUAL 'S'
               PERFORM 3414-TRATAR-INFO-IMOVEL
EL0715     ELSE
.              MOVE ZEROS              TO CRURY1-AREA-N
.                                         CRURY1-QTD-ITEM-FINCD-N
BR1218                                    CRURY1-QTD-PRV-PRODC
.              MOVE ALL '*'            TO CRURY1-AREA-X
.                                         CRURY1-QTD-ITEM-FINCD-X
BR1218                                    CRURY1-QTD-PRV-PRODC-X
BR1018     IF   (CRURY1-QTD-PRV-PRODC-X  NOT NUMERIC) OR
BR1018          (CRURY1-QTD-PRV-PRODC-X  EQUAL ZEROS)
BR1018          EXEC CICS
BR1018               BIF DEEDIT
BR1018               FIELD(CRURW03V-QTD-PROD-ESP)
BR1018               LENGTH(LENGTH OF CRURW03V-QTD-PROD-ESP)
BR1018          END-EXEC
BR1018          IF  (CRURW03V-QTD-PROD-ESP  IS NUMERIC) AND
BR1018              (CRURW03V-QTD-PROD-ESP  GREATER ZEROS)
BR1018               MOVE CRURW03V-QTD-PROD-ESP(3:14)
4.12                                   TO WRK-QTD-PROD-ESP-X
4.12                 MOVE WRK-QTD-PROD-ESP-N
4.12                                   TO WRK-QTD-PROD-ESP-C
4.12                 COMPUTE WRK-QTD-PROD-ESP-C =
4.12                         WRK-QTD-PROD-ESP-C * 100
4.12                 MOVE WRK-QTD-PROD-ESP-C
4.12                                   TO WRK-QTD-PROD-ESP-N
4.12                 MOVE WRK-QTD-PROD-ESP-X
BR1018                                 TO CRURY1-QTD-PRV-PRODC-X
BR1018          END-IF
BR1018     END-IF
BR1018*
EL0915         IF CRURW03V-CFNALD-RURAL
EL0915                                 EQUAL 2
EL0915*
EL0915             MOVE 'N'            TO WRK-CRUR3WJL-FIM1
EL0915*
EL0915             PERFORM 2150-OBTER-INFO-NOTA
EL0915                                 UNTIL WRK-CRUR3WJL-FIM1 EQUAL 'S'
ID6734             IF  WRK-QEMPTO-NOTA-PROTR-TOTAL NOT EQUAL ZEROS
EL0915                 MOVE WRK-QEMPTO-NOTA-PROTR-TOTAL
EL0915                                 TO WRK-VAR12-NUMS
EL0915                 MOVE WRK-VAR12-NUMSS
EL0915                                 TO CRURY1-QTD-ITEM-FINCD-N
ID6734             ELSE
ID6734                 MOVE CRURW08V-QTDE-FINAN
ID6734                                 TO CRURY1-QTD-ITEM-FINCD-N
ID6734             END-IF
EL0915         END-IF
EL0715     END-IF
ID6520*
ID6520* CASO AREA OU QUANTIDADE NAO FORAM CARREGADOS PELO IMOVEL OU
ID6520* PELA COMERCIALIZACAO VERIFICA OS CAMPOS DO DETALHE DO PRODUTO
ID6520     IF (CRURY1-AREA-X           EQUAL '***********'    OR
ID6520         CRURY1-AREA-N           EQUAL ZEROS)          AND
ID6520        (CRURY1-QTD-ITEM-FINCD-X EQUAL '**************' OR
ID6520         CRURY1-QTD-ITEM-FINCD-N EQUAL ZEROS)
ID6520*
ID6520         MOVE CRURW12V-MAREA-FINCD-RURAL-X TO WRK-AUX-X14
ID6520         STRING WRK-AUX-INT-09
ID6520                WRK-AUX-DEC-2
ID6520                DELIMITED BY SIZE INTO WRK-AREA-AUX
ID6520*
ID6520         MOVE CRURW12V-QITEM-FINCD-RURAL-X TO WRK-AUX-X14
ID6520         STRING WRK-AUX-INT-09
ID6520                WRK-AUX-DEC-2
ID6520                DELIMITED BY SIZE INTO WRK-QTDE-AUX
ID6520*
ID6520         IF  WRK-AREA-AUX-N  IS NUMERIC AND
ID6520             WRK-AREA-AUX-N  GREATER ZEROS
ID6520             MOVE WRK-AREA-AUX-N
ID6520                             TO CRURY1-AREA-N
ID6520         END-IF
ID6520*
ID6520*
ID6520         IF  WRK-QTDE-AUX-N  IS NUMERIC AND
ID6520             WRK-QTDE-AUX-N  GREATER ZEROS
ID6520             MOVE WRK-QTDE-AUX-N
ID6520                             TO CRURY1-QTD-ITEM-FINCD-N
ID6520         END-IF
ID6520     END-IF
ID6520*
ONEDA ***  **---------------------------------------
ONEDA ***  -> NAO ENVIA AREA QDO EXIGE COORDENADAS GEODESICAS
ONEDA      IF CRURY1-IDENTC-GLEBA(1:1) NOT EQUAL '*'
ONEDA         MOVE  ALL '*'        TO CRURY1-AREA-X
ONEDA      END-IF.
ONEDA ***  **---------------------------------------
ONEDA *
ED0604*    OBTER ALIQUOTA PROAGRO DO ORCAMENTO
 .         PERFORM 6200-OBTER-ALIQ-PROAGRO
ED0604*
CET        PERFORM 2600-OBTER-CET
SI4.13*    PERCENTUAL CUSTO EFETIVO TOTAL - CET
CET        IF  CRURWCAA-S-CET-ANO      GREATER ZEROS
CET            MOVE CRURWCAA-S-CET-ANO TO WRK-S9-08V7
CET            MOVE WRK-9-03V99        TO CRURY1-PERC-CST-EFT-TOT-N
CET        ELSE
SI4.13         MOVE ALL '*'            TO CRURY1-PERC-CST-EFT-TOT-X
CET        END-IF
      *
           MOVE CRURY1-COR0001-9       TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF CRURY1-COR0001-9).
           ADD  LENGTH OF CRURY1-COR0001-9 TO WRK-TAM.
           ADD  LENGTH OF CRURY1-COR0001-9 TO WRK-POSICAO.
      *
           MOVE CRURY1-COR0001-9       TO WRK-B09A-SIS2(WRK-POSICAO2:
                                       LENGTH OF CRURY1-COR0001-9).
           ADD  LENGTH OF CRURY1-COR0001-9 TO WRK-TAM2.
           ADD  LENGTH OF CRURY1-COR0001-9 TO WRK-POSICAO2.
      *
           ADD 1                       TO WRK-IND1
      *
           .
      *
      *----------------------------------------------------------------*
       3413-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * TRATAR INFORMACOES DE IMOVEL
      *----------------------------------------------------------------*
       3414-TRATAR-INFO-IMOVEL         SECTION.
      *----------------------------------------------------------------*
      *
      * FORMATAR AREA
ID6830     MOVE 'N'                    TO WRK-CRUR3WJL-FIM
ID6830     MOVE ZEROS                  TO WRK-NOCOR-GRP-PPSTA
ID6830*
           PERFORM 3427-BUSCAR-AREA
ID6830       UNTIL WRK-CRUR3WJL-FIM EQUAL 'S'
      *
      *    MOVER AREA FINANCIADA PARA SAIDA
ED2804*    ARREDONDAR PARA RETIRAR CASAS DECIMAIS
 .         COMPUTE WRK-AREA-FINCD-ACUM2 ROUNDED
 .                                     = WRK-AREA-FINCD-ACUM
 .    *
 .    *    REMOVER COMPACTACAO
 .         MOVE WRK-AREA-FINCD-ACUM2   TO WRK-AREA-FINCD-ACUM-SC
 .    *
 .    *    REMOVER SINAL
 .         MOVE WRK-AREA-FINCD-ACUM-SC TO WRK-VAR9-NUMS
 .         MOVE WRK-VAR9-NUMSS         TO CRURY1-AREA-N
 .    *
 .         IF  CRURY1-AREA-N EQUAL ZEROS
 .             MOVE  ALL '*'           TO CRURY1-AREA-X
 .         END-IF
 .    *
 .    *    REMOVER CASAS DECIMAIS
 .         COMPUTE WRK-QITEM-FINCD-ACUM-2 ROUNDED
 .                                     = WRK-QITEM-FINCD-ACUM
 .    *
 .    *    REMOVER COMPACTACAO
 .         MOVE  WRK-QITEM-FINCD-ACUM-2
 .                                     TO WRK-QITEM-FINCD-ACUM-SC
 .    *
 .    *    REMOVER SINAL
 .         MOVE WRK-QITEM-FINCD-ACUM-SC
 .                                     TO WRK-VAR12-NUMS
 .         MOVE WRK-VAR12-NUMSS        TO CRURY1-QTD-ITEM-FINCD-N
 .    *
 .         IF  CRURY1-QTD-ITEM-FINCD-N
 .                                    EQUAL ZEROS
 .             MOVE ALL '*'           TO CRURY1-QTD-ITEM-FINCD-X
ED2804     END-IF
      *
BR1218     IF (CRURY1-AREA-N           IS NUMERIC     AND
BR1218         CRURY1-AREA-N           GREATER ZEROS) AND
BR1218        (CRURY1-QTD-ITEM-FINCD-N IS NUMERIC     AND
BR1218         CRURY1-QTD-ITEM-FINCD-N GREATER ZEROS)
BR1218         IF  WRK-SIT-PARM-AREAGEOD
BR1218                                 EQUAL 'A'
BR1218             MOVE ALL '*'        TO CRURY1-AREA-X
BR1218         END-IF
BR1218     END-IF
BR1218*
EL0817     MOVE 'I'                    TO WRK-IND-PAG-FICHA
.     *                                                                *
.          PERFORM 5530-ACESSAR-CRUR3G8L
.            UNTIL WRK-COD-RET         EQUAL 'N'
.               OR WRK-ACHOU           EQUAL 'S'
EL0817
      *
           PERFORM 3126-RECEITA-BRUTA
      *
           PERFORM 3130-AREA-QTDE-FINAN
      *
BR1218     PERFORM 3729-ACESSA-CRURB0L5
BR1218*
BR1218     PERFORM 3415-ACESSA-CRURB0SZ
BR1218*
BR1218
BR1218     IF   (CRURW03V-CFNALD-RURAL EQUAL 1)
BR1218     AND  (CMODLD-SIST-BACEN OF CRURB0SZ EQUAL 26 OR 31 OR 01 OR
BR1218                                               35 OR 37 OR 39 OR
BR1218                                               55 OR 60 OR 63 OR
BR1218                                               65 OR 67 OR 73 OR
BR1218                                               75 OR 77 OR 38)
BR1218     AND  (CTPO-BNEFC-BACEN  OF CRURB0L5
BR1218                            NOT EQUAL '002' AND '009')
BR1218          CONTINUE
BR1218     ELSE
BR1218          MOVE ALL '*'           TO CRURY1-QTD-PRV-PRODC-X
BR1218          GO                     TO 3414-99-FIM
BR1218     END-IF
BR1218
      *  PRODUTIVIDADE TOTAL ESPERADA
           IF WRK-AREA-FINAN           NOT EQUAL ZEROS
              COMPUTE  WRK-QPROTV-IMOV ROUNDED   =
                      (WRK-AREA-FINAN * WRK-QPROTV-IMOV-ACUM)
BSI999        MOVE  WRK-QPROTV-IMOV-ACUM-AREA
BSI999                                 TO WRK-QPROTV-IMOV
           ELSE
              COMPUTE  WRK-QPROTV-IMOV ROUNDED   =
                      (WRK-QTDE-FINAN * WRK-QPROTV-IMOV-ACUM)
BSI999        MOVE  WRK-QPROTV-IMOV-ACUM-ITEM
BSI999                                 TO WRK-QPROTV-IMOV
           END-IF
      *
           MOVE  WRK-QPROTV-IMOV       TO WRK-VAR12-NUMS
           MOVE  WRK-VAR12-NUMSS       TO CRURY1-QTD-PRV-PRODC
           IF  CRURY1-QTD-PRV-PRODC    EQUAL ZEROS
               MOVE ALL '*'            TO CRURY1-QTD-PRV-PRODC-X
           END-IF
BR1018*
BR1018     IF   (CRURY1-QTD-PRV-PRODC-X  NOT NUMERIC) OR
BR1018          (CRURY1-QTD-PRV-PRODC-X  EQUAL ZEROS)
BR1018          EXEC CICS
BR1018               BIF DEEDIT
BR1018               FIELD(CRURW03V-QTD-PROD-ESP)
BR1018               LENGTH(LENGTH OF CRURW03V-QTD-PROD-ESP)
BR1018          END-EXEC
BR1018          IF  (CRURW03V-QTD-PROD-ESP  IS NUMERIC) AND
BR1018              (CRURW03V-QTD-PROD-ESP  GREATER ZEROS)
BR1018               MOVE CRURW03V-QTD-PROD-ESP(3:14)
4.12                                   TO WRK-QTD-PROD-ESP-X
4.12                 MOVE WRK-QTD-PROD-ESP-N
4.12                                   TO WRK-QTD-PROD-ESP-C
4.12                 COMPUTE WRK-QTD-PROD-ESP-C =
4.12                         WRK-QTD-PROD-ESP-C * 100
4.12                 MOVE WRK-QTD-PROD-ESP-C
4.12                                   TO WRK-QTD-PROD-ESP-N
4.12                 MOVE WRK-QTD-PROD-ESP-X
BR1018                                 TO CRURY1-QTD-PRV-PRODC-X
BR1018          END-IF
BR1018     END-IF
BR1018*
           .
      *
      *----------------------------------------------------------------*
       3414-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
BR1218*----------------------------------------------------------------*
BR1218*    ACESSO A TABELA CRURB0SZ
BR1218*----------------------------------------------------------------*
BR1218 3415-ACESSA-CRURB0SZ            SECTION.
BR1218*----------------------------------------------------------------*
BR1218*
BR1218     MOVE CRURW03V-CMODLD-RURAL-BACEN
BR1218                                 TO CMODLD-RURAL-BACEN OF CRURB0SZ
BR1218
BR1218     EXEC SQL
BR1218       SELECT CMODLD_SIST_BACEN
BR1218       INTO   :CRURB0SZ.CMODLD-SIST-BACEN
BR1218       FROM   DB2PRD.TMODLD_RURAL_BACEN
BR1218       WHERE  CMODLD_RURAL_BACEN = :CRURB0SZ.CMODLD-RURAL-BACEN
BR1218     END-EXEC
BR1218
BR1218     IF ((SQLCODE            NOT EQUAL ZEROS) OR
BR1218         (SQLWARN0               EQUAL 'W'))
BR1218         SET  DB2-SELECT         TO TRUE
BR1218         MOVE 'TMODLD_RURAL_BACEN'
BR1218                                 TO FRWKGDB2-NOME-TABELA
BR1218         MOVE '3414-TRATAR-INFO-IMOVEL'
BR1218                                 TO FRWKGHEA-IDEN-PARAGRAFO
BR1218         MOVE '3414'             TO FRWKGDB2-LOCAL
BR1218         PERFORM 9000-ERRO-DB2
BR1218     END-IF
BR1218     .
BR1218*
BR1218*----------------------------------------------------------------*
BR1218 3415-99-FIM.                    EXIT.
BR1218*----------------------------------------------------------------*
BR1218*
      *----------------------------------------------------------------*
      * BUSCAR O SISTEMA DE PRODUCAO
      *----------------------------------------------------------------*
       3420-BUSCAR-GRUPOS              SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  FNEBW3PI-BLOCO-ENTRADA
                                       FNEBW000-BLOCO-RETORNO
                                       OF WRK-AREA-FNEB33PL
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
      *
 P-01 *** GRUPO 311
 P-01      MOVE 311                    TO FNEBW3PI-E-CGRP-INFO-FLUXO
 P-01                                  OF WRK-AREA-FNEB33PL
 P-01      PERFORM 3424-BUSCAR-INFO-GRUPO
 P-01      MOVE FNEBW3PI-S-WCONTD-GRP-PPSTA
 P-01                                  OF WRK-AREA-FNEB33PL (1)
 P-01                                  TO CRURW00V-REGISTROS
      *
      *** GRUPO 325
           MOVE 325                    TO FNEBW3PI-E-CGRP-INFO-FLUXO
                                       OF WRK-AREA-FNEB33PL
           PERFORM 3424-BUSCAR-INFO-GRUPO
           MOVE FNEBW3PI-S-WCONTD-GRP-PPSTA
                                       OF WRK-AREA-FNEB33PL (1)
                                       TO CRURW06V-REGISTROS
      *
AS3007*** GRUPO 316
.          MOVE 316                    TO FNEBW3PI-E-CGRP-INFO-FLUXO
.                                      OF WRK-AREA-FNEB33PL
.          PERFORM 3424-BUSCAR-INFO-GRUPO
.          MOVE FNEBW3PI-S-WCONTD-GRP-PPSTA
.                                      OF WRK-AREA-FNEB33PL (1)
AS3007                                 TO CRURW03V-REGISTROS
           .
      *
      *----------------------------------------------------------------*
       3420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
4SYS21*----------------------------------------------------------------*
4SYS21* BUSCAR SE EXIGE IMOVEL NA MODALIDADE
.     *----------------------------------------------------------------*
.      3421-OBTER-EXIGE-IMOVEL         SECTION.
.     *----------------------------------------------------------------*
4SYS21*
4SYS21*    XNITIALIZE                  CRURW63C-BLOCO-ENTRADA
4SYS21*                                XF WRK-AREA-CRUR463S
4SYS21*
4SYS21*    XOVE CMODLD-RURAL          OF CRURB0MB
4SYS21*                               XO WRK-05-CSINAL
4SYS21*    XOVE WRK-05-SSINAL         XO CRURW63C-E-CMODLD-RURAL
4SYS21*
4SYS21*    XXEC CICS LINK
4SYS21*         XROGRAM  (WRK-CRUR463S)
4SYS21*         XOMMAREA (WRK-AREA-CRUR463S)
4SYS21*         XENGTH   (LENGTH       OF WRK-AREA-CRUR463S)
4SYS21*         XXHANDLE
4SYS21*    XND-EXEC
4SYS21*
4SYS21*    XF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
4SYS21*        XOVE '0340'             TO CRURWIY1-COD-ERRO
4SYS21*                                XF DFHCOMMAREA
4SYS21*        XOVE '2650-BUSCAR-SIMULA-RUAL'
4SYS21*                                XO FRWKGHEA-IDEN-PARAGRAFO
4SYS21*        XERFORM 9400-ERRO-CICS
4SYS21*    XND-IF
4SYS21*
4SYS21*    XVALUATE CRURW00W-COD-RETORNO
4SYS21*                                XF WRK-AREA-CRUR463S
4SYS21*        XHEN 00
4SYS21*            XONTINUE
4SYS21*
4SYS21*        XHEN OTHER
4SYS21*            XOVE '0350'         TO CRURWIY1-COD-ERRO
4SYS21*                                XF DFHCOMMAREA
4SYS21*            XOVE CRURW00W-COD-MENSAGEM
4SYS21*                                XF WRK-AREA-CRUR463S
4SYS21*                                XO FRWKGMOD-COD-MENSAGEM
4SYS21*            XOVE WRK-CRUR463S   TO FRWKGMOD-NOME-MODULO
4SYS21*            XOVE CRURW00W-BLOCO-RETORNO
4SYS21*                                XF WRK-AREA-CRUR463S
4SYS21*                                XO FRWKGMOD-BLOCO-RETORNO
4SYS21*            XOVE '2650-BUSCAR-SIMULA-RUAL'
4SYS21*                                XO FRWKGHEA-IDEN-PARAGRAFO
4SYS21*            XERFORM 9300-ERRO-MODULO
4SYS21*    XND-EVALUATE
4SYS21*    .
4SYS21*
4SYS21     INITIALIZE                  CRURWFGI-BLOCO-ENTRADA
.                                      OF WRK-AREA-CRUR4F0C
.     *
.          MOVE CMODLD-RURAL          OF CRURB0MB
.                                     TO WRK-05-CSINAL
.          MOVE WRK-05-SSINAL         TO CRURWFGI-E-CMODLD-RURAL
.     *
.          MOVE CTPO-BNEFC-RURAL      OF CRURB0L5
.                                     TO WRK-02-CSINAL
.          MOVE WRK-02-SSINAL         TO CRURWFGI-E-CTPO-BNEFC-RURAL
.
.          EXEC CICS LINK
.               PROGRAM  (WRK-CRUR4F0C)
.               COMMAREA (WRK-AREA-CRUR4F0C)
.               LENGTH   (LENGTH       OF WRK-AREA-CRUR4F0C)
.               NOHANDLE
.          END-EXEC
.     *
.          IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
.              MOVE '0340'             TO CRURWIY1-COD-ERRO
.                                      OF DFHCOMMAREA
.              MOVE '3421-OBTER-EXIGE-IMOVEL'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9400-ERRO-CICS
.          END-IF
.     *
.          EVALUATE CRURW00W-COD-RETORNO
.                                      OF WRK-AREA-CRUR4F0C
.              WHEN 00
.                  CONTINUE
.     *
.              WHEN OTHER
.                  MOVE '0350'         TO CRURWIY1-COD-ERRO
.                                      OF DFHCOMMAREA
.                  MOVE CRURW00W-COD-MENSAGEM
.                                      OF WRK-AREA-CRUR4F0C
.                                      TO FRWKGMOD-COD-MENSAGEM
.                  MOVE WRK-CRUR4F0C   TO FRWKGMOD-NOME-MODULO
.                  MOVE CRURW00W-BLOCO-RETORNO
.                                      OF WRK-AREA-CRUR4F0C
.                                      TO FRWKGMOD-BLOCO-RETORNO
.                  MOVE '3421-OBTER-EXIGE-IMOVEL'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.                  PERFORM 9300-ERRO-MODULO
.          END-EVALUATE
4SYS21     .
.     *
.     *----------------------------------------------------------------*
.      3421-99-FIM.                    EXIT.
EL0715*----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       3422-BUSCAR-PROPOSTA            SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  FNEBWBZI-BLOCO-ENTRADA
                                       FNEBW000-BLOCO-RETORNO
                                       OF WRK-AREA-FNEB3BZL
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
      *
           MOVE CPSSOA-JURID-CONTR     OF CRURB008
                                       TO WRK-COM-SINAL-10
           MOVE WRK-SEM-SINAL-10       TO FNEBWBZI-E-CPSSOA-JURID-VINC
           MOVE 25                     TO FNEBWBZI-E-CTPO-CONTR-VINC
           MOVE NSEQ-CONTR-NEGOC       OF CRURB008
                                       TO WRK-COM-SINAL-10
           MOVE WRK-SEM-SINAL-10       TO FNEBWBZI-E-NSEQ-CONTR-VINC
           MOVE 'I'                    TO FNEBWBZI-INDICADOR-PAGINACAO
           MOVE  1                     TO FNEBWBZI-E-MAX-OCCOR
      *
           EXEC CICS LINK
                PROGRAM  (WRK-FNEB3BZL)
                COMMAREA (WRK-AREA-FNEB3BZL)
                LENGTH   (LENGTH OF WRK-AREA-FNEB3BZL)
                NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0070'             TO CRURWIY1-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE '2950-ACESSA-FNEB3BZL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
           EVALUATE FNEBW000-COD-RETORNO
                                       OF WRK-AREA-FNEB3BZL
               WHEN 00
               WHEN 01
                    MOVE FNEBW000-BLOCO-RETORNO
                                       OF WRK-AREA-FNEB3BZL
                                       TO CRURWIY1-BLOCO-RETORNO
                                       OF DFHCOMMAREA
               WHEN OTHER
                    MOVE FNEBW000-BLOCO-RETORNO
                                       OF WRK-AREA-FNEB3BZL
                                       TO CRURWIY1-BLOCO-RETORNO
                                       OF DFHCOMMAREA
                                          FRWKGMOD-BLOCO-RETORNO
                    MOVE WRK-FNEB3BZL  TO FRWKGMOD-NOME-MODULO
                    MOVE '3422-BUSCAR-PROPOSTA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
                    PERFORM 9300-ERRO-MODULO
           END-EVALUATE
      *
           .
      *----------------------------------------------------------------*
       3422-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DO CURSOR CSR03-CRURB0L4
      *----------------------------------------------------------------*
       3424-BUSCAR-INFO-GRUPO          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 1                      TO FNEBW3PI-E-MAX-OCORR
                                       OF WRK-AREA-FNEB33PL
      *
           MOVE 1                      TO FNEBW3PI-E-NVRSAO-GRP-FLUXO
                                       OF WRK-AREA-FNEB33PL
      *
           MOVE FNEBWBZI-S-CPSSOA-PPSTA-NEGOC(1)
                                       TO FNEBW3PI-E-CPSSOA-JURID-CONTR
                                       OF WRK-AREA-FNEB33PL
      *
           MOVE FNEBWBZI-S-CCLASF-PPSTA-NEGOC(1)
                                       TO FNEBW3PI-E-CTPO-CONTR-NEGOC
                                       OF WRK-AREA-FNEB33PL
      *
           MOVE FNEBWBZI-S-NSEQ-PPSTA-NEGOC(1)
                                       TO FNEBW3PI-E-NSEQ-CONTR-NEGOC
                                       OF WRK-AREA-FNEB33PL
      *
           MOVE 'I'                    TO FNEBW3PI-INDICADOR-PAGINACAO
                                       OF WRK-AREA-FNEB33PL
      *
           EXEC CICS LINK
                PROGRAM  (WRK-FNEB33PL)
                COMMAREA (WRK-AREA-FNEB33PL)
                LENGTH   (LENGTH       OF WRK-AREA-FNEB33PL)
                NOHANDLE
           END-EXEC
      *                                                                *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '3424-BUSCAR-INFO-GRUPO'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *                                                                *
           EVALUATE FNEBW000-COD-RETORNO
                                       OF WRK-AREA-FNEB33PL
               WHEN 00
               WHEN 01
                    CONTINUE
               WHEN OTHER
                    MOVE '0080'     TO CRURWIY1-COD-ERRO
                                    OF DFHCOMMAREA
                    MOVE FNEBW000-COD-RETORNO
                                    OF WRK-AREA-FNEB33PL
                                    TO CRURWIY1-COD-RETORNO
                                    OF DFHCOMMAREA
                    MOVE FNEBW000-COD-MENSAGEM
                                    OF WRK-AREA-FNEB33PL
                                    TO CRURWIY1-COD-MENSAGEM
                                    OF DFHCOMMAREA
                    MOVE FNEBW000-BLOCO-RETORNO
                                    OF WRK-AREA-FNEB33PL
                                    TO FRWKGMOD-BLOCO-RETORNO
                    MOVE WRK-FNEB33PL  TO FRWKGMOD-NOME-MODULO
                    MOVE '3424-BUSCAR-INFO-GRUPO'
                                    TO FRWKGHEA-IDEN-PARAGRAFO
                    PERFORM 9300-ERRO-MODULO
           END-EVALUATE
           .
      *
      *----------------------------------------------------------------*
       3424-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER GRUPO DE INFORMACAO IMOVEL                *
      *----------------------------------------------------------------*
       3425-BUSCAR-IMOVEL              SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CRURWWJI-BLOCO-ENTRADA
                                       WRK-NOCOR-GRP-PPSTA
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
      *
           MOVE 15                     TO CRURWWJI-E-MAX-OCORR
                                       OF WRK-AREA-CRUR3WJL
           MOVE 1                      TO CRURWWJI-E-NVRSAO-GRP-FLUXO
                                       OF WRK-AREA-CRUR3WJL
           MOVE 326                    TO CRURWWJI-E-CGRP-INFO-FLUXO
                                       OF WRK-AREA-CRUR3WJL
           MOVE FNEBWBZI-S-CPSSOA-PPSTA-NEGOC(1)
                                       TO CRURWWJI-E-CPSSOA-JURID-CONTR
                                       OF WRK-AREA-CRUR3WJL
           MOVE FNEBWBZI-S-CCLASF-PPSTA-NEGOC(1)
                                       TO CRURWWJI-E-CTPO-CONTR-NEGOC
                                       OF WRK-AREA-CRUR3WJL
           MOVE FNEBWBZI-S-NSEQ-PPSTA-NEGOC(1)
                                       TO CRURWWJI-E-NSEQ-CONTR-NEGOC
                                       OF WRK-AREA-CRUR3WJL
           MOVE WRK-NOCOR-GRP-PPSTA    TO CRURWWJI-E-NOCOR-GRP-PPSTA
                                       OF WRK-AREA-CRUR3WJL
           MOVE 325                    TO CRURWWJI-E-CGRP-FLUXO-SUPE
                                       OF WRK-AREA-CRUR3WJL
           MOVE 1                      TO CRURWWJI-E-NVRSAO-GRP-SUPE
                                       OF WRK-AREA-CRUR3WJL
           MOVE FNEBW3PI-S-NOCOR-GRP-PPSTA (1)
                                       TO WRK-NUMER-03
           MOVE WRK-NUMER-02           TO CRURWWJI-E-NVRSAO-GRP-SUPE
                                       OF WRK-AREA-CRUR3WJL
      *
           EXEC CICS LINK
               PROGRAM  (WRK-CRUR3WJL)
               COMMAREA (WRK-AREA-CRUR3WJL)
               LENGTH          (LENGTH OF WRK-AREA-CRUR3WJL)
               NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0090'             TO CRURWIY1-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE '3425-BUSCAR-IMOVEL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
           EVALUATE CRURW00W-COD-RETORNO
                                       OF WRK-AREA-CRUR3WJL
             WHEN 00
EL0715            IF CRURWWJI-S-MAX-OCORR
.                                      EQUAL ZEROS
EL0116*-->        ,  NOS CASOS ONDE NAO HA IMOVEL ENVIA O CPF/CNPJ DO
EL0116*-->        ,  BENEFICIARIO NO CPF/CNPJ DO PROPRIETARIO
EL0116            ,  ,  IF PSDCW12I-S-CD-IDENT-PESSOA EQUAL 'F'
EL0116            ,  ,  ,  MOVE 'F'    TO CRURY1-TP-PESSOA-PROPT-F
7C5.10            ,  ,  ,  MOVE CRURY1-CNPJ-CPF-EMIT(1:9)
EL0116            ,  ,  ,              TO WRK-CPFPROPT-CORPO
7C5.10            ,  ,  ,  MOVE CRURY1-CNPJ-CPF-EMIT(10:2)
EL0116            ,  ,  ,              TO WRK-CPFPROPT-DG
EL0116            ,  ,  ,  MOVE WRK-CPFPROPT
EL0116            ,  ,  ,              TO CRURY1-CPFPROPT
BSI1              ,  ,  ,  MOVE ALL '*'
7C5.10*--------------------------------TO-CRURY1-NIRF-F
7C5.10            ,  ,  ,              TO CRURY1-CIB-F
BSI1              ,  ,  ,                 CRURY1-COD-SNCR-F
BSI1              ,  ,  ,                 CRURY1-CREG-AMBTL-RURAL-F
BR0521            ,  ,  ,                 CRURY1-COUTGA-AGUA-IMOV-F
BR0521            ,  ,  ,                 CRURY1-PPRESV-AMBTL-PPRIE-F-X
EL0116            ,  ,  ,  MOVE CRURY1-COR0001-10-F
EL0116            ,  ,  ,              TO WRK-B09A-SIS(WRK-POSICAO:
EL0116            ,  ,  ,  LENGTH OF CRURY1-COR0001-10-F)
EL0116            ,  ,  ,  ADD  LENGTH OF CRURY1-COR0001-10-F
                  ,  ,  ,              TO WRK-TAM
EL0116            ,  ,  ,  ADD  LENGTH OF CRURY1-COR0001-10-F
EL0116            ,  ,  ,              TO WRK-POSICAO
EL0116            ,  ,  ELSE
EL0116            ,  ,  ,   IF  PSDCW12I-S-CD-IDENT-PESSOA EQUAL 'J'
EL0116            ,  ,  ,   ,   MOVE 'J'   TO CRURY1-TP-PESSOA-PROPT-J
EL0116            ,  ,  ,   ,   MOVE CRURY1-CNPJ-CPF-EMIT(1:8)
EL0116            ,  ,  ,   ,          TO WRK-CNPJBASE-PROPT-N
EL0116            ,  ,  ,   ,   MOVE WRK-CNPJBASE-PROPT
EL0116            ,  ,  ,   ,          TO CRURY1-CNPJBASE-PROPT
BSI1              ,  ,  ,   ,   MOVE ALL '*'
7C5.10*--------------------------------TO-CRURY1-NIRF-J
7C5.10            ,  ,  ,   ,          TO CRURY1-CIB-J
BSI1              ,  ,  ,   ,             CRURY1-COD-SNCR-J
BSI1              ,  ,  ,   ,             CRURY1-CREG-AMBTL-RURAL-J
BR0521            ,  ,  ,   ,             CRURY1-COUTGA-AGUA-IMOV-J
BR0521            ,  ,  ,   ,             CRURY1-PPRESV-AMBTL-PPRIE-J-X
EL0116            ,  ,  ,   ,   MOVE CRURY1-COR0001-10-J
EL0116            ,  ,  ,   ,          TO WRK-B09A-SIS(WRK-POSICAO:
EL0116            ,  ,  ,   ,        LENGTH OF CRURY1-COR0001-10-J)
EL0116            ,  ,  ,   ,   ADD  LENGTH OF CRURY1-COR0001-10-J
EL0116            ,  ,  ,   ,          TO WRK-TAM
EL0116            ,  ,  ,   ,   ADD  LENGTH OF CRURY1-COR0001-10-J
EL0116            ,  ,  ,   ,          TO WRK-POSICAO
EL0116            ,  ,  ,   ELSE
EL0116            ,  ,  ,   ,   MOVE ZEROS TO CRURY1-CNPJ-CPF-EMIT
EL0116            ,  ,  ,   END-IF
EL0116            ,  ,  END-IF
EL0116            ,  MOVE 'S'          TO WRK-CODE100-B0L7
EL0715            ELSE
                     PERFORM VARYING WRK-IND6 FROM 1 BY 1
                               UNTIL WRK-IND6
                                          GREATER CRURWWJI-S-MAX-OCORR
      *
                       MOVE CRURWWJI-S-WCONTD-GRP-PPSTA (WRK-IND6)
STE005*                                *O CRURW07V-REGISTROS
STE005                                 TO CRURW27V-REGISTROS
                       PERFORM 3131-TRATA-PROPRIET
                     END-PERFORM
      *
                     IF CRURWWJI-S-IND-PAGINACAO
                                       EQUAL 'S'
                         CONTINUE
                      ELSE
                        MOVE 'S'       TO WRK-CRUR3WJL-FIM
                     END-IF
EL0715            END-IF
             WHEN OTHER
      *
                  MOVE '0100'          TO CRURWIY1-COD-ERRO
                                       OF DFHCOMMAREA
                  MOVE WRK-CRUR3WJL    TO FRWKGMOD-NOME-MODULO
                  MOVE 'CRUR1206'      TO CRURWIY1-COD-MENSAGEM
                                       OF DFHCOMMAREA
                  MOVE CRURW00W-COD-RETORNO
                                       OF WRK-AREA-CRUR3WJL
                                       TO FRWKGMOD-COD-RETORNO
                  MOVE CRURWIY1-COD-ERRO
                                       OF DFHCOMMAREA
                                       TO FRWKGMOD-COD-ERRO
                  MOVE CRURWIY1-COD-MENSAGEM
                                       OF DFHCOMMAREA
                                       TO FRWKGMOD-COD-MENSAGEM
                  MOVE '3425-BUSCAR-IMOVEL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
                  PERFORM 9300-ERRO-MODULO
           END-EVALUATE
      *
           MOVE CRURWWJI-S-NOCOR-GRP-PPSTA (WRK-IND6)
                                       TO WRK-NOCOR-GRP-PPSTA
           .
      *
      *----------------------------------------------------------------*
       3425-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER GRUPO DE INFORMACAO IMOVEL                *
      *----------------------------------------------------------------*
       3427-BUSCAR-AREA                SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CRURWWJI-BLOCO-ENTRADA
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
      *
           MOVE 15                     TO CRURWWJI-E-MAX-OCORR
                                       OF WRK-AREA-CRUR3WJL
           MOVE 1                      TO CRURWWJI-E-NVRSAO-GRP-FLUXO
                                       OF WRK-AREA-CRUR3WJL
           MOVE 326                    TO CRURWWJI-E-CGRP-INFO-FLUXO
                                       OF WRK-AREA-CRUR3WJL
           MOVE FNEBWBZI-S-CPSSOA-PPSTA-NEGOC(1)
                                       TO CRURWWJI-E-CPSSOA-JURID-CONTR
                                       OF WRK-AREA-CRUR3WJL
           MOVE FNEBWBZI-S-CCLASF-PPSTA-NEGOC(1)
                                       TO CRURWWJI-E-CTPO-CONTR-NEGOC
                                       OF WRK-AREA-CRUR3WJL
           MOVE FNEBWBZI-S-NSEQ-PPSTA-NEGOC(1)
                                       TO CRURWWJI-E-NSEQ-CONTR-NEGOC
                                       OF WRK-AREA-CRUR3WJL
           MOVE WRK-NOCOR-GRP-PPSTA    TO CRURWWJI-E-NOCOR-GRP-PPSTA
                                       OF WRK-AREA-CRUR3WJL
           MOVE 325                    TO CRURWWJI-E-CGRP-FLUXO-SUPE
                                       OF WRK-AREA-CRUR3WJL
           MOVE 1                      TO CRURWWJI-E-NVRSAO-GRP-SUPE
                                       OF WRK-AREA-CRUR3WJL
           MOVE FNEBW3PI-S-NOCOR-GRP-PPSTA (1)
                                       TO WRK-NUMER-03
           MOVE WRK-NUMER-02           TO CRURWWJI-E-NVRSAO-GRP-SUPE
                                       OF WRK-AREA-CRUR3WJL
      *
           EXEC CICS LINK
               PROGRAM  (WRK-CRUR3WJL)
               COMMAREA (WRK-AREA-CRUR3WJL)
               LENGTH          (LENGTH OF WRK-AREA-CRUR3WJL)
               NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0110'             TO CRURWIY1-COD-ERRO
                                        OF DFHCOMMAREA
               MOVE '3427-BUSCAR-AREA'
      *
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
           EVALUATE CRURW00W-COD-RETORNO
                                       OF WRK-AREA-CRUR3WJL
             WHEN 00
                  PERFORM VARYING WRK-IND7 FROM 1 BY 1
                            UNTIL WRK-IND7 GREATER CRURWWJI-S-MAX-OCORR
      *
                    MOVE CRURWWJI-S-WCONTD-GRP-PPSTA (WRK-IND7)
STE005*                                *O CRURW07V-REGISTROS
STE005                                 TO CRURW27V-REGISTROS
STE005*             *OVE CRURW07V-MAREA-FINCD-RURAL
STE005              MOVE CRURW27V-MAREA-FINCD-RURAL
                                       TO WRK-MAREA-FINCD-COMP3
STE005*             *OVE CRURW07V-QITEM-FINCD-RURAL
 .                  INSPECT CRURW27V-QTDTOTAL-UND-ANML
 .                                 REPLACING ALL 'X' BY '0'
 .                  INSPECT CRURW27V-QTDTOTAL-UND-ANML
 .                                 REPLACING ALL ' ' BY '0'
 .                  MOVE CRURW27V-QTDTOTAL-UND-ANML
 .                                 TO WRK-QTD-TOTAL-UND
MJ0725              IF WRK-QTD-TOTAL-UND-N IS NUMERIC AND
STE005                 WRK-QTD-TOTAL-UND-N GREATER ZEROS
 .                     MOVE WRK-QTD-TOTAL-UND-N
 .                                 TO CRURW27V-QITEM-FINCD-RURAL
STE005              END-IF
STE005              MOVE CRURW27V-QITEM-FINCD-RURAL
                                       TO WRK-QITEM-FINCD-COMP3
STE005*             *OVE CRURW07V-QPROTV-IMOV-BENEF
STE005              MOVE CRURW27V-QPROTV-IMOV-BENEF
                                       TO WRK-QPROTV-IMOV-COMP3
      *
                    COMPUTE WRK-AREA-FINCD-ACUM
                                       = (WRK-AREA-FINCD-ACUM
                                       +  WRK-MAREA-FINCD-COMP3)
                    COMPUTE WRK-QITEM-FINCD-ACUM
                                       = (WRK-QITEM-FINCD-ACUM
                                       +  WRK-QITEM-FINCD-COMP3)
                    COMPUTE WRK-QPROTV-IMOV-ACUM
                                       = (WRK-QPROTV-IMOV-ACUM
                                       +  WRK-QPROTV-IMOV-COMP3)
BSI999              COMPUTE WRK-QPROTV-IMOV-ACUM-AREA ROUNDED
BSI999                                 = (WRK-QPROTV-IMOV-ACUM-AREA
BSI999                                 + (WRK-MAREA-FINCD-COMP3
BSI999                                 *  WRK-QPROTV-IMOV-COMP3))
BSI999              COMPUTE WRK-QPROTV-IMOV-ACUM-ITEM ROUNDED
BSI999                                 = (WRK-QPROTV-IMOV-ACUM-ITEM
BSI999                                 + (WRK-QITEM-FINCD-COMP3
BSI999                                 *  WRK-QPROTV-IMOV-COMP3))
ID6830              MOVE CRURWWJI-S-NOCOR-GRP-PPSTA (WRK-IND7)
                                       TO WRK-NOCOR-GRP-PPSTA
                  END-PERFORM
      *
                  IF CRURWWJI-S-IND-PAGINACAO
                                       EQUAL 'S'
                      CONTINUE
                   ELSE
                     MOVE 'S'          TO WRK-CRUR3WJL-FIM
                  END-IF
             WHEN OTHER
                  MOVE '0120'          TO CRURWIY1-COD-ERRO
                                       OF DFHCOMMAREA
                  MOVE WRK-CRUR3WJL    TO FRWKGMOD-NOME-MODULO
                  MOVE 'CRUR1206'      TO CRURWIY1-COD-MENSAGEM
                                       OF DFHCOMMAREA
                  MOVE CRURW00W-COD-RETORNO
                                       OF WRK-AREA-CRUR3WJL
                                       TO FRWKGMOD-COD-RETORNO
                  MOVE CRURWIY1-COD-ERRO
                                       OF DFHCOMMAREA
                                       TO FRWKGMOD-COD-ERRO
                  MOVE CRURWIY1-COD-MENSAGEM
                                       OF DFHCOMMAREA
                                       TO FRWKGMOD-COD-MENSAGEM
                  MOVE '3427-BUSCAR-AREA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
                  PERFORM 9300-ERRO-MODULO
           END-EVALUATE
      *
           .
      *
      *----------------------------------------------------------------*
       3427-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR ACESSAR FUNCIONAL CRUR3C42.             *
      *    FORMATA O COD-EMPNMNT                                       *
      *----------------------------------------------------------------*
       3520-ACESSAR-CRUR3C42           SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
               PROGRAM (WRK-CRUR3C42)
               COMMAREA (WRK-AREA-CRUR3C42)
               LENGTH (LENGTH OF WRK-AREA-CRUR3C42)
               NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '3520-ACESSAR-CRUR3C42'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
      *
           IF (CRURWI42-COD-RETORNO    NOT EQUAL ZEROS)
               MOVE WRK-CRUR3C42       TO FRWKGMOD-NOME-MODULO
               MOVE CRURWI42-BLOCO-RETORNO
                                       TO FRWKGMOD-BLOCO-RETORNO
                                          CRURWIY1-BLOCO-RETORNO
               MOVE '3520-ACESSAR-CRUR3C42'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3612-FORMATA-PARCELAS    SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 'N'                    TO WRK-CODE100-B0LZ
           MOVE ZEROS                  TO WRK-VLCTO-PCELA-ACUM
      *
           PERFORM 3615-OPEN-CSR04-CRURB0LZ
      *
           PERFORM 3620-FETCH-CSR04-CRURB0LZ
      *
           PERFORM UNTIL (WRK-CODE100-B0LZ
                                       EQUAL 'S')
                   PERFORM 3620-FETCH-CSR04-CRURB0LZ
           END-PERFORM
      *
           PERFORM 3625-FECHAR-CSR04-CRURB0LZ
           .
      *
      *----------------------------------------------------------------*
       3612-99-FIM.                    EXIT.
      *-------------------------------------------------------
      *
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DO CURSOR CSR04-CRURB0LZ
      *----------------------------------------------------------------*
       3615-OPEN-CSR04-CRURB0LZ        SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURWIY1-E-CPSSOA-JURID-CONTR
                                       TO CPSSOA-JURID-CONTR
                                       OF CRURB0LZ
           MOVE CRURWIY1-E-CTPO-CONTR-NEGOC
                                       TO CTPO-CONTR-NEGOC
                                       OF CRURB0LZ
           MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
                                       TO NSEQ-CONTR-NEGOC
                                       OF CRURB0LZ
      *
           EXEC SQL
               OPEN CSR04-CRURB0LZ
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-OPEN            TO TRUE
               MOVE '0078'             TO FRWKGDB2-LOCAL
               MOVE 'TLCTO_PCELA_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3615-OPEN-CSR04-CRURB0LZ'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3615-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *                                                                *
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FETCH CSR04-CRURB0LZ
      *----------------------------------------------------------------*
       3620-FETCH-CSR04-CRURB0LZ       SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
             FETCH CSR04-CRURB0LZ  INTO
                   :CRURB0LZ.CPSSOA-JURID-CONTR,
                   :CRURB0LZ.CTPO-CONTR-NEGOC,
                   :CRURB0LZ.NSEQ-CONTR-NEGOC,
                   :CRURB0LZ.NPCELA-CONTR-RURAL,
                   :CRURB0LZ.CLCTO-CONTR-RURAL,
                   :CRURB0LZ.VLCTO-PCELA-EMPTO
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-FETCH           TO TRUE
               MOVE '0079'             TO FRWKGDB2-LOCAL
               MOVE 'TLCTO_PCELA_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3620-FETCH-CSR04-CRURB0LZ'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF (SQLCODE                 EQUAL +100)
              MOVE 'S'   TO  WRK-CODE100-B0LZ
           ELSE
              COMPUTE WRK-VLCTO-PCELA-ACUM
                                       = (WRK-VLCTO-PCELA-ACUM
                                       + VLCTO-PCELA-EMPTO
                                       OF CRURB0LZ)
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      * ROTINA DE FECHAMENTO DO CURSOR CSR04-CRURB0LZ
      *----------------------------------------------------------------*
       3625-FECHAR-CSR04-CRURB0LZ      SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               CLOSE CSR04-CRURB0LZ
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '0080'             TO FRWKGDB2-LOCAL
               MOVE 'TLCTO_PCELA_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3625-FECHAR-CSR04-CRURB0LZ'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3625-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DO CURSOR CSR05-CRURB0C4
      *----------------------------------------------------------------*
       3715-OPEN-CSR05-CRURB0C4        SECTION.
      *----------------------------------------------------------------*
      *
      *   -MOVE CIMOV-CREDT-RURAL      OF CRURB0L4
STE005*    *OVE CRURW07V-CIMOV-CREDT-RURAL
STE005     MOVE CRURW27V-CIMOV-CREDT-RURAL
                                       TO CIMOV-CREDT-RURAL
                                       OF CRURB0C4
      *
           EXEC SQL
               OPEN CSR05-CRURB0C4
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-OPEN            TO TRUE
               MOVE '0088'             TO FRWKGDB2-LOCAL
               MOVE 'TPPRIE_CREDT_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3715-OPEN-CSR05-CRURB0C4'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3715-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FETCH CSR05-CRURB0C4
      *----------------------------------------------------------------*
       3720-FETCH-CSR05-CRURB0C4       SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
             FETCH  CSR05-CRURB0C4
              INTO :CRURB0C4.CPSSOA-PPRIE-IMOV
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-FETCH           TO TRUE
               MOVE '0089'             TO FRWKGDB2-LOCAL
               MOVE 'TPPRIE_CREDT_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3720-FETCH-CSR05-CRURB0C4'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF (SQLCODE                 EQUAL +100)
              MOVE 'S'                 TO  WRK-CODE100-B0C4
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      * ROTINA DE FECHAMENTO DO CURSOR CSR05-CRURB0C4
      *----------------------------------------------------------------*
       3725-FECHAR-CSR05-CRURB0C4      SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               CLOSE CSR05-CRURB0C4
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '0080'             TO FRWKGDB2-LOCAL
               MOVE 'TPPRIE_CREDT_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '3725-FECHAR-CSR05-CRURB0C4'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       3725-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    MONTA COR0001- PROPRIETARIO
      *----------------------------------------------------------------*
       3726-MONTA-COR0001-PROPR        SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE WRK-AREA-PSDC312C
      *
BPO        MOVE 2269651
                                       TO PSDCW12I-E-CEMPR
           MOVE CPSSOA-PPRIE-IMOV      OF CRURB0C4
                                       TO WRK-COM-SINAL-10
           MOVE WRK-SEM-SINAL-10       TO PSDCW12I-E-CD-CLUB
           MOVE PSDCWBSI-S-USUARIO-NUM TO PSDCW12I-E-CD-FUNC-BRADESCO
           MOVE 'AWB00001'             TO PSDCW12I-E-CD-TERMINAL
           MOVE CRURWIY1-E-DEPENDENCIA-OPER
                                       TO PSDCW12I-E-DEPTO
      *
           PERFORM 5300-ACESSAR-PSDC312C
           PERFORM 3727-VERIFICA-PROPRIETARIO
      *
ID6695     IF WRK-GRAVA-PROPIE         EQUAL 'S'
ID6695        MOVE PSDCW12I-S-CCPF-CNPJ
ID6695                                 TO WRK-CCPF-CNPJ(WRK-IND-P01)
ID6695        MOVE PSDCW12I-S-CCTRL-CPF-CNPJ
ID6695                                 TO WRK-CCTRL(WRK-IND-P01)
ID6695        MOVE PSDCW12I-S-CD-FILIAL-CNPJ
ID6695                                 TO WRK-FILIAL(WRK-IND-P01)
ID6695     END-IF
      *
ID6695     IF  PSDCW12I-S-CD-IDENT-PESSOA
ID6695                                 EQUAL 'F'
ID6695         MOVE PSDCW12I-S-CCPF-CNPJ
ID6695                                 TO WRK-EDIT-NCPF
ID6695         MOVE PSDCW12I-S-CCTRL-CPF-CNPJ
ID6695                                 TO WRK-EDIT-CCPF
ID6695     ELSE
ID6695       IF  PSDCW12I-S-CD-IDENT-PESSOA
ID6695                                 EQUAL 'J'
ID6695           MOVE PSDCW12I-S-CCPF-CNPJ
ID6695                                 TO WRK-COD9-NUM
ID6695           MOVE WRK-COD9-NUM8-X  TO WRK-EDIT-NCNPJ
ID6695           MOVE SPACES           TO WRK-EDIT-CCNPJ
ID6695       END-IF
ID6695     END-IF
      *
ID6695     IF  CNIRF-IMOV-RURAL   OF CRURB0B1
ID6695                                 NOT EQUAL SPACES
ID6695         MOVE ZEROS              TO WRK-IND-P02
ID6695         PERFORM 3730-MONTA-NIRF
ID6695     END-IF.
ID6695
ID6695     IF  CCCIR              OF CRURB0B1
ID6695                                 NOT EQUAL SPACES
ID6695         MOVE ZEROS              TO WRK-IND-P02
ID6695         PERFORM 3731-MONTA-CCIR
ID6695     END-IF.
ID6695
ID6699     IF  CREG-AMBTL-RURAL   OF CRURB0B1
ID6695                                 NOT EQUAL SPACES
ID6695         MOVE ZEROS              TO WRK-IND-P02
ID6695         PERFORM 3732-MONTA-CAR
ID6695     END-IF.
      *
BR0621     IF  COUTGA-AGUA-IMOV   OF CRURB0B1
BR0621                                 NOT EQUAL SPACES
BR0621         MOVE ZEROS              TO WRK-IND-P02
BR0621         PERFORM 3736-MONTA-OUTGA
BR0621     END-IF.
      *
BR0621     IF  PPRESV-AMBTL-PPRIE OF CRURB0B1
BR0621                                 NOT EQUAL ZEROS
BR0621         MOVE ZEROS              TO WRK-IND-P02
BR0621         PERFORM 3737-MONTA-PERC-PRESRV
BR0621     END-IF.
      *
CAR        PERFORM 3728-VERIFICA-EXIG-CAR
           .
      *
      *----------------------------------------------------------------*
       3726-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
     *
      *---------------------------------------------------------------*
       3727-VERIFICA-PROPRIETARIO  SECTION.
      *---------------------------------------------------------------*
      *
           MOVE 'S'                TO WRK-GRAVA-PROPIE
      *
ID6695     PERFORM VARYING WRK-IND11   FROM 1 BY 1
ID6695               UNTIL WRK-IND11   GREATER 030
ID6695        IF   PSDCW12I-S-CCPF-CNPJ
ID6695                                 EQUAL WRK-CCPF-CNPJ(WRK-IND11)
ID6695         AND PSDCW12I-S-CCTRL-CPF-CNPJ
ID6695                                 EQUAL WRK-CCTRL(WRK-IND11)
ID6695         AND PSDCW12I-S-CD-FILIAL-CNPJ
ID6695                                 EQUAL WRK-FILIAL(WRK-IND11)
ID6695             MOVE 'N'            TO WRK-GRAVA-PROPIE
ID6695             MOVE WRK-IND11      TO WRK-IND-P01
ID6695             MOVE 999            TO WRK-IND11
ID6695        ELSE
ID6695          IF   WRK-CCPF-CNPJ(WRK-IND11)
ID6695                                 EQUAL ZEROS
ID6695           AND WRK-CCTRL(WRK-IND11)
ID6695                                 EQUAL ZEROS
ID6695           AND WRK-FILIAL(WRK-IND11)
ID6695                                 EQUAL ZEROS
ID6695             MOVE 'S'            TO WRK-GRAVA-PROPIE
ID6695             MOVE WRK-IND11      TO WRK-IND-P01
ID6695             MOVE 999            TO WRK-IND11
ID6695          END-IF
ID6695        END-IF
ID6695     END-PERFORM
           .
      *
      *----------------------------------------------------------------*
       3727-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
CAR   *
CAR   *---------------------------------------------------------------*
CAR    3728-VERIFICA-EXIG-CAR      SECTION.
CAR   *---------------------------------------------------------------*
CAR   *
CAR        PERFORM 3729-ACESSA-CRURB0L5
CAR   *
RO1603***  NOTICIAS SICOR 09/03/2021
RO1603***  VERIFICAR SE O EMPREENDIMENTO COM MODALIDADE 60 EH
RO1603***  EXCECAO E NAO DEVE SOLICITAR O CAR
RO1603     PERFORM 3740-VERIFICA-EXCECAO-CAR
RO1603     IF  WRK-COUNT NOT EQUAL ZEROS
RO1603         GO TO 3728-99-FIM
RO1603     END-IF
*
CAR        IF(CTPO-BNEFC-BACEN OF CRURB0L5 EQUAL '001' OR '003' OR '006'
CAR                                           OR '013' OR '016') AND
CAR          ((CRURY1-COD-EMPNMNT          EQUAL '13130360000002'
CAR                                           OR '13130880110002'
CAR                                           OR '13132740000002'
CAR                                           OR '13133160172002'
CAR                                           OR '13133200006002'
CAR                                           OR '13133340000002'
CAR                                           OR '13134020109002'
CAR                                           OR '13136050352042'
CAR                                           OR '13136240217002'
CAR                                           OR '13137705000408') OR
CAR           (CRURY1-COD-EMPNMNT(1:4)        EQUAL '1201' OR '1211'
CAR                                              OR '1311' OR '1312') OR
CAR           (CRURY1-COD-EMPNMNT(1:2)        EQUAL '22'))
CAR            IF CREG-AMBTL-RURAL OF CRURB0B1 EQUAL SPACES
CAR               MOVE 16              TO CRURWIY1-COD-RETORNO
CAR               MOVE '3728'          TO CRURWIY1-COD-ERRO
CAR               MOVE 'CRUR2107'      TO CRURWIY1-COD-MENSAGEM
CAR               PERFORM 3000-FINALIZAR
CAR            END-IF
CAR        END-IF
CAR        .
CAR   *
CAR   *----------------------------------------------------------------*
CAR    3728-99-FIM.                    EXIT.
CAR   *----------------------------------------------------------------*
CAR   *
CAR   *---------------------------------------------------------------*
CAR    3729-ACESSA-CRURB0L5        SECTION.
CAR   *---------------------------------------------------------------*
CAR   *
CAR        MOVE CRURW03V-CTPO-BNEFC-RURAL
CAR                                    TO CTPO-BNEFC-RURAL OF CRURB0L5.
CAR   *
CAR        EXEC SQL
CAR          SELECT CTPO_BNEFC_BACEN
CAR          INTO   :CRURB0L5.CTPO-BNEFC-BACEN
CAR          FROM   DB2PRD.TBNEFC_CREDT_RURAL
CAR          WHERE  CTPO_BNEFC_RURAL   = :CRURB0L5.CTPO-BNEFC-RURAL
CAR        END-EXEC.
CAR   *
CAR        IF ((SQLCODE            NOT EQUAL ZEROS) OR
CAR            (SQLWARN0               EQUAL 'W'))
CAR            SET  DB2-SELECT         TO TRUE
CAR            MOVE 'TBNEFC_CREDT_RURAL'
CAR                                    TO FRWKGDB2-NOME-TABELA
CAR            MOVE '3729-VERIFICA-EXIG-CAR'
CAR                                    TO FRWKGHEA-IDEN-PARAGRAFO
CAR            MOVE '3729'             TO FRWKGDB2-LOCAL
CAR            PERFORM 9000-ERRO-DB2
CAR        END-IF
CAR        .
CAR   *
CAR   *----------------------------------------------------------------*
CAR    3729-99-FIM.                    EXIT.
CAR   *----------------------------------------------------------------*
CAR   *
ID6695*---------------------------------------------------------------*
ID6695 3730-MONTA-NIRF                  SECTION.
ID6695*---------------------------------------------------------------*
ID6695*
ID6695     PERFORM VARYING WRK-IND-P02 FROM 1 BY 1
ID6695             UNTIL   WRK-IND-P02 GREATER 50
ID6695       IF WRK-NIRF(WRK-IND-P01 WRK-IND-P02)
ID6695                                 EQUAL SPACES
ID6695          MOVE PSDCW12I-S-CD-IDENT-PESSOA
ID6695               TO WRK-TP-PESSOA-PROPT(WRK-IND-P01 WRK-IND-P02)
ID6695          MOVE WRK-EDIT-CPF
ID6695               TO WRK-CPFCNPJ-PROPT(WRK-IND-P01 WRK-IND-P02)
STE003          MOVE CNIRF-IMOV-RURAL  OF CRURB0B1(2:8)
ID6695               TO WRK-NIRF(WRK-IND-P01 WRK-IND-P02)
ID6695          MOVE 100               TO WRK-IND-P02
ID6695        END-IF
ID6695     END-PERFORM
ID6695     .
ID6695*
ID6695*----------------------------------------------------------------*
ID6695 3730-99-FIM.                    EXIT.
ID6695*----------------------------------------------------------------*
ID6695*
ID6695*---------------------------------------------------------------*
ID6695 3731-MONTA-CCIR                  SECTION.
ID6695*---------------------------------------------------------------*
ID6695*
ID6695     PERFORM VARYING WRK-IND-P02 FROM 1 BY 1
ID6695             UNTIL   WRK-IND-P02 GREATER 50
ID6695       IF WRK-COD-SNCR(WRK-IND-P01 WRK-IND-P02)
ID6695                                 EQUAL SPACES
ID6695          MOVE PSDCW12I-S-CD-IDENT-PESSOA
ID6695               TO WRK-TP-PESSOA-PROPT(WRK-IND-P01 WRK-IND-P02)
ID6695          MOVE WRK-EDIT-CPF
ID6695               TO WRK-CPFCNPJ-PROPT(WRK-IND-P01 WRK-IND-P02)
ID6695          MOVE CCCIR             OF CRURB0B1
ID6695               TO WRK-COD-SNCR(WRK-IND-P01 WRK-IND-P02)
ID6695          MOVE 100               TO WRK-IND-P02
ID6695        END-IF
ID6695     END-PERFORM
ID6695     .
ID6695*
ID6695*----------------------------------------------------------------*
ID6695 3731-99-FIM.                    EXIT.
ID6695*----------------------------------------------------------------*
ID6695*
ID6695*---------------------------------------------------------------*
ID6695 3732-MONTA-CAR                   SECTION.
ID6695*---------------------------------------------------------------*
ID6695*
ID6695     PERFORM VARYING WRK-IND-P02 FROM 1 BY 1
ID6695             UNTIL   WRK-IND-P02 GREATER 50
ID6695       IF WRK-CREG-AMBTL-RURAL(WRK-IND-P01 WRK-IND-P02)
ID6695                                 EQUAL SPACES
ID6695          MOVE PSDCW12I-S-CD-IDENT-PESSOA
ID6695               TO WRK-TP-PESSOA-PROPT(WRK-IND-P01 WRK-IND-P02)
ID6695          MOVE WRK-EDIT-CPF
ID6695               TO WRK-CPFCNPJ-PROPT(WRK-IND-P01 WRK-IND-P02)
ID6695          MOVE CREG-AMBTL-RURAL   OF CRURB0B1
ID6695               TO WRK-CREG-AMBTL-RURAL(WRK-IND-P01 WRK-IND-P02)
ID6695          MOVE 100               TO WRK-IND-P02
ID6695       END-IF
ID6695     END-PERFORM
ID6695     .
ID6695*
ID6695*----------------------------------------------------------------*
ID6695 3732-99-FIM.                    EXIT.
ID6695*----------------------------------------------------------------*
ID6695*
ID6695*---------------------------------------------------------------*
ID6695 3733-COR0001-PROP               SECTION.
ID6695*---------------------------------------------------------------*
ID6695*
ID6699     MOVE  01                    TO WRK-IND-CAR
ID6699     PERFORM 3734-LIMPA-CAR
ID6699         UNTIL WRK-IND-CAR       GREATER 50
ID6695*
ID6695     PERFORM VARYING WRK-IND-P02 FROM 1 BY 1
ID6695             UNTIL   WRK-IND-P02 GREATER 50
ID6695       IF WRK-TP-PESSOA-PROPT(WRK-IND-P01 WRK-IND-P02)
ID6695                                 EQUAL SPACES
ID6695          MOVE 100               TO WRK-IND-P02
ID6695       ELSE
ID6695          MOVE SPACES            TO CRURY1-GP-COR0001-PROP-F
ID6695          MOVE WRK-TP-PESSOA-PROPT(WRK-IND-P01 WRK-IND-P02)
ID6695                                 TO CRURY1-TP-PESSOA-PROPT-F
ID6695          MOVE WRK-CPFCNPJ-PROPT(WRK-IND-P01 WRK-IND-P02)
ID6695                                 TO CRURY1-CPFPROPT
ID6695          IF WRK-NIRF(WRK-IND-P01 WRK-IND-P02)
ID6695                                 EQUAL SPACES
7C5.10*------------MOVE-ALL '*'        TO-CRURY1-NIRF-F
ID6695             MOVE ALL '*'        TO CRURY1-CIB-F
ID6695          ELSE
STE004             MOVE WRK-NIRF(WRK-IND-P01 WRK-IND-P02)
7C5.10*--------------------------------TO-CRURY1-NIRF-F
ID6695                                 TO CRURY1-CIB-F
ID6695          END-IF
ID6695          IF WRK-COD-SNCR(WRK-IND-P01 WRK-IND-P02)
ID6695                                 EQUAL SPACES
ID6695             MOVE ALL '*'        TO CRURY1-COD-SNCR-F
ID6695          ELSE
ID6695             MOVE WRK-COD-SNCR(WRK-IND-P01 WRK-IND-P02)
ID6695                                 TO CRURY1-COD-SNCR-F
ID6695          END-IF
ID6695          IF WRK-CREG-AMBTL-RURAL(WRK-IND-P01 WRK-IND-P02)
ID6695                                 EQUAL SPACES
ID6695             MOVE ALL '*'        TO CRURY1-CREG-AMBTL-RURAL-F
ID6695          ELSE
ID6695             MOVE WRK-CREG-AMBTL-RURAL(WRK-IND-P01 WRK-IND-P02)
ID6695                                 TO CRURY1-CREG-AMBTL-RURAL-F
ID6695          END-IF
BR0521***     M.MOVE ALL '*'           TO CRURY1-COUTGA-AGUA-IMOV-F
BR0521***     M.MOVE ALL '*'           TO CRURY1-PPRESV-AMBTL-PPRIE-F-X
BR0621          IF WRK-COUTGA-AGUA-IMOV(WRK-IND-P01 WRK-IND-P02)
BR0621                                 EQUAL SPACES
BR0621             MOVE ALL '*'        TO CRURY1-COUTGA-AGUA-IMOV-F
BR0621          ELSE
BR0621             MOVE WRK-COUTGA-AGUA-IMOV(WRK-IND-P01 WRK-IND-P02)
BR0621                                 TO CRURY1-COUTGA-AGUA-IMOV-F
BR0621          END-IF
BR0621          IF WRK-PPRESV-AMBTL-PPRIE(WRK-IND-P01 WRK-IND-P02)
BR0621                                 EQUAL ZEROS
BR0621                                    OR 999,99
BR0621             MOVE ALL '*'        TO CRURY1-PPRESV-AMBTL-PPRIE-F-X
BR0621          ELSE
BR0621             MOVE WRK-PPRESV-AMBTL-PPRIE(WRK-IND-P01 WRK-IND-P02)
BR0621                                 TO CRURY1-PPRESV-AMBTL-PPRIE-F
BR0621          END-IF
ID6695
ID6699          MOVE 'S'               TO WRK-GRAVA-CAR
ID6699          MOVE 01                TO WRK-IND-CAR
ID6699          PERFORM 3735-VERIF-CAR
ID6699              UNTIL WRK-IND-CAR  GREATER 50
ID6699
ID6699          IF  WRK-GRAVA-CAR      EQUAL 'S'
ID6699              MOVE CRURY1-COR0001-10-F
ID6699                                 TO WRK-B09A-SIS(WRK-POSICAO:
ID6699                                    LENGTH OF CRURY1-COR0001-10-F)
ID6699              ADD  LENGTH OF CRURY1-COR0001-10-F TO WRK-TAM
ID6699              ADD  LENGTH OF CRURY1-COR0001-10-F TO WRK-POSICAO
ID6699              ADD  1             TO WRK-IND4
ID6699              ADD  1             TO WRK-IND2
ID6699          END-IF
ID6695       END-IF
ID6695     END-PERFORM.
ID6695
ID6695     ADD  01                     TO WRK-IND-P01
ID6695     .
ID6695*
ID6695*----------------------------------------------------------------*
ID6695 3733-99-FIM.                    EXIT.
ID6695*----------------------------------------------------------------*
ID6699*
ID6699*---------------------------------------------------------------*
ID6699 3734-LIMPA-CAR                  SECTION.
ID6699*---------------------------------------------------------------*
ID6699*
ID6699     MOVE SPACES               TO WRK-GRV-NIRF(WRK-IND-CAR)
ID6699                                  WRK-GRV-SNCR(WRK-IND-CAR)
ID6699                                  WRK-GRV-CREG-AMBTL(WRK-IND-CAR)
BR0621                                  WRK-GRV-COUTGA-AGUA(WRK-IND-CAR)
BR0621                               WRK-GRV-PPRESV-AMBTL-X(WRK-IND-CAR)
BR0621     MOVE ZEROS             TO WRK-GRV-PPRESV-AMBTL(WRK-IND-CAR)
ID6699
ID6699     ADD   01                  TO WRK-IND-CAR
ID6699     .
ID6699*
ID6699*----------------------------------------------------------------*
ID6699 3734-99-FIM.                    EXIT.
ID6699*----------------------------------------------------------------*
ID6699*
ID6699*----------------------------------------------------------------*
ID6699 3735-VERIF-CAR                  SECTION.
ID6699*----------------------------------------------------------------*
ID6699*
ID6699     IF WRK-GRV-NIRF(WRK-IND-CAR)
ID6699                                EQUAL SPACES
7C5.10*--------MOVE-CRURY1-NIRF-F     TO-WRK-GRV-NIRF(WRK-IND-CAR)
7C5.10         MOVE CRURY1-CIB-F      TO WRK-GRV-NIRF(WRK-IND-CAR)
ID6699         MOVE CRURY1-COD-SNCR-F
ID6699                                TO WRK-GRV-SNCR(WRK-IND-CAR)
ID6699         MOVE CRURY1-CREG-AMBTL-RURAL-F
ID6699                                TO WRK-GRV-CREG-AMBTL(WRK-IND-CAR)
BR0621         MOVE CRURY1-COUTGA-AGUA-IMOV-F
BR0621                              TO WRK-GRV-COUTGA-AGUA(WRK-IND-CAR)
BR0621         MOVE CRURY1-PPRESV-AMBTL-PPRIE-F
BR0621                              TO WRK-GRV-PPRESV-AMBTL(WRK-IND-CAR)
BR0621         MOVE CRURY1-PPRESV-AMBTL-PPRIE-F-X
BR0621                            TO WRK-GRV-PPRESV-AMBTL-X(WRK-IND-CAR)
ID6699         MOVE 50                TO WRK-IND-CAR
ID6699         MOVE 'S'               TO WRK-GRAVA-CAR
ID6699     ELSE
7C5.10*------IF---CRURY1-NIRF-F       EQUAL-WRK-GRV-NIRF(WRK-IND-CAR)
7C5.10       IF   CRURY1-CIB-F        EQUAL WRK-GRV-NIRF(WRK-IND-CAR)
ID6699        AND CRURY1-COD-SNCR-F   EQUAL WRK-GRV-SNCR(WRK-IND-CAR)
ID6699        AND CRURY1-CREG-AMBTL-RURAL-F EQUAL
ID6699                                WRK-GRV-CREG-AMBTL(WRK-IND-CAR)
BR0621        AND CRURY1-COUTGA-AGUA-IMOV-F EQUAL
BR0621                                WRK-GRV-COUTGA-AGUA(WRK-IND-CAR)
BR0621        AND (CRURY1-PPRESV-AMBTL-PPRIE-F EQUAL
BR0621                              WRK-GRV-PPRESV-AMBTL(WRK-IND-CAR)
BR0621        OR   CRURY1-PPRESV-AMBTL-PPRIE-F-X EQUAL
BR0621                              WRK-GRV-PPRESV-AMBTL-X(WRK-IND-CAR))
ID6699            MOVE 50             TO WRK-IND-CAR
ID6699            MOVE 'N'            TO WRK-GRAVA-CAR
ID6699       END-IF
ID6699     END-IF
ID6699
ID6699     ADD   01                  TO WRK-IND-CAR
ID6699     .
ID6699*
ID6699*----------------------------------------------------------------*
ID6699 3735-99-FIM.                    EXIT.
ID6699*----------------------------------------------------------------*
ID6699*
      *
BR0621*---------------------------------------------------------------*
BR0621 3736-MONTA-OUTGA                 SECTION.
BR0621*---------------------------------------------------------------*
BR0621*
BR0621     PERFORM VARYING WRK-IND-P02 FROM 1 BY 1
BR0621             UNTIL   WRK-IND-P02 GREATER 50
BR0621       IF WRK-COUTGA-AGUA-IMOV(WRK-IND-P01 WRK-IND-P02)
BR0621                                 EQUAL SPACES
BR0621          MOVE PSDCW12I-S-CD-IDENT-PESSOA
BR0621               TO WRK-TP-PESSOA-PROPT(WRK-IND-P01 WRK-IND-P02)
BR0621          MOVE WRK-EDIT-CPF
BR0621               TO WRK-CPFCNPJ-PROPT(WRK-IND-P01 WRK-IND-P02)
BR0621          MOVE COUTGA-AGUA-IMOV   OF CRURB0B1
BR0621               TO WRK-COUTGA-AGUA-IMOV(WRK-IND-P01 WRK-IND-P02)
BR0621          MOVE 100               TO WRK-IND-P02
BR0621       END-IF
BR0621     END-PERFORM
BR0621     .
BR0621*
BR0621*----------------------------------------------------------------*
BR0621 3736-99-FIM.                    EXIT.
BR0621*----------------------------------------------------------------*
      *
BR0621*---------------------------------------------------------------*
BR0621 3737-MONTA-PERC-PRESRV           SECTION.
BR0621*---------------------------------------------------------------*
BR0621*
BR0621     PERFORM VARYING WRK-IND-P02 FROM 1 BY 1
BR0621             UNTIL   WRK-IND-P02 GREATER 50
BR0621       IF WRK-PPRESV-AMBTL-PPRIE(WRK-IND-P01 WRK-IND-P02)
BR0621                                 EQUAL ZEROS
BR0621          MOVE PSDCW12I-S-CD-IDENT-PESSOA
BR0621               TO WRK-TP-PESSOA-PROPT(WRK-IND-P01 WRK-IND-P02)
BR0621          MOVE WRK-EDIT-CPF
BR0621               TO WRK-CPFCNPJ-PROPT(WRK-IND-P01 WRK-IND-P02)
BR0621
BR0621          IF PPRESV-AMBTL-PPRIE OF CRURB0B1 EQUAL 999,99
BR0621             MOVE PPRESV-AMBTL-PPRIE OF CRURB0B1
BR0621               TO WRK-PPRESV-AMBTL-PPRIE(WRK-IND-P01 WRK-IND-P02)
BR0621          ELSE
BR0621             IF DVALDD-OUTGA-AGUA  OF CRURB0B1 EQUAL SPACES
BR0621                                            OR '**********'
BR0621                MOVE 999,99
BR0621                TO WRK-PPRESV-AMBTL-PPRIE(WRK-IND-P01 WRK-IND-P02)
BR0621             ELSE
BR0621                MOVE DVALDD-OUTGA-AGUA  OF CRURB0B1 (7:4)
BR0621                  TO WRK-DVALDD (1:4)
BR0621                MOVE DVALDD-OUTGA-AGUA  OF CRURB0B1 (4:2)
BR0621                  TO WRK-DVALDD (5:2)
BR0621                MOVE DVALDD-OUTGA-AGUA  OF CRURB0B1 (1:2)
BR0621                  TO WRK-DVALDD (7:2)
BR0621                IF WRK-DATADIA-AMD-N NOT GREATER WRK-DVALDD-NUM
BR0621                   MOVE PPRESV-AMBTL-PPRIE OF CRURB0B1
BR0621                TO WRK-PPRESV-AMBTL-PPRIE(WRK-IND-P01 WRK-IND-P02)
BR0621                ELSE
BR0621                   MOVE 999,99
BR0621                TO WRK-PPRESV-AMBTL-PPRIE(WRK-IND-P01 WRK-IND-P02)
BR0621                END-IF
BR0621             END-IF
BR0621          END-IF
BR0621
BR0621          MOVE 100               TO WRK-IND-P02
BR0621       END-IF
BR0621     END-PERFORM
BR0621     .
BR0621*
BR0621*----------------------------------------------------------------*
BR0621 3737-99-FIM.                    EXIT.
BR0621*----------------------------------------------------------------*
      *
RO1603*----------------------------------------------------------------*
RO1603*    ROTINA PARA VERIFICAR A OBRIGATORIEDADE DO CAR PARA O IMOVEL*
RO1603*----------------------------------------------------------------*
RO1603 3740-VERIFICA-EXCECAO-CAR       SECTION.
RO1603*----------------------------------------------------------------*
RO1603*
RO1603     IF(CTPO-BNEFC-BACEN OF CRURB0L5 EQUAL '001' OR '003' OR '006'
RO1603                                        OR '013' OR '016') AND
RO1603       (CRURY1-COD-EMPNMNT(3:2)      EQUAL '60')
RO1603        PERFORM 3741-CONSISTIR-SIST-PROD
RO1603     END-IF
RO1603     .
RO1603*
RO1603*----------------------------------------------------------------*
RO1603 3740-99-FIM.                    EXIT.
RO1603*----------------------------------------------------------------*
*
RO1603*----------------------------------------------------------------*
RO1603 3741-CONSISTIR-SIST-PROD        SECTION.
RO1603*----------------------------------------------------------------*
RO1603*
RO1603     MOVE ZEROS                  TO WRK-COUNT
RO1603     MOVE CEMPTO-RURAL-BACEN     OF CRURB0MB
RO1603                                 TO CEMPTO-RURAL-BACEN
RO1603                                 OF CRURB0T2
RO1603*
RO1603     EXEC SQL
RO1603          SELECT  COUNT(*)
RO1603            INTO :WRK-COUNT
RO1603            FROM DB2PRD.TSIST_PROD_BACEN
RO1603           WHERE CEMPTO_RURAL_BACEN =
RO1603                :CRURB0T2.CEMPTO-RURAL-BACEN
RO1603             AND CCULTR_RURAL_BACEN IN (18, 21)
RO1603            WITH  UR
RO1603     END-EXEC
RO1603
RO1603     IF ((SQLCODE            NOT EQUAL ZEROS AND +100) OR
RO1603         (SQLWARN0               EQUAL 'W'))
RO1603         SET  DB2-SELECT         TO TRUE
RO1603         MOVE 'TSIST_PROD_BACEN'
RO1603                                 TO FRWKGDB2-NOME-TABELA
RO1603         MOVE '3741-CONSISTIR-EXCECAO-CAR'
RO1603                                 TO FRWKGHEA-IDEN-PARAGRAFO
RO1603         MOVE '3741'             TO FRWKGDB2-LOCAL
RO1603         PERFORM 9000-ERRO-DB2
RO1603     END-IF
RO1603
RO1603     IF (SQLCODE                EQUAL +100 OR -305)
RO1603         MOVE ZEROS             TO WRK-COUNT
RO1603     END-IF
RO1603     .
RO1603*
RO1603*----------------------------------------------------------------*
RO1603 3741-99-FIM.                    EXIT.
RO1603*----------------------------------------------------------------*
      *
BR0521*---------------------------------------------------------------*
BR0521 3800-FORMATA-COD-STN            SECTION.
BR0521*---------------------------------------------------------------*
BR0521
BR0521     MOVE ALL '*'                TO CRURY1-COD-STN-EQUALIZ.
BR0521
BR0521     MOVE CRURY1-COR0001-17      TO WRK-B09A-SIS(WRK-POSICAO:
BR0521                                 LENGTH OF CRURY1-COR0001-17)
BR0521     ADD LENGTH OF CRURY1-COR0001-17 TO WRK-TAM
BR0521     ADD LENGTH OF CRURY1-COR0001-17 TO WRK-POSICAO
BR0521     .
BR0521
BR0521*----------------------------------------------------------------*
BR0521 3800-99-FIM.                    EXIT.
BR0521*----------------------------------------------------------------*
      *
BR0521*---------------------------------------------------------------*
BR0521 3810-FORMATA-CERTF-ORGNCA       SECTION.
BR0521*---------------------------------------------------------------*
BR0521
BR0621     IF CCETFC-ORGNC-EMPTO     OF CRURB0MB NOT EQUAL SPACES
BR0621        MOVE CCETFC-ORGNC-EMPTO
BR0621                               OF CRURB0MB
BR0621                               TO CRURY1-CCETFC-ORGNC-EMPTO
BR0621        MOVE EURL-CETFC-ORGNC-TEXT
BR0621                               OF CRURB0MB(1:EURL-CETFC-ORGNC-LEN)
BR0621                               TO CRURY1-EURL-CETFC-ORGNC
BR0621     ELSE
BR0621        MOVE ALL '*'           TO CRURY1-GP-COR0001-CERTF-ORGNCA
BR0621     END-IF.
BR0521
BR0521     MOVE CRURY1-COR0001-18      TO WRK-B09A-SIS(WRK-POSICAO:
BR0521                                 LENGTH OF CRURY1-COR0001-18)
BR0521     ADD LENGTH OF CRURY1-COR0001-18 TO WRK-TAM
BR0521     ADD LENGTH OF CRURY1-COR0001-18 TO WRK-POSICAO
BR0521     .
BR0521
BR0521*----------------------------------------------------------------*
BR0521 3810-99-FIM.                    EXIT.
BR0521*----------------------------------------------------------------*
      *
BR0521*---------------------------------------------------------------*
BR0521 3820-FORMATA-RASTLDD-EMP        SECTION.
BR0521*---------------------------------------------------------------*
BR0521
BR0621     IF CRSTRB-EMPTO-RURAL     OF CRURB0MB NOT EQUAL SPACES
BR0621        MOVE CRSTRB-EMPTO-RURAL
BR0621                               OF CRURB0MB
BR0621                               TO CRURY1-CRSTRB-EMPTO
BR0621
BR0621        MOVE EURL-RSTRB-EMPTO-TEXT
BR0621                               OF CRURB0MB(1:EURL-RSTRB-EMPTO-LEN)
BR0621                               TO CRURY1-EURL-RSTRB
BR0621     ELSE
BR0621        MOVE ALL '*'           TO CRURY1-GP-COR0001-RASTLDD-EMP
BR0621     END-IF.
BR0521
BR0521     MOVE CRURY1-COR0001-19      TO WRK-B09A-SIS(WRK-POSICAO:
BR0521                                 LENGTH OF CRURY1-COR0001-19)
BR0521     ADD LENGTH OF CRURY1-COR0001-19 TO WRK-TAM
BR0521     ADD LENGTH OF CRURY1-COR0001-19 TO WRK-POSICAO
BR0521     .
BR0521
BR0521*----------------------------------------------------------------*
BR0521 3820-99-FIM.                    EXIT.
BR0521*----------------------------------------------------------------*
      *
BR0521*---------------------------------------------------------------*
BR0521 3830-FORMATA-PENERG-RENOV       SECTION.
BR0521*---------------------------------------------------------------*
BR0521
BR0621     IF PENERG-RENOV-EMPTO       OF CRURB0MB GREATER ZEROS
BR0621        MOVE PENERG-RENOV-EMPTO  OF CRURB0MB
BR0621                                 TO CRURY1-PENERG-RENOV
BR0621     ELSE
BR0621        MOVE ALL '*'             TO CRURY1-PENERG-RENOV-X
BR0621     END-IF.
BR0521
BR0521     MOVE CRURY1-COR0001-20      TO WRK-B09A-SIS(WRK-POSICAO:
BR0521                                 LENGTH OF CRURY1-COR0001-20)
BR0521     ADD LENGTH OF CRURY1-COR0001-20 TO WRK-TAM
BR0521     ADD LENGTH OF CRURY1-COR0001-20 TO WRK-POSICAO
BR0521     .
BR0521
BR0521*----------------------------------------------------------------*
BR0521 3830-99-FIM.                    EXIT.
BR0521*----------------------------------------------------------------*
      *
WP1021*---------------------------------------------------------------*
WP1021 3840-FORMATA-SOLO-CULTIVO       SECTION.
WP1021*---------------------------------------------------------------*
WP1021
WP1021***  SE EMPREENDIMENTO NAO ZONEADO - NAO ENVIAR ZARC NA COR0001
WP1021
WP1021     IF  CRURY1-COD-EMPNMNT(14:1)  EQUAL   '1'
WP1021         CONTINUE
WP1021     ELSE
WP1021         MOVE  ZEROS               TO      WRK-CCICLO-CTVDA
WP1021                                           WRK-CMANUT-SOLO
WP1021     END-IF.
WP1021
WP1021     IF (WRK-CCICLO-CTVDA        GREATER   ZEROS)  AND
WP1021        (WRK-CMANUT-SOLO         GREATER   ZEROS)
WP1021         MOVE  WRK-CTPO-SOLO-AGCLT-X
WP1021                                 TO        CRURY1-TIPO-SOLO
WP1021         MOVE  WRK-CCICLO-CTVDA-X
WP1021                                 TO        CRURY1-CICLO-CULTURA
WP1021         MOVE  WRK-CMANUT-SOLO-X
WP1021                                 TO        CRURY1-TIPO-MANEJO
WP1021     ELSE
WP1021         MOVE  ALL '*'           TO        CRURY1-COR0001-21
WP1021     END-IF.
WP1021
WP1021     MOVE CRURY1-COR0001-21      TO WRK-B09A-SIS(WRK-POSICAO:
WP1021                                 LENGTH OF CRURY1-COR0001-21)
WP1021     ADD LENGTH OF CRURY1-COR0001-21 TO WRK-TAM
WP1021     ADD LENGTH OF CRURY1-COR0001-21 TO WRK-POSICAO
WP1021     .
WP1021
WP1021*----------------------------------------------------------------*
WP1021 3840-99-FIM.                    EXIT.
WP1021*----------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
       4100-ENVIO-RCOR7000         SECTION.
      *---------------------------------------------------------------*
      *
           INITIALIZE  RCOR10-REGISTRO.
      *
           MOVE 'CRUR'                 TO RCOR10-CCUSTO.
EG1117
EG1117     IF  WRK-CONTINUA            EQUAL 'S'
EG1117         MOVE 'S'                TO RCOR10-IND-CONTINUA
EG1117     ELSE
               MOVE 'N'                TO RCOR10-IND-CONTINUA
EG1117     END-IF.
      *
ID6260     IF  WRK-SEQ-ENVIO-RCOR      EQUAL ZEROS
ID6260         MOVE 1                  TO WRK-SEQ-ENVIO-RCOR
ID6260                                    RCOR10-SEQ-MSG
ID6260         MOVE SPACES             TO RCOR10-NRO-CTRLIF
ID6260     ELSE
ID6260         ADD  1                  TO WRK-SEQ-ENVIO-RCOR
ID6260         MOVE WRK-SEQ-ENVIO-RCOR TO RCOR10-SEQ-MSG
ID6260         MOVE WRK-ENVIO-NUMCTRLIF   TO RCOR10-NRO-CTRLIF
ID6260     END-IF.
      *
           MOVE WRK-TAM                TO WRK-TAMANHO-SISMSG.
      *
           MOVE WRK-TAMANHO-SISMSG     TO RCOR10-TAM-SISMSG.
      *
      *
           MOVE WRK-B09A-SIS(1: LENGTH OF RCOR10-SISMSG)
                                       TO RCOR10-SISMSG.
      *
           MOVE WRK-TAMANHO-SISMSG     TO WRK-ENVIO-LEN.
      *
           MOVE WRK-B09A-SIS(1: LENGTH OF WRK-ENVIO-SIS)
                                       TO WRK-ENVIO-SIS.
      *
           EXEC CICS LINK
                PROGRAM  (WRK-RCOR7000)
                COMMAREA (RCOR10-REGISTRO)
                LENGTH   (LENGTH OF RCOR10-REGISTRO)
                NOHANDLE
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '4100-ENVIO-RCOR7000'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
           IF (RCOR10-COD-RETORNO NOT EQUAL ZEROS)
               MOVE WRK-RCOR7000       TO FRWKGMOD-NOME-MODULO
               MOVE RCOR10-COD-RETORNO TO FRWKGMOD-COD-RETORNO
                                          CRURWIY1-COD-RETORNO
               MOVE '4100-ENVIO-RCOR7000'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-IF
      *
           MOVE RCOR10-NRO-CTRL-IF     TO WRK-ENVIO-NUMCTRLIF
ID6854                                    WRK-ENVIO-NUMCTRLIF-AUX
           .
      *
      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
7C0120*---------------------------------------------------------------*
.      4110-TRATA-RCOR7000         SECTION.
.     *---------------------------------------------------------------*
.     *
.          INITIALIZE  RCOR10-REGISTRO.
.     *
.          MOVE WRK-TAM                TO WRK-TAMANHO-SISMSG.
.     *
.          MOVE WRK-TAMANHO-SISMSG     TO RCOR10-TAM-SISMSG.
.     *
.     *
.          MOVE WRK-B09A-SIS(1: LENGTH OF RCOR10-SISMSG)
.                                      TO RCOR10-SISMSG.
.     *
.          MOVE 'CRUR'                 TO RCOR10-CCUSTO.
.
.          IF  WRK-CONTINUA            EQUAL 'S'
.              MOVE 'S'                TO RCOR10-IND-CONTINUA
.          ELSE
.              MOVE 'N'                TO RCOR10-IND-CONTINUA
.          END-IF.
.     *
.          IF  WRK-SEQ-ENVIO-RCOR      EQUAL ZEROS
.              MOVE 1                  TO WRK-SEQ-ENVIO-RCOR
.                                         RCOR10-SEQ-MSG
.              MOVE SPACES             TO RCOR10-NRO-CTRLIF
.          ELSE
.              ADD  1                  TO WRK-SEQ-ENVIO-RCOR
.              MOVE WRK-SEQ-ENVIO-RCOR TO RCOR10-SEQ-MSG
ID6854         MOVE WRK-ENVIO-NUMCTRLIF-AUX
ID6854                                 TO RCOR10-NRO-CTRLIF
.          END-IF.
.     *
.          MOVE RCOR10-TAM-SISMSG      TO WRK-ENVIO-LEN.
.     *
.          MOVE RCOR10-SISMSG(1: LENGTH OF RCOR10-SISMSG)
.                                      TO WRK-ENVIO-SIS.
.     *
ID6854     MOVE WRK-ENVIO-NUMCTRLIF-AUX
ID6854                                 TO WRK-ENVIO-NUMCTRLIF
.          .
.     *
.     *----------------------------------------------------------------*
.      4110-99-FIM.                    EXIT.
7C0120*----------------------------------------------------------------*
STE001*----------------------------------------------------------------*
.      5051-TRATAR-DE-PARA             SECTION.
.     *----------------------------------------------------------------*
.          EVALUATE WRK-INDICADOR-TELA
.              WHEN  1
.                 MOVE 'S'             TO WRK-INDICADOR-TELA-X
.              WHEN  2
.                 MOVE 'N'             TO WRK-INDICADOR-TELA-X
.              WHEN OTHER
.                 MOVE '*'             TO WRK-INDICADOR-TELA-X
.          END-EVALUATE.
.     *----------------------------------------------------------------*
.      5051-99-FIM.  EXIT.
STE001*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR ACESSAR FUNCIONAL UORG122B.             *
      *----------------------------------------------------------------*
       5100-ACESSAR-UORG122B           SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
               PROGRAM (WRK-UORG122B)
               COMMAREA (WRK-AREA-UORG122B)
               LENGTH (LENGTH OF WRK-AREA-UORG122B)
               NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '5100-ACESSAR-UORG122B'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
           IF (UORGW000-COD-RETORNO    NOT EQUAL ZEROS)
               MOVE WRK-UORG122B       TO FRWKGMOD-NOME-MODULO
               MOVE UORGW000-BLOCO-RETORNO
                                       TO FRWKGMOD-BLOCO-RETORNO
                                          CRURWIY1-BLOCO-RETORNO
               MOVE '5100-ACESSAR-UORG122B'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-IF
           .
      *
      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR ACESSAR FUNCIONAL PSDC3BSR.             *
      *----------------------------------------------------------------*
       5200-ACESSAR-PSDC3BSR           SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
               PROGRAM (WRK-PSDC3BSR)
               COMMAREA (WRK-AREA-PSDC3BSR)
               LENGTH (LENGTH OF WRK-AREA-PSDC3BSR)
               NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '5200-ACESSAR-PSDC3BSR'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
           IF (PSDCW00W-COD-RETORNO    OF WRK-AREA-PSDC3BSR
                                       NOT EQUAL ZEROS)
               MOVE WRK-PSDC3BSR       TO FRWKGMOD-NOME-MODULO
               MOVE PSDCW00W-BLOCO-RETORNO
                                       OF WRK-AREA-PSDC3BSR
                                       TO FRWKGMOD-BLOCO-RETORNO
                                          CRURWIY1-BLOCO-RETORNO
               MOVE '5200-ACESSAR-PSDC3BSR'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-IF
           .
      *
      *----------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR ACESSAR FUNCIONAL PSDC312C.             *
      *----------------------------------------------------------------*
       5300-ACESSAR-PSDC312C           SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
               PROGRAM (WRK-PSDC312C)
               COMMAREA (WRK-AREA-PSDC312C)
               LENGTH (LENGTH OF WRK-AREA-PSDC312C)
               NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '5300-ACESSAR-PSDC312C'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
           IF (PSDCW00W-COD-RETORNO    OF WRK-AREA-PSDC312C
                                       NOT EQUAL ZEROS)
               MOVE WRK-PSDC312C       TO FRWKGMOD-NOME-MODULO
               MOVE PSDCW00W-BLOCO-RETORNO
                                       OF WRK-AREA-PSDC312C
                                       TO FRWKGMOD-BLOCO-RETORNO
                                          CRURWIY1-BLOCO-RETORNO
               MOVE '5300-ACESSAR-PSDC312C'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-IF
           .
      *
      *----------------------------------------------------------------*
       5300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * TRATAR INCLUSAO TABELAS TCTRL_ENVIO_INFO E TLIN_MSGEM_REMSS
      * ENVIA RCOR7000 E ATULIZA TABELAS CONFORME RETORNO.
      *----------------------------------------------------------------*
       5500-TRATAR-INCLUSAO            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 4100-ENVIO-RCOR7000
      *
           PERFORM 6000-INCLUIR-CRURB0U2
      *
           PERFORM 6100-INCLUIR-CRURB0UA

TESTEM     MOVE '5500-TRATAR-INCLUSAO'
TESTEM                                     TO WRK-PARAGRAFO
TESTEM     MOVE SPACES                     TO WRK-RLIN-MSGEM-TEXT
TESTEM     MOVE 'REGISTRO INSERIDOS NA TABELA'
TESTEM                                     TO WRK-RLIN-MSGEM-TEXT
TESTEM     PERFORM 9500-ERRO-LIVRE

BR1901     MOVE 'S'                    TO WRK-ENVIADO.
      *
      *----------------------------------------------------------------*
       5500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
EL0817*                                                                *
7C0120*----------------------------------------------------------------*
.     * TRATAR CONTINUACAO DAS MAENSAGENS SO VAI GRAVAR E NAO ENVIAR
.     *----------------------------------------------------------------*
.      5510-TRATAR-INCLUSAO-CONT       SECTION.
.     *----------------------------------------------------------------*
.     *
.          PERFORM 4110-TRATA-RCOR7000
.     *
.          PERFORM 6000-INCLUIR-CRURB0U2
.     *
.          PERFORM 6100-INCLUIR-CRURB0UA
.          .
.          MOVE 'S'                    TO WRK-ENVIADO.
.     *
.     *----------------------------------------------------------------*
.      5510-99-FIM.                    EXIT.
7C0120*----------------------------------------------------------------*
.     *----------------------------------------------------------------*
.     *    ROTINA DE ACESSO AO MODULO CRUR3G8L P OBTER OCORRENCIAS DE  *
.     *    COD DE PESSOA                                               *
.     *----------------------------------------------------------------*
.      5530-ACESSAR-CRUR3G8L           SECTION.
.     *----------------------------------------------------------------*
.     *                                                                *
.          INITIALIZE                  CRURWG8I-BLOCO-ENTRADA
.                                      OF WRK-AREA-CRUR3G8L
.                                      CRURW00W-BLOCO-RETORNO
.                                      OF WRK-AREA-CRUR3G8L
.                                      REPLACING ALPHANUMERIC BY SPACES
.                                                     NUMERIC BY ZEROS
.     *                                                                *
.          MOVE 50                     TO CRURWG8I-E-MAX-OCCOR
.                                      OF WRK-AREA-CRUR3G8L
.     *                                                                *
.          MOVE WRK-CCPF-CNPJ-BENEF    TO CRURWG8I-E-NRO-CPF-CNPJ
.          MOVE WRK-CD-FILIAL-CNPJ-BENEF
.                                      TO CRURWG8I-E-FILIAL-CNPJ
.          MOVE WRK-CCTRL-CPF-CNPJ-BENEF
.                                      TO CRURWG8I-E-CTRL-CPF-CNPJ
.          MOVE CRURWIY1-E-USUARIO     TO CRURWG8I-E-CUSUAR
.          MOVE WRK-IND-PAG-FICHA      TO CRURWG8I-INDICADOR-PAGINACAO
.     *                                                                *
.          EXEC CICS LINK
.               PROGRAM  (WRK-CRUR3G8L)
.               COMMAREA (WRK-AREA-CRUR3G8L)
.               LENGTH   (LENGTH       OF WRK-AREA-CRUR3G8L)
.               NOHANDLE
.          END-EXEC
.     *                                                                *
.          IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
.              MOVE '0590'             TO CRURWIY1-COD-ERRO
.                                      OF DFHCOMMAREA
.              MOVE '5530-ACESSAR-CRUR3G8L'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9400-ERRO-CICS
.          END-IF
.     *                                                                *
.          EVALUATE CRURW00W-COD-RETORNO
.                                      OF WRK-AREA-CRUR3G8L
.              WHEN 00
.                   MOVE 'N'           TO WRK-COD-RET
.              WHEN 01
.                   MOVE 'S'           TO WRK-IND-PAG-FICHA
.              WHEN OTHER
.                   MOVE '0600'        TO CRURWIY1-COD-ERRO
.                                      OF DFHCOMMAREA
.                   MOVE CRURW00W-COD-MENSAGEM
.                                      OF WRK-AREA-CRUR3G8L
.                                      TO CRURWIY1-COD-MENSAGEM
.                                      OF DFHCOMMAREA
.                   MOVE WRK-CRUR3G8L  TO FRWKGMOD-NOME-MODULO
.                   MOVE CRURW00W-BLOCO-RETORNO
.                                      OF WRK-AREA-CRUR3G8L
.                                      TO FRWKGMOD-BLOCO-RETORNO
.                   MOVE '5530-ACESSAR-CRUR3G8L'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.                   PERFORM 9300-ERRO-MODULO
.          END-EVALUATE
.     *
.          PERFORM 5540-TRATA-CRURB069
.          VARYING IND-2               FROM 1 BY 1 UNTIL
.                  IND-2               GREATER CRURWG8I-S-NUM-OCCORS
.               OR WRK-ACHOU           EQUAL 'S'
.     *
.          .
.     *                                                                *
.     *----------------------------------------------------------------*
.      5530-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *                                                                *
.     *----------------------------------------------------------------*
.     *    ROTINA PARA TRATAR SE EXISTE FICHA CADASTRAL                *
.     *----------------------------------------------------------------*
.      5540-TRATA-CRURB069             SECTION.
.     *----------------------------------------------------------------*
.     *
.          INITIALIZE                  CRURB069
.     *
.          MOVE CRURWG8I-S-CPSSOA(IND-2)
.                                      TO CPSSOA
.                                      OF CRURB069
.     *
.          EXEC SQL
.              SELECT VALUE(MAX(CPSSOA), 0)
.                INTO :WRK-CPSSOA
.                FROM DB2PRD.TFICHA_CAD_RURAL
.               WHERE CPSSOA           = :CRURB069.CPSSOA
.                 AND CFICHA_CAD_RURAL = 1
.          END-EXEC
.     *
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100 AND -811
.                                                      AND -305)
.          OR (SQLWARN0                EQUAL 'W')
.              SET DB2-SELECT          TO TRUE
.              MOVE '0610'             TO CRURWIY1-COD-ERRO
.                                      OF DFHCOMMAREA
.              MOVE 'TFICHA_CAD_RURAL' TO FRWKGDB2-NOME-TABELA
.              MOVE '5540-TRATA-CRURB069'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9000-ERRO-DB2
.          END-IF
.     *
.          IF WRK-CPSSOA               NOT EQUAL ZEROS
.              MOVE 'S'                TO WRK-ACHOU
.          END-IF
.     *
.          .
.     *
.     *----------------------------------------------------------------*
.      5540-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *                                                                *
      *----------------------------------------------------------------*
      * INCLIR REGISTRO TABELA TCTRL_ENVIO_INFO                        *
      *----------------------------------------------------------------*
       6000-INCLUIR-CRURB0U2           SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2100-SELECIONAR-MAXIMO
           MOVE WRK-NENVIO-INFO-RECOR  TO NENVIO-INFO-RECOR OF CRURB0U2
      *
           MOVE 'COR0001'              TO CMSGEM-SIST-BACEN
                                       OF CRURB0U2
           MOVE WRK-ENVIO-NUMCTRLIF    TO CRQUIS-FINCR-BACEN
                                       OF CRURB0U2
           MOVE ZEROS                  TO WRK-CRQUIS-FINCR-NULL
           MOVE 5                      TO CSIT-MSGEM-BACEN
                                       OF CRURB0U2
           MOVE 'A'                    TO CSIT-REG-RURAL
                                       OF CRURB0U2
           MOVE CRURWIY1-E-USUARIO     TO CUSUAR-INCL
                                       OF CRURB0U2
           MOVE -1                     TO WRK-CUSUAR-MANUT-NULL
           MOVE -1                     TO WRK-HMANUT-REG-NULL
      *
           EXEC SQL
                INSERT INTO DB2PRD.TCTRL_ENVIO_INFO
                      (CPSSOA_JURID_CONTR
                      ,CTPO_CONTR_NEGOC
                      ,NSEQ_CONTR_NEGOC
                      ,NENVIO_INFO_RECOR
                      ,CMSGEM_SIST_BACEN
                      ,CRQUIS_FINCR_BACEN
                      ,CSIT_MSGEM_BACEN
                      ,CSIT_REG_RURAL
                      ,CUSUAR_INCL
                      ,HINCL_REG
                      ,CUSUAR_MANUT
                      ,HMANUT_REG)
                VALUES
                      (:CRURB0U2.CPSSOA-JURID-CONTR
                      ,:CRURB0U2.CTPO-CONTR-NEGOC
                      ,:CRURB0U2.NSEQ-CONTR-NEGOC
                      ,:CRURB0U2.NENVIO-INFO-RECOR
                      ,:CRURB0U2.CMSGEM-SIST-BACEN
                      ,:CRURB0U2.CRQUIS-FINCR-BACEN
                         :WRK-CRQUIS-FINCR-NULL
                      ,:CRURB0U2.CSIT-MSGEM-BACEN
                      ,:CRURB0U2.CSIT-REG-RURAL
                      ,:CRURB0U2.CUSUAR-INCL
                      , CURRENT TIMESTAMP
                      ,:CRURB0U2.CUSUAR-MANUT
                         :WRK-CUSUAR-MANUT-NULL
                      ,:CRURB0U2.HMANUT-REG
                         :WRK-HMANUT-REG-NULL)
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND -803)
           OR (SQLWARN0                EQUAL 'W')
               SET  DB2-INSERT         TO TRUE
               MOVE '0130'             TO CRURWIY1-COD-ERRO
               MOVE '6000-INCLUIR-CRURB0U2'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE 'TCTRL_ENVIO_INFO' TO FRWKGDB2-NOME-TABELA
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF (SQLCODE                 EQUAL -803)
               MOVE 83                 TO CRURWIY1-COD-RETORNO
               MOVE '0140'             TO CRURWIY1-COD-ERRO
               MOVE 'CRUR0043'         TO CRURWIY1-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF
           .
      *
      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * INCLIR REGISTRO TABELA TLIN_MSGEM_REMSS                        *
      *----------------------------------------------------------------*
       6100-INCLUIR-CRURB0UA           SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CPSSOA-JURID-CONTR     OF CRURB0U2
                                       TO CPSSOA-JURID-CONTR
                                       OF CRURB0UA
           MOVE CTPO-CONTR-NEGOC       OF CRURB0U2
                                       TO CTPO-CONTR-NEGOC
                                       OF CRURB0UA
           MOVE NSEQ-CONTR-NEGOC       OF CRURB0U2
                                       TO NSEQ-CONTR-NEGOC
                                       OF CRURB0UA
           MOVE NENVIO-INFO-RECOR      OF CRURB0U2
                                       TO NENVIO-INFO-RECOR
                                       OF CRURB0UA
           MOVE 1                      TO NLIN-MSGEM-REMSS
                                       OF CRURB0UA
           MOVE 1                      TO CPTCAO-TBELA
                                       OF CRURB0UA
           MOVE SPACES                 TO CTRNSM-INFO-BACEN
                                       OF CRURB0UA
           MOVE 'A'                    TO CSIT-REG-RURAL
                                       OF CRURB0UA
           MOVE CRURWIY1-E-USUARIO     TO CUSUAR-INCL
                                       OF CRURB0UA
           MOVE -1                     TO WRK-B0UA-CUS-MAN-NULL
           MOVE -1                     TO WRK-B0UA-HMA-REG-NULL
           MOVE WRK-ENVIO-LEN          TO RLIN-MSGEM-ENVID-LEN
                                       OF CRURB0UA.
           MOVE WRK-ENVIO-SIS          TO RLIN-MSGEM-ENVID-TEXT
                                       OF CRURB0UA.
      *
           EXEC SQL
                INSERT INTO DB2PRD.TLIN_MSGEM_REMSS
                      (CPSSOA_JURID_CONTR
                      ,CTPO_CONTR_NEGOC
                      ,NSEQ_CONTR_NEGOC
                      ,NENVIO_INFO_RECOR
                      ,NLIN_MSGEM_REMSS
                      ,CPTCAO_TBELA
                      ,CTRNSM_INFO_BACEN
                      ,CSIT_REG_RURAL
                      ,CUSUAR_INCL
                      ,HINCL_REG
                      ,CUSUAR_MANUT
                      ,HMANUT_REG
                      ,RLIN_MSGEM_ENVID)
                VALUES
                      (:CRURB0UA.CPSSOA-JURID-CONTR
                      ,:CRURB0UA.CTPO-CONTR-NEGOC
                      ,:CRURB0UA.NSEQ-CONTR-NEGOC
                      ,:CRURB0UA.NENVIO-INFO-RECOR
                      ,:CRURB0UA.NLIN-MSGEM-REMSS
                      ,:CRURB0UA.CPTCAO-TBELA
                      ,:CRURB0UA.CTRNSM-INFO-BACEN
                      ,:CRURB0UA.CSIT-REG-RURAL
                      ,:CRURB0UA.CUSUAR-INCL
                      , CURRENT TIMESTAMP
                      ,:CRURB0UA.CUSUAR-MANUT
                         :WRK-B0UA-CUS-MAN-NULL
                      ,:CRURB0UA.HMANUT-REG
                         :WRK-B0UA-HMA-REG-NULL
                      ,:CRURB0UA.RLIN-MSGEM-ENVID)
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND -803)
           OR (SQLWARN0                EQUAL 'W')
               SET  DB2-INSERT         TO TRUE
               MOVE '0150'             TO CRURWIY1-COD-ERRO
               MOVE '6100-INCLUIR-CRURB0UA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE 'TLIN_MSGEM_REMSS' TO FRWKGDB2-NOME-TABELA
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF (SQLCODE                 EQUAL -803)
               MOVE 83                 TO CRURWIY1-COD-RETORNO
               MOVE '0160'             TO CRURWIY1-COD-ERRO
               MOVE 'CRUR0043'         TO CRURWIY1-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF
           .
      *
      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
ED0604*----------------------------------------------------------------*
 .    * INCLIR REGISTRO TABELA TLIN_MSGEM_REMSS                        *
 .    *----------------------------------------------------------------*
 .     6200-OBTER-ALIQ-PROAGRO         SECTION.
 .    *----------------------------------------------------------------*
ED0604*
ED2804     MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
 .                                     TO WRK-CNEGOC-CONTR
 .         EXEC SQL
 .            SELECT  A.CORCAM_CONTR_RURAL
 .                   ,A.NAPOLC_SEGUR_BACEN
 .                   ,A.PALIQT_SEGUR_BACEN
                     ,A.CTPO_SEGUR_CONTT
 .            INTO   :CRURB0V3.CORCAM-CONTR-RURAL
 .                  ,:CRURB0V3.NAPOLC-SEGUR-BACEN
 .                  ,:CRURB0V3.PALIQT-SEGUR-BACEN
 .                        :WRK-PALIQT-SEGUR-BACEN-NULL
                    ,:CRURB0V3.CTPO-SEGUR-CONTT
 .            FROM DB2PRD.TAPOLC_SEGUR_BACEN   A
 .                ,DB2PRD.TORCAM_CONTR_CREDT   B
 .                ,DB2PRD.TCOMPO_CONTR_CREDT   C
 .           WHERE A.CORCAM_CONTR_RURAL  = B.CORCAM_CONTR_RURAL
 .             AND B.CORCAM_CONTR_RURAL  = C.CORCAM_CONTR_RURAL
 .             AND C.NSEQ_CONTR_NEGOC    = :WRK-CNEGOC-CONTR
 .             FETCH FIRST 1 ROWS ONLY
 .         END-EXEC
 .    *
 .         IF (SQLCODE                 NOT EQUAL ZEROS AND +100
ED2804                                 AND -811)
ED0604     OR (SQLWARN0                EQUAL 'W')
 .             SET DB2-SELECT          TO TRUE
 .             MOVE '0170'             TO FRWKGDB2-LOCAL
 .             MOVE 'TAPOLC_SEGUR_BACEN'
 .                                     TO FRWKGDB2-NOME-TABELA
 .             MOVE '2100-ACESSAR-TABELAS'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .             PERFORM 9000-ERRO-DB2
 .         END-IF
 .    *
 .         PERFORM 6210-TRATAR-NULOS
 .    *
 .         IF (SQLCODE                 EQUAL +100)
 .             MOVE ZEROS              TO PALIQT-SEGUR-BACEN
 .                                     OF CRURB0V3
CET            MOVE ZEROS              TO VCOBER-APOLC-SEGUR
CET                                    OF CRURB0V3
 .         END-IF
CET        MOVE VCOBER-APOLC-SEGUR     OF CRURB0V3
CET                                    TO WRK-VAPOLC-CS
 .    *
 .    * MOVENDO ZERO PARA ALIQUOTA - SEGURO PROPRIO
 .         IF CTPO-SEGUR-CONTT         EQUAL 1
 .            MOVE ZEROS               TO PALIQT-SEGUR-BACEN
 .                                     OF CRURB0V3
 .         END-IF
 .    *
 .    *    REMOCAO DE SINAL
 .         MOVE PALIQT-SEGUR-BACEN     OF CRURB0V3
 .                                     TO WRK-C-SINAL-9V
 .         MOVE WRK-S-SINAL-9V         TO WRK-VLR-ALIQ
 .    *
 .         IF WRK-VLR-ALIQ             EQUAL ZEROS
 .             MOVE ALL '*'            TO CRURY1-ALQ-PROAGRO
 .         ELSE
 .    *    ARREDONDAMENTO PARA RETIRAR CASAS DECIMAIS
 .             COMPUTE WRK-VLR-ALIQ-SICOR  ROUNDED
ED0604                                 = WRK-VLR-ALIQ
ED2804*
 .    *    REMOCAO DE COMPACTACAO
 .             MOVE WRK-VLR-ALIQ-SICOR TO WRK-VLR-ALIQ-SICOR-SC
 .    *
 .    *    TRANSFORMACAO NUMERICO EM CHAR
 .             MOVE WRK-VLR-ALIQ-SICOR-SC
ED2804                                 TO WRK-VLR-ALIQ-N
ED0604         MOVE WRK-VLR-ALIQ-C     TO CRURY1-ALQ-PROAGRO
 .    *
 .         END-IF
 .    *
 .         .
 .    *
 .    *----------------------------------------------------------------*
 .     6200-99-FIM.                    EXIT.
 .    *----------------------------------------------------------------*
 .    *
 .    *----------------------------------------------------------------*
 .    *  RESPONSAVEL POR TRATAR OS CAMPOS NULOS                        *
 .    *----------------------------------------------------------------*
 .     6210-TRATAR-NULOS               SECTION.
 .    *----------------------------------------------------------------*
 .    *
 .         IF (WRK-PALIQT-SEGUR-BACEN-NULL
 .                                     NOT EQUAL ZEROS)
 .             MOVE ZEROS              TO PALIQT-SEGUR-BACEN
 .                                     OF CRURB0V3
 .         END-IF
 .         .
 .    *
 .    *----------------------------------------------------------------*
 .     6210-99-FIM.                    EXIT.
 .    *----------------------------------------------------------------*
ED0604*
ED2804*----------------------------------------------------------------*
 .    *  OBTER SISTEMA DE PRODUCAO
 .    *----------------------------------------------------------------*
 .     6300-OBTER-SIST-PROD            SECTION.
 .    *----------------------------------------------------------------*
 .    *
 .    *    OBTER GRUPO 309 - DETALHE DO PRODUTO
 .         PERFORM 6310-BUSTAR-GRP-309
 .    *
 .    *    OBS - CAMPOS INVERTIDOS NO BOOK
 .    *          CTPO-CULTR-BACEN   = TIPO DE CULTURA
 .    *          CCULTR-RURAL-BACEN = GRAO E SEMENTE
 .    *
 .    *    *************OBTER TIPO DE AGRICULTURA BACEN*****************
 .    *
 .         MOVE CRURW12V-CAGROP-RURAL-BACEN
 .                                     TO WRK-TIPO-AGRIC-GRP
 .    *
 .         EXEC SQL
 .            SELECT CAGROP_SIST_BACEN
 .              INTO :WRK-TIPO-AGRIC-TAB
 .              FROM DB2PRD.TAGROP_SIST_BACEN
 .             WHERE CAGROP_RURAL_BACEN = :WRK-TIPO-AGRIC-GRP
 .         END-EXEC
 .    *
 .         IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
 .            (SQLWARN0                EQUAL 'W')
 .             SET DB2-SELECT          TO TRUE
 .             MOVE '0060'             TO FRWKGDB2-LOCAL
 .             MOVE 'TAGROP_SIST_BACEN' TO FRWKGDB2-NOME-TABELA
 .             MOVE '6300-OBTER-SIST-PROD'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .         ELSE
 .             IF SQLCODE                  EQUAL +100
 .                MOVE ZEROS              TO WRK-TIPO-AGRIC-TAB
 .             END-IF
 .         END-IF
 .    *
 .    *    MOVIMENTACAO PARA ITEM DE GRUPO - SISTEMA DE PRODUCAO
 .         MOVE WRK-TIPO-AGRIC-TAB     TO WRK-COM-SINAL-01
WIP001     MOVE WRK-SEM-SINAL-01       TO WRK-VAR11-NUM(1:1)
 .    *
 .    *    *************************************************************
 .    *    ***************OBTER TIPO DE INTEGRACAO BACEN****************
 .    *
 .         MOVE CRURW12V-CINTGC-RURAL-BACEN
 .                                     TO WRK-TIPO-INGR-GRP
 .    *
 .         EXEC SQL
 .            SELECT CINTGC_SIST_BACEN
 .              INTO :WRK-TIPO-INGR-TAB
 .              FROM DB2PRD.TINTGC_RURAL_BACEN
 .             WHERE CINTGC_RURAL_BACEN = :WRK-TIPO-INGR-GRP
 .         END-EXEC
 .         IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
 .            (SQLWARN0                EQUAL 'W')
 .             SET DB2-SELECT          TO TRUE
 .             MOVE '0060'             TO FRWKGDB2-LOCAL
 .             MOVE 'TINTGC_RURAL_BACEN' TO FRWKGDB2-NOME-TABELA
 .             MOVE '6300-OBTER-SIST-PROD'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .         ELSE
 .            IF SQLCODE               EQUAL +100
 .               MOVE ZEROS            TO WRK-TIPO-INGR-TAB
 .            END-IF
 .         END-IF
 .    *
 .    *    MOVIMETACAO PARA ITEM DE GRUPO - SISTEMA DE PRODUCAO
 .         MOVE WRK-TIPO-INGR-TAB      TO WRK-COM-SINAL-01
WIP001     MOVE WRK-SEM-SINAL-01       TO WRK-VAR11-NUM(2:1)
 .    *    *************************************************************
 .    *    ********************OBTER GRAO E SEMENTE*********************
 .    *
 .    *    MOVIMENTACAO PARA ITEM DE GRUPO
 .         IF CRURW12V-CTPO-CULTR-BACEN   EQUAL ZEROS
WIP001         MOVE ZEROS              TO WRK-VAR11-NUM(3:1)
 .         ELSE
 .             MOVE CRURW12V-CTPO-CULTR-BACEN(5:1)
WIP001                                 TO WRK-VAR11-NUM(3:1)
 .         END-IF
 .    *
 .    *    *************************************************************
 .    *    ***************OBTER TIPO DE IRRIGACAO BACEN*****************
 .    *
 .         MOVE CRURW12V-CIGCAO-RURAL-BACEN
 .                                     TO WRK-TIPO-IRRIGC-GRP
 .    *
 .         EXEC SQL
 .            SELECT CIGCAO_SIST_BACEN
 .              INTO :WRK-TIPO-IRRIGC-TAB
 .              FROM DB2PRD.TIGCAO_SIST_BACEN
 .             WHERE CIGCAO_RURAL_BACEN = :WRK-TIPO-IRRIGC-GRP
 .         END-EXEC
 .         IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
 .            (SQLWARN0                EQUAL 'W')
 .             SET DB2-SELECT          TO TRUE
 .             MOVE '0060'             TO FRWKGDB2-LOCAL
 .             MOVE 'TIGCAO_SIST_BACEN' TO FRWKGDB2-NOME-TABELA
 .             MOVE '6300-OBTER-SIST-PROD'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .         ELSE
 .             IF SQLCODE              EQUAL +100
 .                MOVE ZEROS           TO WRK-TIPO-IRRIGC-TAB
 .             END-IF
 .         END-IF
 .    *
 .    *    MOVIMENTACAO PARA ITEM DE GRUPO - SISTEMA DE PRODUCAO
 .         MOVE WRK-TIPO-IRRIGC-TAB    TO WRK-COM-SINAL-02
WIP001     MOVE WRK-SEM-SINAL-02       TO WRK-VAR11-NUM(4:2)
 .    *
 .    *    *************************************************************
 .    *    ***************OBTER TIPO DE CULTURA BACEN*******************
 .    *
 .         MOVE CRURW12V-CCULTR-RURAL-BACEN
 .                                     TO WRK-CCULTR-GRP
 .    *
 .         EXEC SQL
 .            SELECT CCULTR_SIST_BACEN
 .              INTO :WRK-TIPO-CULTR-TAB
 .              FROM DB2PRD.TCULTR_SIST_BACEN
 .             WHERE CCULTR_RURAL_BACEN = :WRK-CCULTR-GRP
 .         END-EXEC
 .         IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
 .            (SQLWARN0                EQUAL 'W')
 .             SET DB2-SELECT          TO TRUE
 .             MOVE '0060'             TO FRWKGDB2-LOCAL
 .             MOVE 'TCULTR_SIST_BACEN' TO FRWKGDB2-NOME-TABELA
 .             MOVE '6300-OBTER-SIST-PROD'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .         ELSE
 .             IF SQLCODE              EQUAL +100
 .                MOVE ZEROS           TO WRK-TIPO-CULTR-TAB
 .             END-IF
 .         END-IF
 .    *
 .    *    MOVIMENTACAO PARA ITEM DE GRUPO - SISTEMA DE PRODUCAO
 .         MOVE WRK-TIPO-CULTR-TAB     TO WRK-COM-SINAL-02
WIP001     MOVE WRK-SEM-SINAL-02       TO WRK-VAR11-NUM(6:2)
 .    *
 .    *    *************************************************************
 .    *    ****************OBTER TIPO DE CICLO BACEN********************
 .    *
 .         MOVE CRURW12V-CCICLO-PROD-BACEN
 .                                     TO WRK-TIPO-CICLO-GRP
 .    *
 .         EXEC SQL
 .            SELECT CCICLO_SIST_BACEN
 .              INTO :WRK-TIPO-CICLO-TAB
 .              FROM DB2PRD.TCICLO_PROD_BACEN
 .             WHERE CCICLO_PROD_BACEN = :WRK-TIPO-CICLO-GRP
 .         END-EXEC
 .    *
 .         IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
 .            (SQLWARN0                EQUAL 'W')
 .             SET DB2-SELECT          TO TRUE
 .             MOVE '0060'             TO FRWKGDB2-LOCAL
 .             MOVE 'TCICLO_PROD_BACEN' TO FRWKGDB2-NOME-TABELA
 .             MOVE '6300-OBTER-SIST-PROD'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .         ELSE
 .             IF SQLCODE              EQUAL +100
 .                MOVE ZEROS           TO WRK-TIPO-CICLO-TAB
 .             END-IF
 .         END-IF
 .    *
 .    *    MOVIMENTACAO PARA ITEM DE GRUPO - SISTEMA DE PRODUCAO
 .         MOVE WRK-TIPO-CICLO-TAB     TO WRK-COM-SINAL-02
WIP001     MOVE WRK-SEM-SINAL-02       TO WRK-VAR11-NUM(8:2)
PRI499*
PRI499*    *************************************************************
PRI499*    ****************OBTER TIPO DE CLIMA BACEN********************
PRI499*
PRI499     MOVE CRURW12V-CCLIMT-RURAL-BACEN
PRI499                                 TO WRK-TIPO-CLIMA-GRP
PRI499*
PRI499     EXEC SQL
PRI499        SELECT CCLIMT_SIST_BACEN
PRI499          INTO :WRK-TIPO-CLIMA-TAB
PRI499          FROM DB2PRD.TCLIMT_SIST_BACEN
PRI499         WHERE CCLIMT_RURAL_BACEN
PRI499                                 = :WRK-TIPO-CLIMA-GRP
PRI499     END-EXEC
PRI499*
PRI499     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
PRI499        (SQLWARN0                EQUAL 'W')
PRI499         SET DB2-SELECT          TO TRUE
PRI499         MOVE '0061'             TO FRWKGDB2-LOCAL
PRI499         MOVE 'TCLIMT_SIST_BACEN'
PRI499                                 TO FRWKGDB2-NOME-TABELA
PRI499         MOVE '6300-OBTER-SIST-PROD'
PRI499                                 TO FRWKGHEA-IDEN-PARAGRAFO
PRI499     ELSE
PRI499         IF SQLCODE              EQUAL +100
PRI499            MOVE ZEROS           TO WRK-TIPO-CLIMA-TAB
PRI499         END-IF
PRI499     END-IF
PRI499*
PRI499*    MOVIMENTACAO PARA ITEM DE GRUPO - SISTEMA DE PRODUCAO
PRI499     MOVE WRK-TIPO-CLIMA-TAB     TO WRK-COM-SINAL-02
PRI499     MOVE WRK-SEM-SINAL-02       TO WRK-VAR11-NUM(10:2)
STE001*
.     *    *************************************************************
.     *    ***********  OBTER OUTRAS PRATICAS DE MANEJO  ***************
.     *
.          MOVE CRURW12V-COUTRO-TRATO-SOLO
.                                      TO COUTRO-TRATO-SOLO
.                                      OF CRURB0X1.
.     *
.          EXEC SQL
.             SELECT COUTRO_SOLO_BACEN
.               INTO :CRURB0X1.COUTRO-SOLO-BACEN
.               FROM DB2PRD.TOUTRO_TRATO_SOLO
.              WHERE COUTRO_TRATO_SOLO = :CRURB0X1.COUTRO-TRATO-SOLO
.          END-EXEC
.     *
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0                EQUAL 'W')
.              SET DB2-SELECT          TO TRUE
.              MOVE '0062'             TO FRWKGDB2-LOCAL
.              MOVE 'TOUTRO_TRATO_SOLO'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '6300-OBTER-SIST-PROD'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.          ELSE
.              IF SQLCODE              EQUAL +100
.                 MOVE ZEROS           TO COUTRO-SOLO-BACEN
.                                      OF CRURB0X1
.              END-IF
.          END-IF
.     *
.     *    *************************************************************
.     *
.     *    MOVIMENTACAO DE SISTEMA DE PRODUCAO PARA SAIDA
WIP001*   XMOVE ZEROS                  TO WRK-VAR11-NUM(10:02)
.          MOVE COUTRO-SOLO-BACEN      OF CRURB0X1
.                                      TO WRK-COM-SINAL-02
STE001     MOVE WRK-SEM-SINAL-02       TO WRK-VAR11-NUM(12:2)
WIP001     MOVE WRK-VAR11-CHAR         TO CRURY1-COD-SIST-PRODC
 .    *
DEBUG      MOVE SQLCODE                TO WRK-SQLCODE-N
DEBUG      MOVE COUTRO-SOLO-BACEN      OF CRURB0X1
DEBUG        TO WRK-AUX-02-N
DEBUG      INITIALIZE WRK-MSG-ERRO-LIVRE
DEBUG      STRING ' CRUR3CY1'
DEBUG             ' SQLCODE...: 'WRK-SQLCODE
DEBUG             ' COUTRO-SOLO-BACEN - X01..: 'WRK-AUX-02-X
DEBUG             ' WRK-VAR11-CHAR...........: 'WRK-VAR11-CHAR
DEBUG             ' CRURY1-COD-SIST-PRODC....: 'CRURY1-COD-SIST-PRODC
DEBUG             ' W12V-COUTRO-TRATO-SOLO...: 'WRK-AUX-02-X
DEBUG             ' BONUS CAR................: 'WRK-PERCENTUAL-CAR-X
DEBUG              CRURW12V-COUTRO-TRATO-SOLO
DEBUG             DELIMITED BY SIZE           INTO WRK-RLIN-MSGEM-TEXT
DEBUG             PERFORM 9500-ERRO-LIVRE
 .         .
 .    *
 .    *----------------------------------------------------------------*
 .     6300-99-FIM.                    EXIT.
 .    *----------------------------------------------------------------*
 .    *
 .    *----------------------------------------------------------------*
 .    * OBTER GRUPO DE DETALHE DO PRODUTO - GRP 309
 .    *----------------------------------------------------------------*
 .     6310-BUSTAR-GRP-309             SECTION.
 .    *----------------------------------------------------------------*
 .    *
 .         INITIALIZE                  FNEBW3PI-BLOCO-ENTRADA
 .                                     FNEBW000-BLOCO-RETORNO
 .                                     OF WRK-AREA-FNEB33PL
 .                                     REPLACING ALPHANUMERIC BY SPACES
 .                                                    NUMERIC BY ZEROS
 .    *
 .         MOVE 309                    TO FNEBW3PI-E-CGRP-INFO-FLUXO
 .                                     OF WRK-AREA-FNEB33PL
 .    *
 .         PERFORM 3424-BUSCAR-INFO-GRUPO
 .    *
 .         MOVE FNEBW3PI-S-WCONTD-GRP-PPSTA
 .                                     OF WRK-AREA-FNEB33PL(1)
 .                                     TO CRURW12V-REGISTROS
 .    *
 .         .
 .    *
 .    *----------------------------------------------------------------*
 .     6310-99-FIM.                    EXIT.
 .    *----------------------------------------------------------------*
 .    *
 .    *----------------------------------------------------------------*
 .    * OBTER GRUPO DE DETALHE DO PRODUTO - GRP 309
 .    *----------------------------------------------------------------*
 .     6400-OBTER-MUN-IMOV             SECTION.
 .    *----------------------------------------------------------------*
 .    *
 .         MOVE CRURWIY1-E-NSEQ-CONTR-NEGOC
 .                                     TO WRK-CNEGOC-CONTR
 .         EXEC SQL
 .             SELECT A.CMUN
 .               INTO :WRK-CMUN-IMOV-SICOR
 .               FROM DB2PRD.TIMOV_CREDT_RURAL    A
 .                   ,DB2PRD.TORCAM_CONTR_CREDT   B
 .                   ,DB2PRD.TCOMPO_CONTR_CREDT   C
 .                   ,DB2PRD.TBEM_IMOV_BENEF      D
 .              WHERE D.CORCAM_CONTR_RURAL = B.CORCAM_CONTR_RURAL
 .                AND C.CORCAM_CONTR_RURAL = B.CORCAM_CONTR_RURAL
 .                AND C.NSEQ_CONTR_NEGOC   = :WRK-CNEGOC-CONTR
 .                AND D.CIMOV_CREDT_RURAL  = A.CIMOV_CREDT_RURAL
 .
 .         END-EXEC
 .    *
 .         IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
 .            (SQLWARN0                EQUAL 'W')
 .             SET DB2-SELECT          TO TRUE
 .             MOVE '0060'             TO FRWKGDB2-LOCAL
 .             MOVE 'TIMOV_CREDT_RURAL' TO FRWKGDB2-NOME-TABELA
 .             MOVE '6400-OBTER-MUN-IMOV'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .         END-IF
 .
 .         IF SQLCODE                  EQUAL +100
 .             MOVE ZEROS              TO WRK-CMUN-IMOV-SICOR
 .         END-IF
 .    *
 .    *    REMOCAO DE COMPACTACAO
 .         MOVE WRK-CMUN-IMOV-SICOR    TO WRK-CMUN-IMOV-SICOR-SC
 .    *
 .    *    REMOCAO DE SINAL E TRANSFORMACAO EM CHAR
 .         MOVE WRK-CMUN-IMOV-SICOR-SC TO WRK-IMOV-N
 .         MOVE WRK-IMOV-N-SICOR       TO CRURY1-COD-MUNIC
ID6151                                    WRK-IMOV-N-SICOR-SC
 .    *
 .         .
 .    *
 .    *----------------------------------------------------------------*
 .     6400-99-FIM.                    EXIT.
 .    *----------------------------------------------------------------*
ED2804*
ID6703*----------------------------------------------------------------*
ID6703* VERIFICA CLASSIFICACAO
ID6703*----------------------------------------------------------------*
ID6703 6410-ACESSAR-CRURB056           SECTION.
ID6703*----------------------------------------------------------------*
ID6703*
ID6703     INITIALIZE                  CRURB056
ID6703                                 REPLACING ALPHANUMERIC BY SPACES
ID6703                                                NUMERIC BY ZEROS
ID6703*
ID6703     MOVE CRURW12V-CEMPTO-RURAL  TO CEMPTO-RURAL       OF CRURB056
ID6703*
ID6703     EXEC SQL
ID6703          SELECT CEMPTO_RURAL
ID6703               , CCLASF_EMPTO_RURAL
ID6703            INTO :CRURB056.CEMPTO-RURAL
ID6703               , :CRURB056.CCLASF-EMPTO-RURAL
ID6703            FROM DB2PRD.TCLASF_EMPTO_RURAL
ID6703           WHERE CEMPTO_RURAL        = :CRURB056.CEMPTO-RURAL
ID6703             AND CCLASF_EMPTO_RURAL >= 0
ID6703           ORDER BY CCLASF_EMPTO_RURAL DESC
ID6703           FETCH FIRST 1 ROWS ONLY
ID6703     END-EXEC.
ID6703*
ID6703     IF (SQLCODE             NOT EQUAL ZEROS AND +100)
ID6703     OR (SQLWARN0                EQUAL 'W')
ID6703         SET  DB2-SELECT         TO TRUE
ID6703         MOVE '6410'             TO FRWKGDB2-LOCAL
ID6703         MOVE 'TCLASF_EMPTO_RURAL'
ID6703                                 TO FRWKGDB2-NOME-TABELA
ID6703         MOVE '6410-ACESSAR-CRURB056'
ID6703                                 TO FRWKGHEA-IDEN-PARAGRAFO
ID6703         PERFORM 9000-ERRO-DB2
ID6703     END-IF.
ID6703*
ID6703     IF  SQLCODE                 EQUAL ZEROS
ID6703         MOVE 'S'                TO WRK-ACHOU-CLASF
ID6703     ELSE
ID6703         MOVE 'N'                TO WRK-ACHOU-CLASF
ID6703     END-IF.
ID6703*
ID6703*----------------------------------------------------------------*
ID6703 6410-99-FIM.                    EXIT.
ID6703*----------------------------------------------------------------*
ID6703*
ID6703*----------------------------------------------------------------*
ID6703* OBTER GRUPO DE DETALHE DO PRODUTO - GRP 327
ID6703*----------------------------------------------------------------*
ID6703 6420-ACESSAR-CRUR3WJL-327       SECTION.
ID6703*----------------------------------------------------------------*
ID6703*
ID6703     INITIALIZE                  FNEBW3PI-BLOCO-ENTRADA
ID6703                                 FNEBW000-BLOCO-RETORNO
ID6703                                 OF WRK-AREA-FNEB33PL
ID6703                                 REPLACING ALPHANUMERIC BY SPACES
ID6703                                                NUMERIC BY ZEROS
ID6703*
ID6703     MOVE 327                    TO FNEBW3PI-E-CGRP-INFO-FLUXO
ID6703                                 OF WRK-AREA-FNEB33PL
ID6703*
ID6703     PERFORM 3424-BUSCAR-INFO-GRUPO
ID6703*
ID6703     MOVE FNEBW3PI-S-WCONTD-GRP-PPSTA
ID6703                                 OF WRK-AREA-FNEB33PL(1)
ID6703                                 TO CRURW08V-REGISTROS
ID6703*
ID6703     .
ID6703*
ID6703*----------------------------------------------------------------*
ID6703 6420-99-FIM.                    EXIT.
ID6703*----------------------------------------------------------------*
ID6703*
WP1021*----------------------------------------------------------------*
WP1021 6430-OBTER-SOLO-CULTIVO         SECTION.
WP1021*----------------------------------------------------------------*
WP1021*
WP1021     PERFORM 6431-OPEN-CSR09-CRURB0VJ
WP1021*
WP1021     PERFORM 6432-FETCH-CSR09-CRURB0VJ
WP1021                         UNTIL WRK-CSR-VJ-FIM EQUAL 'S'.
WP1021*
WP1021     PERFORM 6433-CLOSE-CSR09-CRURB0VJ.
WP1021*
WP1021*----------------------------------------------------------------*
WP1021 6430-99-FIM.                    EXIT.
WP1021*----------------------------------------------------------------*
WP1021*----------------------------------------------------------------*
WP1021*    ROTINA RESPONSAVEL POR ABRIR O CURSOR CSR09-CRURB0VJ        *
WP1021*----------------------------------------------------------------*
WP1021 6431-OPEN-CSR09-CRURB0VJ        SECTION.
WP1021*----------------------------------------------------------------*
WP1021*
WP1021     INITIALIZE                  CRURB0VJ
WP1021                                 REPLACING ALPHANUMERIC BY SPACES
WP1021                                                NUMERIC BY ZEROS
WP1021*
WP1021     MOVE  'N'                   TO WRK-CSR-VJ-FIM.
WP1021*
WP1021     MOVE  CORCAM-CONTR-RURAL  OF  CRURB0MB
WP1021                                 TO CORCAM-CONTR-RURAL OF CRURB0VJ
WP1021*
WP1021     EXEC SQL
WP1021          OPEN CSR09-CRURB0VJ
WP1021     END-EXEC
WP1021*
WP1021     IF (SQLCODE                 NOT EQUAL ZEROS) OR
WP1021        (SQLWARN0                EQUAL 'W')
WP1021         SET DB2-OPEN            TO TRUE
WP1021         MOVE '6431'             TO CRURWIY1-COD-ERRO
WP1021                                 OF DFHCOMMAREA
WP1021         MOVE 'TSOLO_IMOV_BENEF'
WP1021                           TO FRWKGDB2-NOME-TABELA
WP1021         MOVE '6431-OPEN-CSR09-CRURB0VJ'
WP1021                                 TO FRWKGHEA-IDEN-PARAGRAFO
WP1021         PERFORM 9000-ERRO-DB2
WP1021     END-IF.
WP1021*
WP1021*----------------------------------------------------------------*
WP1021 6431-99-FIM.                    EXIT.
WP1021*----------------------------------------------------------------*
WP1021*
WP1021*----------------------------------------------------------------*
WP1021*    ROTINA RESPONSAVEL POR LER O CURSOR CSR09-CRURB0VJ          *
WP1021*----------------------------------------------------------------*
WP1021 6432-FETCH-CSR09-CRURB0VJ       SECTION.
WP1021*----------------------------------------------------------------*
WP1021*
WP1021     EXEC SQL
WP1021         FETCH CSR09-CRURB0VJ INTO
WP1021              :CRURB0VJ.CTPO-SOLO-AGCLT ,
WP1021              :CRURB0VJ.CCICLO-CTVDA
WP1021                 :WRK-CCICLO-CTVDA-NULL ,
WP1021              :CRURB0VJ.CMANUT-SOLO
WP1021                 :WRK-CMANUT-SOLO-NULL
WP1021     END-EXEC
WP1021*
WP1021     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
WP1021        (SQLWARN0                EQUAL 'W')
WP1021         SET DB2-FETCH           TO TRUE
WP1021         MOVE '6432'             TO CRURWIY1-COD-ERRO
WP1021                                 OF DFHCOMMAREA
WP1021         MOVE 'TSOLO_IMOV_BENEF'
WP1021                                 TO FRWKGDB2-NOME-TABELA
WP1021         MOVE '6432-FETCH-CSR09-CRURB0VJ'
WP1021                                 TO FRWKGHEA-IDEN-PARAGRAFO
WP1021         PERFORM 9000-ERRO-DB2
WP1021     END-IF
WP1021*
WP1021     IF (SQLCODE                 EQUAL    +100)
WP1021         MOVE 'S'                TO       WRK-CSR-VJ-FIM
WP1021         GO TO 6432-99-FIM
WP1021     END-IF.
WP1021*
WP1021     IF  WRK-CCICLO-CTVDA-NULL   LESS  ZEROS
WP1021         MOVE  ZEROS             TO    CCICLO-CTVDA  OF CRURB0VJ
WP1021     END-IF
WP1021*
WP1021     IF  WRK-CMANUT-SOLO-NULL    LESS  ZEROS
WP1021         MOVE  ZEROS             TO    CMANUT-SOLO   OF CRURB0VJ
WP1021     END-IF
WP1021*
WP1021     IF (CCICLO-CTVDA  OF CRURB0VJ
WP1021                                 GREATER  ZEROS)  AND
WP1021        (CMANUT-SOLO   OF CRURB0VJ
WP1021                                 GREATER  ZEROS)
WP1021         MOVE  CTPO-SOLO-AGCLT OF CRURB0VJ
WP1021                                 TO       WRK-CTPO-SOLO-AGCLT
WP1021         MOVE  CCICLO-CTVDA    OF CRURB0VJ
WP1021                                 TO       WRK-CCICLO-CTVDA
WP1021         MOVE  CMANUT-SOLO     OF CRURB0VJ
WP1021                                 TO       WRK-CMANUT-SOLO
WP1021         MOVE 'S'                TO       WRK-CSR-VJ-FIM
WP1021     END-IF.
WP1021*
WP1021*----------------------------------------------------------------*
WP1021 6432-99-FIM.                    EXIT.
WP1021*----------------------------------------------------------------*
WP1021*
WP1021*----------------------------------------------------------------*
WP1021*    ROTINA RESPONSAVEL FOR FECHAR O CURSOR CSR09-CRURB0VJ       *
WP1021*----------------------------------------------------------------*
WP1021 6433-CLOSE-CSR09-CRURB0VJ       SECTION.
WP1021*----------------------------------------------------------------*
WP1021*
WP1021     EXEC SQL
WP1021         CLOSE CSR09-CRURB0VJ
WP1021     END-EXEC
WP1021*
WP1021     IF (SQLCODE                 NOT EQUAL ZEROS) OR
WP1021        (SQLWARN0                EQUAL 'W')
WP1021         SET DB2-CLOSE           TO TRUE
WP1021         MOVE '6433'             TO CRURWIY1-COD-ERRO
WP1021                                 OF DFHCOMMAREA
WP1021         MOVE 'TSOLO_IMOV_BENEF'
WP1021                                 TO FRWKGDB2-NOME-TABELA
WP1021         MOVE '6433-CLOSE-CSR09-CRURB0VJ'
WP1021                                 TO FRWKGHEA-IDEN-PARAGRAFO
WP1021         PERFORM 9000-ERRO-DB2
WP1021     END-IF.
WP1021*
WP1021*----------------------------------------------------------------*
WP1021 6433-99-FIM.                    EXIT.
WP1021*----------------------------------------------------------------*
ED0405*----------------------------------------------------------------*
 .    * OBTER GRUPO DE DETALHE DO PRODUTO - GRP 309
 .    *----------------------------------------------------------------*
 .     6500-BUSTAR-GRP-335             SECTION.
 .    *----------------------------------------------------------------*
 .    *
ID6260     MOVE 1                      TO WRK-INDC.
ID6260
ID6260     IF  WRK-CONTINUA            EQUAL 'S'
ID6260         IF  WRK-QTDE-6510       GREATER ZEROS
ID6260             MOVE WRK-QTDE-6510  TO WRK-INDC
ID6260             MOVE ZEROS          TO WRK-QTDE-6510
ID6260         ELSE
ID6260             PERFORM 6510-BUSCAR-CORDN
ID6260         END-IF
ID6631*
ID6631         MOVE FNEBW3PI-S-WCONTD-GRP-PPSTA
ID6631                                 OF WRK-AREA-FNEB33PL(WRK-INDC)
ID6631                                 TO CRURW15V-REGISTROS
               IF ((CRURW15V-CIMOV-CREDT-RURAL  EQUAL
                                           WRK-CIMOV-CREDT-RURAL-ANT)
               AND(CRURW15V-MDGLEBA        EQUAL WRK-MDGLEBA-ANT))
               AND WRK-ENVIADO             EQUAL 'S'
BR1901             MOVE 'N'              TO WRK-ENVIADO


7C2511             MOVE ALL '*'          TO WRK-B09A-SIS
7C2511                                    (WRK-POSICAO - 6: 6)
7C2511             PERFORM 6530-FORMATAR-GLEBA
7C2511*           XMOVE CRURY1-COR0001-6 TO WRK-B09A-SIS(WRK-POSICAO:
7C2511*                           LENGTH OF CRURY1-COR0001-6)
7C2511*           XADD LENGTH OF CRURY1-COR0001-6
7C2511*                                TO WRK-POSICAO WRK-TAM
               END-IF
ID6260     ELSE
               PERFORM 6510-BUSCAR-CORDN
ID6260     END-IF.
 .    *
ID6799     MOVE 'N'                    TO  WRK-AINDA-TEM
ID6799*
 .         PERFORM 6520-TRATA-COORDN
EG1117     VARYING WRK-INDC            FROM   WRK-INDC BY 1
EG1117       UNTIL (WRK-INDC          GREATER FNEBW3PI-S-MAX-OCORR
7C2511*         OR  WRK-INDCORDN      GREATER 90
7C2511          OR  WRK-INDCORDN      GREATER 350
7C2511          OR  WRK-NUM-POLIGN    GREATER 1)
ID6799*
7C2511*   XIF  (WRK-INDCORDN           GREATER 90  OR
7C2511     IF (WRK-INDCORDN           GREATER 350   OR
7C2511         WRK-NUM-POLIGN         GREATER 1   ) AND
ID6799         WRK-INDC           NOT GREATER FNEBW3PI-S-MAX-OCORR
ID6799         MOVE 'S'               TO  WRK-AINDA-TEM
ID6799     END-IF.

 .    *
 .    *----------------------------------------------------------------*
 .     6500-99-FIM.                    EXIT.
 .    *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       6501-MONTAR-POLIGONO            SECTION.
      *----------------------------------------------------------------*

           IF WRK-QTD-CULTVD           EQUAL ZEROS
7C2511        MOVE  ALL '*'            TO CRURY1-IDENTC-GLEBA
7C7511                                    CRURY1-AREA-NAO-CULT
7C2511        MOVE CRURY1-COR0001-6    TO
7C2511             WRK-B09A-SIS(WRK-POSICAO:
7C2511                                 LENGTH OF CRURY1-COR0001-6)
7C2511        ADD LENGTH OF CRURY1-COR0001-6  TO WRK-TAM
7C2511        ADD LENGTH OF CRURY1-COR0001-6  TO WRK-POSICAO
7C2511        MOVE 1                   TO WRK-IND3
7C2511        MOVE CRURY1-COR0001-6    TO WRK-B09A-SIS2(WRK-POSICAO2:
7C2511                                 LENGTH OF CRURY1-COR0001-6)
7C2511        ADD LENGTH OF CRURY1-COR0001-6  TO WRK-TAM2
7C2511        ADD LENGTH OF CRURY1-COR0001-6  TO WRK-POSICAO2
              MOVE ALL '*'             TO WRK-POLIGN
              MOVE WRK-POLIGN          TO CRURY1-WKT-PART
              MOVE CRURY1-COR0001-7    TO WRK-B09A-SIS(WRK-POSICAO:
                                          LENGTH OF CRURY1-COR0001-7)
              ADD LENGTH               OF CRURY1-COR0001-7
                                       TO WRK-POSICAO WRK-TAM
              GO                       TO 6501-99-FIM
           END-IF.

           MOVE SPACE                  TO WRK-POLIGN
           MOVE 1                      TO WRK-TAM-POLIGN

           MOVE 'POLYGON(('            TO WRK-POLIGN(WRK-TAM-POLIGN:9)
           ADD 9                       TO WRK-TAM-POLIGN

           PERFORM                     VARYING IND-5
                                       FROM 1
                                       BY 1
                                       UNTIL IND-5
                                       GREATER WRK-QTD-CULTVD
              IF IND-5                 GREATER 1
                 MOVE ','              TO WRK-POLIGN(WRK-TAM-POLIGN:1)
                 ADD 1                 TO WRK-TAM-POLIGN
              END-IF

              MOVE WRK-PONTO-CULTVD(IND-5)
                                       TO WRK-POLIGN
                                         (WRK-TAM-POLIGN: LENGTH OF
                                          WRK-PONTO-CULTVD(IND-5))

              ADD LENGTH               OF WRK-PONTO-CULTVD(IND-5)
                                       TO WRK-TAM-POLIGN
           END-PERFORM.

           MOVE ')'                    TO WRK-POLIGN(WRK-TAM-POLIGN:1)
           ADD 1                       TO WRK-TAM-POLIGN

           IF WRK-QTD-NCULTVD          GREATER ZEROS
              MOVE ',('                TO WRK-POLIGN(WRK-TAM-POLIGN:2)
              ADD 2                    TO WRK-TAM-POLIGN
           END-IF.

           PERFORM                     VARYING IND-5
                                       FROM 1
                                       BY 1
                                       UNTIL IND-5
                                       GREATER WRK-QTD-NCULTVD
              IF IND-5                 GREATER 1
                 MOVE ','              TO WRK-POLIGN(WRK-TAM-POLIGN:1)
                 ADD 1                 TO WRK-TAM-POLIGN
              END-IF

              MOVE WRK-PONTO-NCULTVD(IND-5)
                                       TO WRK-POLIGN
                                         (WRK-TAM-POLIGN:LENGTH OF
                                          WRK-PONTO-NCULTVD(IND-5))

              ADD LENGTH               OF WRK-PONTO-NCULTVD(IND-5)
                                       TO WRK-TAM-POLIGN
           END-PERFORM.

           IF WRK-QTD-NCULTVD          GREATER ZEROS
              MOVE ')'                 TO WRK-POLIGN(WRK-TAM-POLIGN:1)
              ADD 1                    TO WRK-TAM-POLIGN
           END-IF.

           MOVE ')'                    TO WRK-POLIGN(WRK-TAM-POLIGN:1)
           ADD 1                       TO WRK-TAM-POLIGN.

           MOVE  WRK-POLIGN            TO  CRURY1-WKT-PART
           MOVE  CRURY1-COR0001-7      TO  WRK-B09A-SIS(WRK-POSICAO:
                                           LENGTH OF CRURY1-COR0001-7)
           ADD LENGTH OF CRURY1-COR0001-7
                                       TO WRK-POSICAO WRK-TAM
           PERFORM 6502-LIMPAR-POLIGONO.

           ADD 1                       TO WRK-NUM-POLIGN.

      *----------------------------------------------------------------*
       6501-99-FIM.  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       6502-LIMPAR-POLIGONO            SECTION.
      *----------------------------------------------------------------*
           PERFORM                      VARYING IND-5
                                       FROM 1
                                       BY 1
                                       UNTIL IND-5
                                       GREATER WRK-QTD-CULTVD
              MOVE SPACES              TO WRK-PONTO-CULTVD(IND-5)
           END-PERFORM

           PERFORM                     VARYING IND-5
                                       FROM 1
                                       BY 1
                                       UNTIL IND-5
                                       GREATER WRK-QTD-NCULTVD
              MOVE SPACES              TO WRK-PONTO-NCULTVD(IND-5)
           END-PERFORM

           MOVE ZEROS                  TO WRK-QTD-CULTVD
                                          WRK-QTD-NCULTVD.

      *----------------------------------------------------------------*
       6502-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
 .    *----------------------------------------------------------------*
 .    * ROTINA PARA OBTER COORDENADAS GEODESICAS
 .    *----------------------------------------------------------------*
 .     6510-BUSCAR-CORDN               SECTION.
 .    *----------------------------------------------------------------*
 .    *
 .         INITIALIZE                  FNEBW3PI-BLOCO-ENTRADA
 .                                     FNEBW000-BLOCO-RETORNO
 .                                     OF WRK-AREA-FNEB33PL
 .                                     REPLACING ALPHANUMERIC BY SPACES
 .                                                    NUMERIC BY ZEROS
 .    *
 .         MOVE 335                    TO FNEBW3PI-E-CGRP-INFO-FLUXO
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE 15                     TO FNEBW3PI-E-MAX-OCORR
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE 1                      TO FNEBW3PI-E-NVRSAO-GRP-FLUXO
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE FNEBWBZI-S-CPSSOA-PPSTA-NEGOC(1)
 .                                     TO FNEBW3PI-E-CPSSOA-JURID-CONTR
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE FNEBWBZI-S-CCLASF-PPSTA-NEGOC(1)
 .                                     TO FNEBW3PI-E-CTPO-CONTR-NEGOC
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE FNEBWBZI-S-NSEQ-PPSTA-NEGOC(1)
 .                                     TO FNEBW3PI-E-NSEQ-CONTR-NEGOC
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE WRK-IND-PAG            TO FNEBW3PI-INDICADOR-PAGINACAO
 .                                     OF WRK-AREA-FNEB33PL
 .    *
 .         EXEC CICS LINK
 .              PROGRAM  (WRK-FNEB33PL)
 .              COMMAREA (WRK-AREA-FNEB33PL)
 .              LENGTH   (LENGTH       OF WRK-AREA-FNEB33PL)
 .              NOHANDLE
 .         END-EXEC
 .    *                                                                *
 .         IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
 .             MOVE '6510-BUSCAR-CORDN'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .             PERFORM 9400-ERRO-CICS
 .         END-IF
 .    *                                                                *
 .         EVALUATE FNEBW000-COD-RETORNO
 .                                     OF WRK-AREA-FNEB33PL
 .             WHEN 00
 .                  MOVE 'S'           TO WRK-COORDN-FIM
 .                  IF FNEBW000-COD-MENSAGEM
 .                                     OF WRK-AREA-FNEB33PL
 .                                     EQUAL 'FNEB0025'
 .                     MOVE 'N'        TO WRK-COORDN-EXIST
 .                  END-IF
 .             WHEN 01
 .                  MOVE 'S'           TO WRK-IND-PAG
 .             WHEN OTHER
 .                  IF  FNEBW000-COD-RETORNO
 .                                     OF WRK-AREA-FNEB33PL
 .                                     EQUAL 08
 .                  AND FNEBW000-COD-MENSAGEM
 .                                     OF WRK-AREA-FNEB33PL
 .                                     EQUAL 'FNEB0025'
 .                     MOVE ZEROS      TO FNEBW3PI-S-MAX-OCORR
 .                  END-IF
ID6344              IF  WRK-INDCORDN   EQUAL ZEROS
 .                      MOVE 'N'       TO WRK-COORDN-EXIST
ID6344              END-IF
 .                  MOVE 'S'           TO WRK-COORDN-FIM
 .         END-EVALUATE
 .         .
 .    *----------------------------------------------------------------*
 .     6510-99-FIM.                    EXIT.
 .    *----------------------------------------------------------------*
 .    *
 .    *----------------------------------------------------------------*
 .    * ROTINA PARA OBTER COORDENADAS GEODESICAS
 .    *----------------------------------------------------------------*
 .     6520-TRATA-COORDN               SECTION.
 .    *----------------------------------------------------------------*
 .    *
           MOVE FNEBW3PI-S-WCONTD-GRP-PPSTA
                                       OF WRK-AREA-FNEB33PL(WRK-INDC)
                                       TO CRURW15V-REGISTROS
      *
STE003     IF CRURW15V-IND-GLEBA-EXCL  EQUAL '1'
STE003        GO TO 6520-99-FIM
STE003     END-IF.
      *
ID5488     IF ((CRURW15V-CIMOV-CREDT-RURAL  NOT EQUAL
.                                          WRK-CIMOV-CREDT-RURAL-ANT)
EG1117     OR (CRURW15V-MDGLEBA        NOT EQUAL WRK-MDGLEBA-ANT)
STE003     AND (CRURW15V-IND-GLEBA-EXCL NOT EQUAL '1'))
.          AND WRK-AREA-NCULT          EQUAL 'S'
ID6799     AND WRK-INDCORDN            GREATER ZEROS
.     * MOVIMENTACAO DA AREA NAO CULTIVADA
               MOVE WRK-CIMOV-CREDT-RURAL-ANT
                                           TO WRK-CIMOV-CREDT-RURAL
               PERFORM 3140-SELECT-CRURB0B1
BR4.11********-MOVE CMUN OF CRURB0B1   TO WRK-CMUN-NUM
7C2511*       -MOVE ALL '*'            TO CRURY1-AREA-NAO-CULT
BR4.11********-MOVE WRK-CMUN-NUM-6     TO CRURY1-COD-MUN-GLEB
7C2511*       XMOVE CRURY1-COR0001-7A  TO WRK-B09A-SIS(WRK-POSICAO:
7C2511*                                LENGTH OF CRURY1-COR0001-7A)
7C2511*       XADD  LENGTH OF CRURY1-COR0001-7A
7C2511*                                TO WRK-TAM
7C2511*       XADD  LENGTH OF CRURY1-COR0001-7A
7C2511*                                TO WRK-POSICAO
.              MOVE 'N'                TO WRK-AREA-NCULT
STE001***  TRATAMENTO GLEBA EXCLUIDA
7C2511*       XPERFORM 3183-TRATAR-GLEBA-EXCLUIDA
ID5488     END-IF

EG1117     IF  CRURW15V-MDGLEBA       NUMERIC
EG1117         MOVE CRURW15V-MDGLEBA  TO WRK-IDENTC-GLEBA(6:05)
EG1117     ELSE
EG1117         MOVE ZEROS             TO WRK-IDENTC-GLEBA(6:05)
EG1117     END-IF.
EG1117
      *
ID5488     IF ((CRURW15V-CIMOV-CREDT-RURAL  NOT EQUAL
.                                          WRK-CIMOV-CREDT-RURAL-ANT)
EG1117     OR (CRURW15V-MDGLEBA        NOT EQUAL WRK-MDGLEBA-ANT))
               MOVE 'S'                TO WRK-AREA-NCULT
ID6799         MOVE 'N'                TO WRK-ENVIADO
7C2511         IF WRK-INDGLEBA         GREATER ZEROS
7C2511            PERFORM  6501-MONTAR-POLIGONO
7C2511         ELSE
7C2511            MOVE ALL '*'          TO WRK-B09A-SIS
7C2511                                    (WRK-POSICAO - 6: 6)
7C2511         END-IF
EL0116         ADD 1                   TO WRK-INDGLEBA
EL0116         MOVE WRK-INDGLEBA       TO WRK-INDGLEBA-NUM
EG1117         MOVE ZEROS              TO WRK-IDENTC-GLEBA(1:5)
EL0116         MOVE WRK-INDGLEBA-ALFA  TO WRK-IDENTC-GLEBA(11:9)
7C2511         IF WRK-NUM-POLIGN       LESS 2
EL0116            PERFORM 6530-FORMATAR-GLEBA
7C2511         END-IF
EL0116     END-IF
      *
STE003* --- MOVIMENTA SOMENTE AREA CULTIVADA
STE003     IF CRURW15V-IND-GLEBA-EXCL  NOT EQUAL '1'
AS1202*    LATITUDE
CAT408        MOVE CRURW15V-MLATTD-CORND-408
.                                      TO WRK-LAT-PONTO
.     *
7C2511        ADD 1                    TO WRK-QTD-CULTVD
7C2511        MOVE WRK-LAT-PONTO          TO WRK-LAT-PONTO-N
.             IF CRURW15V-MLATTD-CORND-SNAL
.                                      EQUAL 'N'
7C2511*       XCOMPUTE WRK-LAT-PONTO = WRK-LAT-PONTO * -1
7C2511           MOVE WRK-LAT-PONTO-9V7
7C2511                                 TO WRK-MASK-COORD
7C2511           MOVE WRK-MASK-COORD-X TO WRK-CULTVD-LAT
                                         (WRK-QTD-CULTVD)
7C2511           MOVE '-'              TO WRK-CULTVD-LAT
                                         (WRK-QTD-CULTVD)(1:1)
              ELSE
7C2511           MOVE WRK-LAT-PONTO-9V7   TO WRK-MASK-COORD
7C2511           MOVE WRK-MASK-COORD-X    TO WRK-CULTVD-LAT
7C2511                                   (WRK-QTD-CULTVD)
.             END-IF
7C2511        INSPECT WRK-CULTVD-LAT (WRK-QTD-CULTVD)
7C2511                                 REPLACING ALL ','
7C2511                                 BY '.'
.     *
7C2511*   XMOVE WRK-LAT-PONTO          TO WRK-LAT-PONTO-N
7C2511*   XMOVE WRK-LAT-PONTO-A        TO CRURY1-LAT-PONTO
.     *
.     *    LONGITUDE
.     *
CAT408        MOVE CRURW15V-MLOGTD-CORND-408
.                                      TO WRK-LONG-PONTO
7C2511        MOVE WRK-LONG-PONTO         TO WRK-LONG-PONTO-N
.     *
.             IF CRURW15V-MLOGTD-CORND-SNAL
.                                      EQUAL 'N'
7C2511*       XCOMPUTE WRK-LONG-PONTO = WRK-LONG-PONTO * -1
7C2511           MOVE WRK-LONG-PONTO-9V7 TO WRK-MASK-COORD
7C2511           MOVE WRK-MASK-COORD-X   TO WRK-CULTVD-LON
7C2511                                   (WRK-QTD-CULTVD)
7C2511           MOVE '-'                TO WRK-CULTVD-LON
7C2511                                   (WRK-QTD-CULTVD)(1:1)
              ELSE
7C2511           MOVE WRK-LONG-PONTO-9V7 TO WRK-MASK-COORD
7C2511           MOVE WRK-MASK-COORD-X   TO WRK-CULTVD-LON
7C2511                                   (WRK-QTD-CULTVD)
.             END-IF
7C2511        INSPECT WRK-CULTVD-LON (WRK-QTD-CULTVD)
7C2511                                 REPLACING ALL ','
7C2511                                 BY '.'
.     *
7C2511*   XMOVE WRK-LONG-PONTO         TO WRK-LONG-PONTO-N
7C2511*   XMOVE WRK-LONG-PONTO-A       TO CRURY1-LONG-PONTO
.     *
.     *    ALTITUDE
.     *
7C2511*    XMOVE CRURW15V-MALTTD-CORND-IMOV
7C2511*                                TO WRK-ALTT-PONTO
7C2511*
7C2511*    XIF CRURW15V-MALTTD-CORND-SNAL
7C2511*                                EQUAL 'N'
7C2511*        XCOMPUTE WRK-ALTT-PONTO = WRK-ALTT-PONTO * -1
7C2511*    END-IF
7C2511*
7C2511*    XMOVE WRK-ALTT-PONTO         TO WRK-ALTT-PONTO-N
7C2511*    XMOVE WRK-ALTT-PONTO-A       TO CRURY1-ALTT-PONTO
      *
 .    *    MOVIMENTAR OCORRENCIA
7C2511*    XMOVE CRURY1-COR0001-7       TO WRK-B09A-SIS(WRK-POSICAO:
7C2511*                                LENGTH OF CRURY1-COR0001-7)
7C2511*    XADD LENGTH OF CRURY1-COR0001-7 TO WRK-TAM
7C2511*    XADD LENGTH OF CRURY1-COR0001-7 TO WRK-POSICAO
7C2511*
7C2511*    XMOVE CRURY1-COR0001-7       TO WRK-B09A-SIS2(WRK-POSICAO2:
7C2511*                                LENGTH OF CRURY1-COR0001-7)
7C2511*    XADD LENGTH OF CRURY1-COR0001-7 TO WRK-TAM2
7C2511*    XADD LENGTH OF CRURY1-COR0001-7 TO WRK-POSICAO2
 .    *
 .            ADD 1                       TO WRK-INDCORDN
EL0116*
EL0116        MOVE CRURW15V-CIMOV-CREDT-RURAL
EL0116                                    TO WRK-CIMOV-CREDT-RURAL-ANT
EG1117        MOVE CRURW15V-MDGLEBA       TO WRK-MDGLEBA-ANT
ED0405*
STE003     END-IF
 .         .
 .    *
 .    *----------------------------------------------------------------*
 .     6520-99-FIM.                    EXIT.
 .    *----------------------------------------------------------------*
ED0405*
 .    *
EL0116*----------------------------------------------------------------*
EL0116* ROTINA PARA GRAVAR GLEBA
EL0116*----------------------------------------------------------------*
EL0116 6530-FORMATAR-GLEBA             SECTION.
EL0116*----------------------------------------------------------------*
EL0116*
EL0116      MOVE WRK-IDENTC-GLEBA      TO CRURY1-IDENTC-GLEBA
7C2511      MOVE ALL '*'               TO CRURY1-AREA-NAO-CULT
EL0116      MOVE CRURY1-COR0001-6      TO
EL0116           WRK-B09A-SIS(WRK-POSICAO:
EL0116                          LENGTH OF CRURY1-COR0001-6)
EL0116      ADD LENGTH OF CRURY1-COR0001-6  TO WRK-TAM
EL0116      ADD LENGTH OF CRURY1-COR0001-6  TO WRK-POSICAO
EL0116      MOVE 1                     TO WRK-IND3
EL0116      MOVE CRURY1-COR0001-6      TO WRK-B09A-SIS2(WRK-POSICAO2:
EL0116                          LENGTH OF CRURY1-COR0001-6)
EL0116      ADD LENGTH OF CRURY1-COR0001-6  TO WRK-TAM2
EL0116      ADD LENGTH OF CRURY1-COR0001-6  TO WRK-POSICAO2
EL0116*
EL0116     .
EL0116*
EL0116*----------------------------------------------------------------*
EL0116 6530-99-FIM.                    EXIT.
EL0116*----------------------------------------------------------------*
ED0405*
EL0506*----------------------------------------------------------------*
 .    * OBTER GRUPO DE DETALHE DO PRODUTO - GRP 309
 .    *----------------------------------------------------------------*
 .     6600-BUSTAR-GRP-308             SECTION.
 .    *----------------------------------------------------------------*
 .    *
 .    *    OBTER GRUPO DE COORDENADAS
 .         MOVE 'I'                    TO WRK-IND-PAG
 .    *
 .         PERFORM 6610-BUSCAR-CONSORCIO
 .    *
 .         PERFORM 6620-TRATA-CONSORCIADO
 .         VARYING WRK-IND8            FROM 1 BY 1
 .           UNTIL WRK-IND8            GREATER FNEBW3PI-S-MAX-OCORR
 .              OR WRK-INDCONSOR       GREATER 999
 .              OR WRK-CONSOR-FIM      EQUAL 'S'
 .    *
 .         .
 .    *
 .    *----------------------------------------------------------------*
 .     6600-99-FIM.                    EXIT.
 .    *----------------------------------------------------------------*
 .    *
 .    *----------------------------------------------------------------*
 .    * ROTINA PARA OBTER COORDENADAS GEODESICAS
 .    *----------------------------------------------------------------*
 .     6610-BUSCAR-CONSORCIO           SECTION.
 .    *----------------------------------------------------------------*
 .    *
 .         INITIALIZE                  FNEBW3PI-BLOCO-ENTRADA
 .                                     FNEBW000-BLOCO-RETORNO
 .                                     OF WRK-AREA-FNEB33PL
 .                                     REPLACING ALPHANUMERIC BY SPACES
 .                                                    NUMERIC BY ZEROS
 .    *
 .         MOVE 308                    TO FNEBW3PI-E-CGRP-INFO-FLUXO
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE 15                     TO FNEBW3PI-E-MAX-OCORR
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE 1                      TO FNEBW3PI-E-NVRSAO-GRP-FLUXO
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE FNEBWBZI-S-CPSSOA-PPSTA-NEGOC(1)
 .                                     TO FNEBW3PI-E-CPSSOA-JURID-CONTR
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE FNEBWBZI-S-CCLASF-PPSTA-NEGOC(1)
 .                                     TO FNEBW3PI-E-CTPO-CONTR-NEGOC
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE FNEBWBZI-S-NSEQ-PPSTA-NEGOC(1)
 .                                     TO FNEBW3PI-E-NSEQ-CONTR-NEGOC
 .                                     OF WRK-AREA-FNEB33PL
 .         MOVE WRK-IND-PAG            TO FNEBW3PI-INDICADOR-PAGINACAO
 .                                     OF WRK-AREA-FNEB33PL
 .    *
 .         EXEC CICS LINK
 .              PROGRAM  (WRK-FNEB33PL)
 .              COMMAREA (WRK-AREA-FNEB33PL)
 .              LENGTH   (LENGTH       OF WRK-AREA-FNEB33PL)
 .              NOHANDLE
 .         END-EXEC
 .    *                                                                *
 .         IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
 .             MOVE '6610-BUSCAR-CONSORCIO'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .             PERFORM 9400-ERRO-CICS
 .         END-IF
 .    *                                                                *
 .         EVALUATE FNEBW000-COD-RETORNO
 .                                     OF WRK-AREA-FNEB33PL
 .             WHEN 00
 .                  MOVE 'S'           TO WRK-CONSOR-FIM
 .                  IF FNEBW000-COD-MENSAGEM
 .                                     OF WRK-AREA-FNEB33PL
 .                                     EQUAL 'FNEB0025'
 .                     MOVE 'N'        TO WRK-CONSOR-EXIST
 .                  END-IF
 .             WHEN 01
 .                  MOVE 'S'           TO WRK-IND-PAG
 .             WHEN OTHER
 .                  IF  FNEBW000-COD-RETORNO
 .                                     OF WRK-AREA-FNEB33PL
 .                                     EQUAL 08
 .                  AND FNEBW000-COD-MENSAGEM
 .                                     OF WRK-AREA-FNEB33PL
 .                                     EQUAL 'FNEB0025'
 .                     MOVE ZEROS      TO FNEBW3PI-S-MAX-OCORR
 .                  END-IF
 .                  MOVE 'N'           TO WRK-CONSOR-EXIST
 .                  MOVE 'S'           TO WRK-CONSOR-FIM
 .         END-EVALUATE
 .         .
 .    *
 .    *----------------------------------------------------------------*
 .     6610-99-FIM.                    EXIT.
 .    *----------------------------------------------------------------*
 .    *
 .    *----------------------------------------------------------------*
 .    * ROTINA PARA OBTER COORDENADAS GEODESICAS
 .    *----------------------------------------------------------------*
 .     6620-TRATA-CONSORCIADO          SECTION.
 .    *----------------------------------------------------------------*
 .    *
 .         MOVE FNEBW3PI-S-WCONTD-GRP-PPSTA
 .                                     OF WRK-AREA-FNEB33PL(WRK-IND8)
 .                                     TO CRURW17V-REGISTROS
 .    *
 .         PERFORM 6630-OBTER-PRODT-CONSORCIADO
 .    *
 .         MOVE WRK-CPRODT-RURAL       TO CRURY1-COD-PRODT-CONSCD
 .         MOVE CRURY1-COR0001-8       TO WRK-B09A-SIS(WRK-POSICAO:
 .                                     LENGTH OF CRURY1-COR0001-8)
 .         ADD LENGTH OF CRURY1-COR0001-8 TO WRK-TAM
 .         ADD LENGTH OF CRURY1-COR0001-8 TO WRK-POSICAO
 .         MOVE CRURY1-COR0001-8       TO WRK-B09A-SIS2(WRK-POSICAO2:
 .                                     LENGTH OF CRURY1-COR0001-4)
 .         ADD LENGTH OF CRURY1-COR0001-8 TO WRK-TAM2
 .         ADD LENGTH OF CRURY1-COR0001-8 TO WRK-POSICAO2
 .         ADD 1                       TO WRK-INDCONSOR
 .    *
 .         .
 .    *
 .    *----------------------------------------------------------------*
 .     6620-99-FIM.                    EXIT.
 .    *----------------------------------------------------------------*
 .    *
 .    *----------------------------------------------------------------*
 .    *    ROTINA RESPONSAVEL PELO ACESSO AO SERVICO CRUR414S          *
 .    *----------------------------------------------------------------*
 .     6630-OBTER-PRODT-CONSORCIADO      SECTION.
 .    *----------------------------------------------------------------*
 .    *
 .         INITIALIZE                  CRURW14C-BLOCO-ENTRADA
 .                                     CRURW00W-BLOCO-RETORNO
 .                                     OF WRK-AREA-CRUR414S
 .                                     REPLACING ALPHANUMERIC BY SPACES
 .                                                    NUMERIC BY ZEROS
 .    *
 .         MOVE CRURW17V-CEMPTO-RURAL  TO CRURW14C-E-CEMPTO-RURAL
 .    *
 .         EXEC CICS LINK
 .              PROGRAM  (WRK-CRUR414S)
 .              COMMAREA (WRK-AREA-CRUR414S)
 .              LENGTH   (LENGTH       OF WRK-AREA-CRUR414S)
 .              NOHANDLE
 .         END-EXEC
 .    *
 .         IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
 .             MOVE '0130'             TO CRURWIY1-COD-ERRO
 .                                     OF DFHCOMMAREA
 .             MOVE '6630-OBTER-PRODT-CONSORCIADO'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .             PERFORM 9400-ERRO-CICS
 .         END-IF
 .    *
 .         EVALUATE CRURW00W-COD-RETORNO
 .                                     OF WRK-AREA-CRUR414S
 .             WHEN 00
 .                 MOVE CRURW14C-S-CPRODT-RURAL-BACEN
 .                                     TO WRK-CPRODT-RURAL-N
 .    *
 .             WHEN OTHER
 .                 MOVE '0145'         TO CRURWIY1-COD-ERRO
 .                                     OF DFHCOMMAREA
 .                 MOVE CRURW00W-COD-MENSAGEM
 .                                     OF WRK-AREA-CRUR414S
 .                                     TO CRURWIY1-COD-MENSAGEM
 .                                     OF DFHCOMMAREA
 .                 MOVE WRK-CRUR414S   TO FRWKGMOD-NOME-MODULO
 .                 MOVE CRURW00W-BLOCO-RETORNO
 .                                     OF WRK-AREA-CRUR414S
 .                                     TO FRWKGMOD-BLOCO-RETORNO
 .                 MOVE '6630-OBTER-PRODT-CONSORCIADO'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .                 PERFORM 9300-ERRO-MODULO
 .         END-EVALUATE
 .    *
 .         .
 .    *
 .    *----------------------------------------------------------------*
 .     6630-99-FIM.                    EXIT.
EL0506*----------------------------------------------------------------*
EG1117
EG1117*----------------------------------------------------------------*
EG1117 7000-TRATAR-CONTINUACAO         SECTION.
EG1117*----------------------------------------------------------------*
EG1117
EG1117     MOVE 1                      TO WRK-POSICAO.
EG1117     MOVE ZEROS                  TO WRK-TAM.
EG1117
EG1117***
ID6260     MOVE SPACES                 TO WRK-B09A-SIS.
ID6260
EG1117     MOVE CRURY1-COR0001-1       TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-1).
EG1117     ADD LENGTH OF CRURY1-COR0001-1     TO WRK-POSICAO WRK-TAM.
EG1117***
EG1117     MOVE ALL '*'                TO CRURY1-GP-COR0001-EMIT
EG1117     MOVE CRURY1-COR0001-2       TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-2).
EG1117     ADD LENGTH OF CRURY1-COR0001-2     TO WRK-POSICAO WRK-TAM.
EG1117***
EG1117     MOVE CRURY1-COR0001-3       TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-3).
EG1117     ADD LENGTH OF CRURY1-COR0001-3     TO WRK-POSICAO WRK-TAM.
EG1117***
EG1117     MOVE ALL '*'                TO CRURY1-GP-COR0001-RENEGC
EG1117     MOVE CRURY1-COR0001-4       TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-4).
EG1117     ADD LENGTH OF CRURY1-COR0001-4     TO WRK-POSICAO WRK-TAM.
EG1117***
EG1117     MOVE CRURY1-COR0001-5       TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-5).
EG1117     ADD LENGTH OF CRURY1-COR0001-5     TO WRK-POSICAO WRK-TAM.
EG1117***
EG1117***  **********************************************
EG1117***  CONTINUA FORMATACAO DAS COORDENADAS GEODESICAS
EG1117***
STE003*    *OVE 'N'                    *O WRK-BUSCAR-CORDN.
EG1117     PERFORM 3123-FORMATA-GRUPO-PTPOLG.
EG1117***
EG1117***  **********************************************
EG1117***
EG1117     MOVE CRURY1-COR0001-8       TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-8).
EG1117     ADD LENGTH OF CRURY1-COR0001-8     TO WRK-POSICAO WRK-TAM.
EG1117***
EG1117     MOVE CRURY1-COR0001-9       TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-9).
EG1117     ADD LENGTH OF CRURY1-COR0001-9     TO WRK-POSICAO WRK-TAM.
EG1117***
EG1117     MOVE ALL '*'                TO CRURY1-GP-COR0001-PROP-F
EG1117     MOVE CRURY1-COR0001-10-F    TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-10-F).
EG1117     ADD LENGTH OF CRURY1-COR0001-10-F  TO WRK-POSICAO WRK-TAM.
EG1117***
EG1117     MOVE CRURY1-COR0001-11      TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-11).
EG1117     ADD LENGTH OF CRURY1-COR0001-11    TO WRK-POSICAO WRK-TAM.
EG1117***
EG1117     MOVE ALL '*'                TO CRURY1-GP-COR0001-PARCL
EG1117     MOVE CRURY1-COR0001-12      TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-12).
EG1117     ADD LENGTH OF CRURY1-COR0001-12    TO WRK-POSICAO WRK-TAM.
EG1117***
7C5.10*----MOVE-CRURY1-COR0001-13      TO-WRK-B09A-SIS(WRK-POSICAO:
7C5.10*                                LENGTH-OF-CRURY1-COR0001-13).
7C5.10*----ADD-LENGTH OF-CRURY1-COR0001-13    TO-WRK-POSICAO WRK-TAM.
EG1117***
BR4.11****-MOVE ALL '*'                TO CRURY1-GP-COR0001-LIBRC
BR4.11****-MOVE CRURY1-COR0001-14      TO WRK-B09A-SIS(WRK-POSICAO:
BR4.11****-                            LENGTH OF CRURY1-COR0001-14).
BR4.11****-ADD LENGTH OF CRURY1-COR0001-14    TO WRK-POSICAO WRK-TAM.
EG1117***
EG1117     MOVE CRURY1-COR0001-15      TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-15).
EG1117     ADD LENGTH OF CRURY1-COR0001-15    TO WRK-POSICAO WRK-TAM.
EG1117***
BR4.11****-MOVE ALL '*'                TO CRURY1-GP-COR0001-DESCLASS
BR4.11****-MOVE CRURY1-COR0001-16      TO WRK-B09A-SIS(WRK-POSICAO:
BR4.11****-                            LENGTH OF CRURY1-COR0001-16).
BR4.11****-ADD LENGTH OF CRURY1-COR0001-16    TO WRK-POSICAO WRK-TAM.
EG1117***
QT1120     MOVE ALL '*'                TO CRURY1-GP-COR0001-DESCLASS
QT1120
QT1120     MOVE CRURY1-COR0001-16      TO WRK-B09A-SIS(WRK-POSICAO:
QT1120                                 LENGTH OF CRURY1-COR0001-16).
QT1120     ADD LENGTH OF CRURY1-COR0001-16    TO WRK-POSICAO WRK-TAM.
QT1120
BR0521***
BR0521     MOVE CRURY1-COR0001-17      TO WRK-B09A-SIS(WRK-POSICAO:
BR0521                                 LENGTH OF CRURY1-COR0001-17).
BR0521     ADD LENGTH OF CRURY1-COR0001-17    TO WRK-POSICAO WRK-TAM.
BR0521***
BR0521     MOVE CRURY1-COR0001-18      TO WRK-B09A-SIS(WRK-POSICAO:
BR0521                                 LENGTH OF CRURY1-COR0001-18).
BR0521     ADD LENGTH OF CRURY1-COR0001-18    TO WRK-POSICAO WRK-TAM.
BR0521***
BR0521     MOVE CRURY1-COR0001-19      TO WRK-B09A-SIS(WRK-POSICAO:
BR0521                                 LENGTH OF CRURY1-COR0001-19).
BR0521     ADD LENGTH OF CRURY1-COR0001-19    TO WRK-POSICAO WRK-TAM.
BR0521***
BR0521     MOVE CRURY1-COR0001-20      TO WRK-B09A-SIS(WRK-POSICAO:
BR0521                                 LENGTH OF CRURY1-COR0001-20).
BR0521     ADD LENGTH OF CRURY1-COR0001-20    TO WRK-POSICAO WRK-TAM.
BR0521***
WP1021     MOVE CRURY1-COR0001-21      TO WRK-B09A-SIS(WRK-POSICAO:
WP1021                                 LENGTH OF CRURY1-COR0001-21).
WP1021     ADD LENGTH OF CRURY1-COR0001-21    TO WRK-POSICAO WRK-TAM.
TESTEM     MOVE '7000-TRATAR-CONTINUACAO'
TESTEM                                     TO WRK-PARAGRAFO.
TESTEM     MOVE SPACES                     TO WRK-RLIN-MSGEM-TEXT.
TESTEM     MOVE CRURY1-COR0001-16
TESTEM                                     TO WRK-RLIN-MSGEM-TEXT.
TESTEM     PERFORM 9500-ERRO-LIVRE.
QT1120***
BR0521
STE002* --- ALTERACAO INC0467861
STE002* --- ENVIO DOS INDICADORES - CAR
STE002     IF PREDC-JURO-AMBTL        OF CRURB008
.                                     GREATER ZEROS
.             MOVE  PREDC-JURO-AMBTL  OF CRURB008
.                                     TO WRK-03V2-COM-S
.             MOVE  WRK-03V2-SEM-S
.                                     TO WRK-PERC-CAR-N
.             MOVE  WRK-PERCENTUAL-CAR-X
.                                     TO CRURY1-PERC-BONUS-CAR
.          ELSE
.             MOVE  ALL '*'           TO CRURY1-PERC-BONUS-CAR
.          END-IF.
.
7C5.10     MOVE  ALL '*'              TO CRURY1-PERC-BONUS-SUST-X.
.
.     *----IF-CINDCD-CONFM-IBAMA      OF-CRURB008
.     *                               GREATER-ZEROS
.     *       MOVE--CINDCD-CONFM-IBAMA
.     *         OF--CRURB008
.     *         TO--WRK-INDICADOR-TELA
.     *       PERFORM-5051-TRATAR-DE-PARA
.     *       MOVE--WRK-INDICADOR-TELA-X
.     *                               TO-CRURY1-IND-CONF-IBAMA
.     *   -ELSE
.     *       MOVE--ALL-'*'           TO-CRURY1-IND-CONF-IBAMA
.     *   -END-IF.
.     *
.     *    IF-CINDCD-CONFM-CSVAO      OF-CRURB008
.     *                               GREATER-ZEROS
.     *       MOVE--CINDCD-CONFM-CSVAO
.     *         OF--CRURB008
.     *         TO--WRK-INDICADOR-TELA
.     *       PERFORM-5051-TRATAR-DE-PARA
.     *       MOVE--WRK-INDICADOR-TELA-X
.     *                               TO-CRURY1-IND-CONF-UC
.     *   -ELSE
.     *       MOVE--ALL '*'           TO-CRURY1-IND-CONF-UC
.     *   -END-IF.
.     *
.     *   -IF-CINDCD-CONFM-FLORS      OF-CRURB008
.     *                               GREATER-ZEROS
.     *       MOVE--CINDCD-CONFM-FLORS
.     *         OF--CRURB008
.     *         TO--WRK-INDICADOR-TELA
.     *       PERFORM-5051-TRATAR-DE-PARA
.     *       MOVE--WRK-INDICADOR-TELA-X
.     *                               TO-CRURY1-IND-CONF-FLOR-PUBL
.     *   -ELSE
.     *       MOVE--ALL-'*'           TO-CRURY1-IND-CONF-FLOR-PUBL
.     *   -END-IF.
.
.          MOVE CRURY1-COR0001-21A     TO WRK-B09A-SIS(WRK-POSICAO:
.                                      LENGTH OF CRURY1-COR0001-21A).
.          ADD  LENGTH OF CRURY1-COR0001-21A TO WRK-TAM.
STE002     ADD  LENGTH OF CRURY1-COR0001-21A TO WRK-POSICAO.
TC5.10***
7C2512     MOVE ALL '*'                TO CRURY1-GP-COR0001-CONFDD
.          MOVE CRURY1-COR0001-21B     TO WRK-B09A-SIS(WRK-POSICAO:
.                                      LENGTH OF CRURY1-COR0001-21B).
.          ADD LENGTH OF CRURY1-COR0001-21B   TO WRK-POSICAO WRK-TAM.
7C5.10***
TC5.10***
.          MOVE CRURY1-COR0001-21C     TO WRK-B09A-SIS(WRK-POSICAO:
.                                      LENGTH OF CRURY1-COR0001-21C).
.          ADD LENGTH OF CRURY1-COR0001-21C   TO WRK-POSICAO WRK-TAM.
7C5.10***
TC5.10***
.          MOVE CRURY1-COR0001-21D     TO WRK-B09A-SIS(WRK-POSICAO:
.                                      LENGTH OF CRURY1-COR0001-21D).
.          ADD LENGTH OF CRURY1-COR0001-21D   TO WRK-POSICAO WRK-TAM.
7C5.10***
STE002*
EG1117     MOVE CRURY1-COR0001-22      TO WRK-B09A-SIS(WRK-POSICAO:
EG1117                                 LENGTH OF CRURY1-COR0001-22).
EG1117     ADD LENGTH OF CRURY1-COR0001-22    TO WRK-POSICAO WRK-TAM.
MJ0618*
EG1117     .
EG1117*----------------------------------------------------------------*
EG1117 7000-99-FIM.                    EXIT.
EG1117*----------------------------------------------------------------*
EG1117*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PARA TRATAR RETORNO                         *
      *----------------------------------------------------------------*
       8300-TRATAR-RETORNO             SECTION.
      *----------------------------------------------------------------*
      *
           MOVE ZEROS                  TO CRURWIY1-COD-RETORNO
           MOVE '0000'                 TO CRURWIY1-COD-ERRO
           MOVE 'CRUR0044'             TO CRURWIY1-COD-MENSAGEM
           .
      *
      *----------------------------------------------------------------*
       8300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO DB2.                         *
      *----------------------------------------------------------------*
       9000-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-DB2               TO TRUE
           MOVE 12                     TO CRURWIY1-COD-RETORNO
           MOVE '0180'                 TO CRURWIY1-COD-ERRO
           MOVE 'CRUR0042'             TO CRURWIY1-COD-MENSAGEM
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE
           MOVE SQLCA                  TO FRWKGDB2-SQLCA
           MOVE SQLCODE                TO FRWKGDB2-SQLCODE
           MOVE WRK-COPY-DB2           TO WRK-BLOCO-INFO-ERRO
      *
           PERFORM 9999-API-ERROS
           .
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DE ERRO MODULO                          *
      *----------------------------------------------------------------*
       9300-ERRO-MODULO                SECTION.
      *----------------------------------------------------------------*
      *
           SET ERRO-MODULO             TO TRUE
      *
           MOVE 16                     TO CRURWIY1-COD-RETORNO
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-COPY-MOD(1:35)     TO WRK-BLOCO-INFO-ERRO(1:35)
      *
           PERFORM 9999-API-ERROS
           .
      *
      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DE ERRO CICS.                           *
      *----------------------------------------------------------------*
       9400-ERRO-CICS                  SECTION.
      *----------------------------------------------------------------*
      *
           SET ERRO-CICS               TO TRUE
      *
           MOVE 16                     TO CRURWIY1-COD-RETORNO
           MOVE '0190'                 TO CRURWIY1-COD-ERRO
           MOVE 'CRUR0045'             TO CRURWIY1-COD-MENSAGEM
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGCIC-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE EIBFN                  TO FRWKGCIC-EIBFN
           MOVE EIBRCODE               TO FRWKGCIC-EIBRCODE
           MOVE EIBRSRCE               TO FRWKGCIC-EIBRSRCE
           MOVE EIBRESP                TO FRWKGCIC-EIBRESP
           MOVE EIBRESP2               TO FRWKGCIC-EIBRESP2
           MOVE EIBTASKN               TO FRWKGCIC-EIBTASKN
           MOVE WRK-COPY-CICS(1:41)    TO WRK-BLOCO-INFO-ERRO(1:41)
      *
           PERFORM 9999-API-ERROS
           .
      *
      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
TESTEM*                                                                *
TESTEM*----------------------------------------------------------------*
TESTEM 9500-ERRO-LIVRE                 SECTION.
TESTEM*----------------------------------------------------------------*
TESTEM*                                                                *
TESTEM     SET  ERRO-LIVRE             TO TRUE
TESTEM*
TESTEM     MOVE WRK-RLIN-MSGEM-TEXT    TO FRWKGLIV-PARAMETROS
TESTEM     MOVE 'CRUR3CY1'             TO FRWKGHEA-NOME-PROGRAMA
TESTEM     MOVE WRK-PARAGRAFO          TO FRWKGHEA-IDEN-PARAGRAFO
TESTEM     MOVE FRWKGLIV-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
TESTEM     MOVE 'CRUR1421'             TO FRWKGLIV-COD-MENSAGEM
TESTEM     MOVE 1                      TO FRWKGLIV-IDIOMA
TESTEM     MOVE WRK-COPY-LIVRE
TESTEM                       (1:LENGTH OF WRK-COPY-LIVRE)
TESTEM                                 TO WRK-BLOCO-INFO-ERRO
TESTEM*                                                                *
TESTEM     EXEC CICS LINK
TESTEM         PROGRAM  (WRK-FRWK1999)
TESTEM         COMMAREA (WRK-AREA-ERRO)
TESTEM         LENGTH   (LENGTH OF WRK-AREA-ERRO)
TESTEM         NOHANDLE
TESTEM     END-EXEC
TESTEM*
TESTEM     IF (EIBRESP NOT EQUAL DFHRESP(NORMAL))
TESTEM         CONTINUE
TESTEM     END-IF
TESTEM     .
TESTEM*----------------------------------------------------------------*
TESTEM 9500-99-FIM.                    EXIT.
TESTEM*----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA CHAMADA DA API DE ERROS                            *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
               PROGRAM (WRK-FRWK1999)
               COMMAREA (WRK-AREA-ERRO)
               LENGTH (LENGTH OF WRK-AREA-ERRO)
               NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF
           PERFORM 3000-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
