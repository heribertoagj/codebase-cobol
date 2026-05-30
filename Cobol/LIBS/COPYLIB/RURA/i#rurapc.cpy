      *****************************************************************
      *   ANTIGO:I#RURACF / NOVO:I#RURAJC (MULTIBANCO)                *
      *               CADASTRO DO CREDITO RURAL - COBOL               *
      *              INC  ****  RRCBCADA  ****                   *    *
      *****************************************************************
WP1021*================================================================*
.     *  ANALISTA......:   WIPRO                                       *
.     *  DATA..........:   OUTUBRO/2021                                *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.03                       *
.     *                    FORMATACAO DOS CAMPOS                       *
WP1021*================================================================*
TCS001*================================================================*
TCS001* ALTERACOES..: TCS001                                           *
TCS001* NOME........: TCS                                              *
TCS001* DATA........: 29/11/2021                                       *
TCS001* DESCRICAO...: GRAVAR CAMPOS NOVOS LEGADO PARA ATENDER DEMANDA  *
TCS001*               TAXA JUROS CONFORME FORMA PAGAMENTO E ORIGEM     *
TCS001*               RECURSOS.                                        *
TCS001*================================================================*
      *================================================================*
B0923 * ANALISTA REPONSAVEL: BELGA JUNIOR - FOURSYS                    *
B0923 * DATA...............: SETEMBRO - 2023                           *
B0923 * OBJETIVOS..........: RESOLUCAO 5.078 - REDUTOR DE TAXA - CAR   *
      *================================================================*

      *****************************************************************
      *
       01  N110RURA.
           05  FILLER1            PIC 9(5)  COMP-3.
           05  N110CHAV           PIC 9(9)  COMP-3.
           05  N110ORDE           PIC 9(3)  COMP-3.
      *
      *    ****  REGISTRO TIPO 00  ****
      *
           05  N110RG00.
               10  N110TP00       PIC 9(3)  COMP-3.
               10  N110LOTE.
                   15  N110LPRP   PIC 9(15) COMP-3.
                   15  N110LTOT   PIC 9(13)V99 COMP-3.
               10  N110CONS.
                   15  N110CPRP   PIC 9(15) COMP-3.
                   15  N110CTOT   PIC 9(13)V99 COMP-3.
               10  N110INCO.
                   15  N110IPRP   PIC 9(15) COMP-3.
                   15  N110ITOT   PIC 9(13)V99 COMP-3.
               10  N110SUBC       PIC 9(5)  COMP-3.
               10  N110DRG0       PIC 9(9)  COMP-3.
               10  FILLER         PIC X(271).
      *
      *    ****   REGISTRO TIPO 10   ****
      *
           05  N110RG10  REDEFINES  N110RG00.
               10  N110TP10       PIC 9(3)  COMP-3.
               10  N110CONT       PIC X(16).
               10  N110C006       PIC 9(13)  COMP-3.
               10  N110C120       PIC 9(13)  COMP-3.
               10  N110C005       PIC X(40).
               10  N110C010       PIC X.
               10  N110C011       PIC 9(15) COMP-3.
               10  N110C009       PIC 9(03) COMP-3.
               10  N110C167       PIC X(40).
               10  N110C168       PIC X.
               10  N110C169       PIC 9(15) COMP-3.
               10  N110C170       PIC X(40).
               10  N110C171       PIC X.
               10  N110C172       PIC 9(15) COMP-3.
               10  N110C001       PIC 9(5)  COMP-3.
               10  N110NUER       PIC 9(7)  COMP-3.
               10  N110NDEP       PIC 9(7)  COMP-3.
               10  N110TPDP       PIC 9(3)  COMP-3.
               10  N110NBCO       PIC 9(5)  COMP-3.
               10  N110SITC       PIC 9(1).
               10  N110DFOR       PIC 9(9)  COMP-3.
               10  N110DAPR       PIC 9(9)  COMP-3.
               10  N110DEXC       PIC 9(9)  COMP-3.
               10  N110DADC       PIC 9(9)  COMP-3.
               10  FILLER         PIC X(111).
      *
      *   ****   REGISTRO TIPO 20  ****
      *
           05  N110RG20  REDEFINES  N110RG00.
               10  N110TP20       PIC 9(3)  COMP-3.
               10  N110LOCA.
                   15  N110C060   PIC 9(13) COMP-3.
                   15  N110C530   PIC 9(3)  COMP-3.
                   15  N110C520   PIC 9(3)  COMP-3.
                   15  N110C580   PIC 9(02).
                   15  N110C500   PIC X.
                   15  N110C560   PIC X.
               10  N110PROD.
                   15  N110C008   PIC 9(5)  COMP-3.
                   15  N110C007   PIC 9(13)V99 COMP-3.
                   15  N110C801   PIC 9(7)  COMP-3.
               10  N110C059       PIC 9(13)V99 COMP-3.
               10  N110C093       PIC X.
               10  N110C058       PIC X(40).
B0923          10  N110CIREDT     PIC X(01).
B0923          10  N110CPTXRED    PIC S9(3)V9(2) COMP-3.
               10  FILLER         PIC X(244).
      *
      *    ****   REGISTRO TIPO 30   ****
      *
           05  N110RG30  REDEFINES  N110RG00.
               10  N110TP30       PIC 9(3)  COMP-3.
               10  FILLER         PIC X(14).
               10  N110C099       PIC 9(3)  COMP-3.
               10  N110AGAV       PIC 9(7)  COMP-3.
               10  N110CTAV       PIC 9(13)  COMP-3.
               10  N110C094       PIC X(40).
               10  N110AVPE       PIC X.
               10  N110C095       PIC 9(15) COMP-3.
               10  N110SGDR       PIC 9(03) COMP-3.
               10  FILLER         PIC X(249).
      *
      *    ****   REGISTRO TIPO 40  ****
      *
           05  N110RG40  REDEFINES  N110RG00.
               10  N110TP40       PIC 9(3)  COMP-3.
               10  N110C069       PIC 9(3)  COMP-3.
               10  N110C070       PIC 9(3)  COMP-3.
               10  N110C072       PIC 9(15) COMP-3.
               10  N110C073       PIC 9(13)V99 COMP-3.
               10  N110C071       PIC X(20).
               10  N110C194       PIC X(40).
               10  FILLER         PIC X(247).
      *
      *    ****   REGISTRO TIPO 50  ****
      *
           05  N110RG50  REDEFINES  N110RG00.
               10  N110TP50       PIC 9(3)  COMP-3.
               10  N110OR01.
                   15  N110C096   PIC 9(3)  COMP-3.
                   15  N110C100   PIC X.
                   15  N110DTOR   PIC 9(9)  COMP-3.
               10  N110OR02.
                   15  N110X096   PIC 9(3)  COMP-3.
                   15  N110X100   PIC X.
                   15  N110XTOR   PIC 9(9)  COMP-3.
               10  N110OR03.
                   15  N110Y096   PIC 9(3)  COMP-3.
                   15  N110Y100   PIC X.
                   15  N110YTOR   PIC 9(9)  COMP-3.
               10  N110OR04.
                   15  N110Z096   PIC 9(3)  COMP-3.
                   15  N110Z100   PIC X.
                   15  N110ZTOR   PIC 9(9)  COMP-3.
               10  N110RECO.
                   15  N110RNUM   PIC 9(9)  COMP-3.
                   15  N110RIDE   PIC X.
               10  N110REFI.
                   15  N110C750   PIC 9(3)  COMP-3.
                   15  N110C751   PIC 9(3)  COMP-3.
                   15  N110C754   PIC 9(9)  COMP-3.
                   15  N110C752   PIC 9(5)  COMP-3.
                   15  N110C753   PIC 9(5)  COMP-3.
               10  N110C101       PIC 9(9)  COMP-3.
               10  N110C097       PIC X.
               10  N110C098       PIC X.
               10  N110C012       PIC 9(3)  COMP-3.
               10  N110C495       PIC 9(3)  COMP-3.
               10  N110C013       PIC 9(3)  COMP-3.
               10  N110C014       PIC 9(3)  COMP-3.
               10  N110C015       PIC 9(13)V99 COMP-3.
               10  N110C016       PIC 9(3)  COMP-3.
               10  N110C017       PIC 9(13)V99 COMP-3.
               10  N110C018       PIC 9(3)  COMP-3.
               10  N110C019       PIC 9(3)  COMP-3.
               10  N110C020       PIC 9(13) COMP-3.
               10  N110C056       PIC 9(13)V99 COMP-3.
               10  N110C057       PIC 9(13)V99 COMP-3.
               10  N110C800       PIC 9(13)V99 COMP-3.
               10  N110C570       PIC 9(3)  COMP-3.
               10  N110C091       PIC XX.
               10  N110DATA.
                   15  N110DTVF   PIC 9(9)  COMP-3.
                   15  N110C004   PIC 9(9)  COMP-3.
                   15  N110DTOP   PIC 9(9)  COMP-3.
                   15  N110DTRG   PIC 9(9)  COMP-3.
               10  N110C130       PIC 9(13)V99 COMP-3.
               10  N110C160       PIC 9(13)V99 COMP-3.
               10  N110C021       PIC 9(5)  COMP-3.
               10  N110C022       PIC 9(3)  COMP-3.
               10  N110C023       PIC 9(2).
               10  N110C024       PIC 9(11) COMP-3.
               10  N110C025       PIC 9(11) COMP-3.
               10  N110C190       PIC 9(13)V99 COMP-3.
               10  N110C191       PIC 9(13)V99 COMP-3.
               10  N110C192       PIC 9(3)  COMP-3.
               10  N110C193       PIC 9(3)  COMP-3.
PW2875         10  N110DI4V-X     PIC X(5).
PW2875         10  N110DI4V       REDEFINES  N110DI4V-X
                                  PIC 9(9)  COMP-3.
               10  N110CONA       PIC 9(4)  COMP-3.
               10  N110CONJ       PIC 9(5)  COMP-3.
               10  N110SCON       PIC 9(9)  COMP-3.
               10  N110MODS       PIC 9(3)  COMP-3.
               10  N110VARS       PIC 9(3)  COMP-3.
               10  N110CESS       PIC 9(3)  COMP-3.
               10  N110AGRC       PIC 9(3)  COMP-3.
               10  N110INTG       PIC 9(3)  COMP-3.
               10  N110IRRI       PIC 9(3)  COMP-3.
               10  N110GRAS       PIC 9(1).
               10  N110CULS       PIC 9(3)  COMP-3.
               10  N110CICP       PIC 9(3)  COMP-3.
               10  N110TCED       PIC 9(3)  COMP-3.
               10  N110CNSC       PIC 9(1).
               10  N110ZONS       PIC 9(1).
               10  N110RENB       PIC S9(15)V99
                                            COMP-3.
               10  N110CINT       PIC 9(1).
               10  N110CICS       PIC 9(3)  COMP-3.
               10  N110RNUN       PIC 9(11) COMP-3.
               10  N110RNUS       PIC 9(11) COMP-3.
WP1021         10  N110PRRE       PIC S9(3)V9(2) COMP-3.
WP1021         10  N110CTSA       PIC S9(2) COMP-3.
WP1021         10  N110CCTV       PIC S9(2) COMP-3.
WP1021         10  N110CMSL       PIC S9(2) COMP-3.
PR1203         10  N110CTCL       PIC S9(2) COMP-3.
******* FOI NECESSARIO AJUSTAR O TAMANHO DO FILLER POIS O GRUPO
******* USADO NA REDEFINICAO POSSUI 329 BYTES E COM ESTE FILLER
******* ESTE GRUPO POSSUIA 324 BYTES
PR1203*        10  F1LLER         PIC X(52).
PR1203         10  FILLER         PIC X(55).
      *
      *    ****   REGISTRO TIPO 60  ****
      *
           05  N110RG60  REDEFINES  N110RG00.
               10  N110TP60       PIC 9(3)  COMP-3.
               10  N110TSEG       PIC 9(1).
               10  N110CSEG       PIC 9(4)  COMP-3.
               10  N110ALQP       PIC S9(3)V9999
                                            COMP-3.
               10  N110VCOB       PIC S9(15)V99
                                            COMP-3.
               10  N110NAPO       PIC X(20).
               10  N110DIVS       PIC 9(9)  COMP-3.
               10  N110DTVS       PIC 9(9)  COMP-3.
               10  N110VADC       PIC S9(13)V99
                                            COMP-3.
               10  N110FCBS       PIC 9(1).
               10  FILLER         PIC X(271).
WP1021*
WP1021*    ****   REGISTRO TIPO 70  ****
WP1021*
WP1021     05  N110RG70  REDEFINES  N110RG00.
WP1021         10  N110TP70       PIC 9(3)  COMP-3.
WP1021         10  N110CCOR       PIC X(80).
WP1021         10  N110CREM       PIC X(80).
WP1021         10  FILLER         PIC X(167).
WP1021*
WP1021*    ****   REGISTRO TIPO 71  ****
WP1021*
WP1021     05  N110RG71  REDEFINES  N110RG00.
WP1021         10  N110TP71       PIC 9(3)  COMP-3.
WP1021         10  N110EUCO.
WP1021             15  N110EUCL   PIC S9(4) COMP.
WP1021             15  N110EUCT   PIC X(256).
WP1021         10  FILLER         PIC X(069).
WP1021*
WP1021*    ****   REGISTRO TIPO 72  ****
WP1021*
WP1021     05  N110RG72  REDEFINES  N110RG00.
WP1021         10  N110TP72       PIC 9(3)  COMP-3.
WP1021         10  N110EURE.
WP1021             15  N110EURL   PIC S9(4) COMP.
WP1021             15  N110EURT   PIC X(256).
WP1021         10  FILLER         PIC X(069).
      *
      *          REGISTRO DO TIPO 100
      *
           05  N110R100  REDEFINES  N110RG00.
               10  N110T100       PIC 9(3)  COMP-3.
               10  N110ESPC       PIC X.
               10  N110COMA       PIC X(08).
               10  N110C043       PIC 9(09) COMP-3.
               10  N110DTCT       PIC 9(09) COMP-3.
               10  N110DTDP       PIC 9(09) COMP-3.
               10  N110C995       PIC 9(03) COMP-3.
               10  N110OREV       PIC X.
               10  N110DBCR       PIC X.
               10  N110NUME       PIC 9(07) COMP-3.
               10  N110C044       PIC 9(13)V99 COMP-3.
               10  N110PRAC       PIC 9(13)V99 COMP-3.
               10  N110VRAX       PIC 9(13)V99 COMP-3.
               10  N110C045       PIC 9(13)V99 COMP-3.
               10  N110RPAC       PIC 9(13)V99 COMP-3.
               10  N110C104       PIC 9(03) COMP-3.
               10  N110C105       PIC X(02).
               10  N110C106       PIC 9(03) COMP-3.
               10  N110AGCR       PIC 9(05) COMP-3.
               10  N110RZCR       PIC 9(05) COMP-3.
               10  N110CTCR       PIC 9(13) COMP-3.
               10  N110PRPO       PIC 9(13)V99 COMP-3.
               10  N110PRPA       PIC 9(13)V99 COMP-3.
               10  N110PRMO       PIC 9(13)V99 COMP-3.
               10  N110PRMA       PIC 9(13)V99 COMP-3.
               10  N110PGPO       PIC 9(13)V99 COMP-3.
               10  N110PGPA       PIC 9(13)V99 COMP-3.
               10  N110PGJO       PIC 9(13)V99 COMP-3.
               10  N110PGJA       PIC 9(13)V99 COMP-3.
               10  N110PGCO       PIC 9(13)V99 COMP-3.
               10  N110PGCA       PIC 9(13)V99 COMP-3.
               10  N110PGAO       PIC 9(13)V99 COMP-3.
               10  N110PGAA       PIC 9(13)V99 COMP-3.
               10  N110PGLO       PIC 9(13)V99 COMP-3.
               10  N110PGLA       PIC 9(13)V99 COMP-3.
               10  N110PGMO       PIC 9(13)V99 COMP-3.
               10  N110PGMA       PIC 9(13)V99 COMP-3.
               10  N110PGRO       PIC 9(13)V99 COMP-3.
               10  N110PGRA       PIC 9(13)V99 COMP-3.
               10  N110PGIO       PIC 9(13)V99 COMP-3.
               10  N110PGIA       PIC 9(13)V99 COMP-3.
               10  N110M043       PIC 9(09) COMP-3.
               10  N110M995       PIC 9(03) COMP-3.
               10  N110MDCR       PIC X.
               10  N110MNUM       PIC 9(07) COMP-3.
               10  N110RPCO       PIC 9(13)V99 COMP-3.
               10  N110RPCA       PIC 9(13)V99 COMP-3.
TCS001         10  N110VRAXF      PIC S9(3)V9(2) COMP-3.
TCS001         10  N110VRAXD      PIC S9(3)V9(4) COMP-3.
TCS001         10  N110VRAXB      PIC S9(3)V9(4) COMP-3.
TCS001         10  FILLER         PIC X(37).
      *
      *    ****  AREA COMUM A TODOS OS REGISTROS  ****
      *
           05  N110ARCO.
               10  N110CENS       PIC 9(4).
               10  N110ACER.
                   15  N110DTAC   PIC 9(9)  COMP-3.
                   15  N110CDAC   PIC 9(3)  COMP-3.

      *
      *          DEFINICAO DOS CAMPOS DO REGISTRO
      *
      *
      *   CHAVE DO CADASTRO
      *
      *   TIPO DE REGISTRO  00
      *
      *   TOTAIS DOS CABECA-DE-LOTES
      *   QUANTIDADE DE PROPOSTAS DO LOTE
      *   TOTAL DO VR. FINANCIADO DO LOTE
      *   TOTAIS DOS CONTRATOS CONSISTENTES
      *   QUANTIDADE DE PROPOSTAS CONSISTENTES
      *   TOTAL DOS FINANCIAMENTOS CONSISTENTES
      *   TOTAIS DOS CONTRATOS INCONSISTENTES
      *   QUANTIDADE DE PROPOSTAS INCONSISTENTES
      *   TOTAL - FINANCIAMENTOS INCONSISTENTES
      *   CODIGO DE JUNCAO DO SUB-CENTRO
      *   DATA DE REGISTRO
      *   RESERVA
      *
      *   TIPO DE REGISTRO  10
      *
      *   CONTROLES DO CONTRATO
      *   CONTA CORRENTE
      *   CONTA VINCULADA
      *   NOME DO MUTUARIO    - 1
      *   TIPO DE PESSOA      - 1
      *   CGC/CPF DO MUTUARIO - 1
      *   TIPO DE PRODUTOR
      *   NOME DO MUTUARIO    - 2
      *   TIPO DE PESSOA      - 2
      *   CGC/CPF DO MUTUARIO - 2
      *   NOME DO MUTUARIO    - 3
      *   TIPO DE PESSOA      - 3
      *   CGC/CPF DO MUTUARIO - 3
      *   AGENCIA OPERADORA
      *   NUMERO DO EMPRESTIMO/CEDULA
      *   ULTIMO NUMERO DE DEPENDENTE
      *   ULTIMO TIPO DE DEPENDENTE
      *   BANCO
      *   SITUACAO DO CONTRATO
      *      0 FORMALIZADO
      *      1 APROVADO
      *      9 EXCLUIDO
      *   DATA FORMALIZACAO DA PROPOSTA
      *   DATA APROVACAO DA PROPOSTA
      *   DATA EXCLUSAO SICOR
      *   DATA COBRANCA ADICIONAL PROAGRO
      *   RESERVA
      *
      *
      *   TIPO DO REGISTRO 20
      *
      *   LOCALIZACAO DO IMOVEL FINANCIADO
      *   CODIGO DO MUNICIPIO COM DIGITO
      *   MICRO REGIAO
      *   CENTRO
      *   U. F.  - ESTADO
      *   REGIAO
      *   EMERGENCIA
      *   PRODUCAO AGROPECUARIA ANUAL
      *   ANO DA PRODUCAO AGROPECUARIA
      *   VALOR DA PROD. AGROPECUARIA ANUAL
      *   QUANTIDADE DE MVR FINANCIADA
      *   AREA TOTAL
      *   NUMERO DE INDENIZACOES
      *   IMOVEL BENEFICIADO
      *   RESERVA
      *
      *   TIPO DE REGISTRO  30
      *
      *   RESERVA
      *   CODIGO DE FIANCA/AVAL
      *   AGENCIA DO AVALISTA/CONVENIADA
      *   CONTA CORRENTE DO AVALISTA/CONVENIADA
      *   NOME DO AVALISTA
      *   TIPO DE PESSOA DO AVALISTA
      *   CGC/CPF DO AVALISTA
      *   COD SEGURADORA QDO TEM FIANCA/AVAL
      *   RESERVA
      *
      *   TIPO DO REGISTRO 40
      *
      *   ESPECIE DE TITULO
      *   TIPO DE GARANTIA
      *   QUANTIDADE DE BENS VINCULADOS
      *   VALOR DOS BENS VINCULADOS
      *   ESPECIE DE BENS VINCULADOS
      *   LOCALIZACAO DE BENS VINCULADOS
      *   RESERVA
      *
      *   TIPO DO REGISTRO 50
      *
      *   PRIMEIRA ORIGEM DE RECURSOS
      *   ORIGEM DE RECURSOS
      *   RECURSOS PROPRIOS LIVRES (IDENT)
      *   DATA DA ORIGEM DE RECURSOS
      *   SEGUNDA ORIGEM DE RECURSOS
      *   ORIGEM DE RECURSOS
      *   RECURSOS PROPRIOS LIVRES (IDENT)
      *   DATA DA ORIGEM DE RECURSOS
      *   TERCEIRA ORIGEM DE RECURSOS
      *   ORIGEM DE RECURSOS
      *   RECURSOS PROPRIOS LIVRES (IDENT)
      *   DATA DA ORIGEM DE RECURSOS
      *   QUARTA ORIGEM DE RECURSOS
      *   ORIGEM DE RECURSOS
      *   RECURSOS PROPRIOS LIVRES (IDENT)
      *   DATA DA ORIGEM DE RECURSOS
      *   INFORMACOES PARA PROJETO RECOR - BACEN
      *   NUMERO DO RECOR
      *   IDENTIFICACAO
      *   INFORMACOES S/ REFINANCIAMENTO
      *   PLANO DE REFINANCIAMENTO
      *   SUB-PROGRAMA DE REFINANCIAMENTO
      *   DATA DO REFINANCIAMENTO
      *   TAXA DE REFINANCIAMENTO
      *   NUMERO DA CARTA DE REFINANCIAMENTO
      *   NUMERO DA AUTORIZACAO
      *   POSTO AVANCADO
      *   FISCALIZACAO
      *   ATIVIDADE
      *   APLICACAO
      *   FINALIDADE
      *   EMPRENDIMENTO
      *   QUANTIDADE FINANCIADA
      *   UNIDADE
      *   QUANTIDADE DE PRODUCAO PROVAVEL
      *   UNIDADE
      *   FAIXA DE PRODUTIVIDADE
      *   SAFRA OU PERIODO DE PRODUCAO
      *   TOTAL FINANCIADO
      *   TOTAL - RECURSOS PROPRIOS DO MUTUARIO
      *   VBC  - VALOR BASICO DE CUSTEIO
      *   LIMITE DE FINANCIAMENTO
      *   CODIGO DE INCIDENCIA
      *   DATAS
      *   DATA DE VENCIMENTO FINAL
      *   DATA DA CEDULA
      *   DATA DA OPERACAO/LIBERACAO DO CREDITO
      *   DATA DE REGISTRO DA OPERACAO
      *   VALOR DE COBERTURA/BASE DO PROAGRO
      *   TOTAL COBERTO PROAG-OUTRAS OPERAC.OTNS
      *   EGF = CLASSIFICACAO DO PRODUTO
      *   EGF = CODIGO DA EMBALAGEM
      *   EGF = CATEGORIA DO MUTUARIO
      *   EGF = CAPACIDADE DE PRODUCAO
      *   EGF = OUTROS EGFS.
      *   EGF = PRECO MINIMO DO PRODUTO
      *   EGF = PRECO MINIMO DA EMBALAGEM
      *   EGF = % LIM. DE PRODUCAO PESO/CAPACID.
      *   EGF = % LIM. DO PRODUTO NO FINANCIAMEN
      *   DATA DE IMPRESSAO DAS 4 VIAS DO CONTRATO
      *   CONSULTA CONE - ANO
      *   CONSULTA CONE - CODIGO JUNCAO
      *   CONSULTA CONE - SEQUENCIA
      *   MODALIDADE SICOR
      *   VARIEDADE SICOR
      *   CESTA SAFRA SICOR
      *   TIPO AGRICULTURA SICOR
      *   TIPO INTEGRACAO SICOR
      *   TIPO IRRIGACAO SICOR
      *   GRAO/SEMENTE SICOR
      *   TIPO CULTIVO SICOR
      *   CICLO PRODUCAO SICOR
      *   TIPO CEDULA SICOR
      *   CONSORCIO SIM OU NAO SICOR
      *   ZONEAMENTO SIM OU NAO SICOR
      *   RENDA BRUTA SICOR
      *   CULTURA INTERCALADA
      *   TIPO CICLO SICOR
      *   NUMERO REFERENCIA BACEN/RECOR
      *   NUMERO SICOR
      *   RESERVA
      *
      *   TIPO DO REGISTRO 60
      *
      *   AGENCIA OPERADORA
      *   NUMERO DO EMPRESTIMO
      *   ORDEM DO REGISTRO
      *   TIPO DE REGISTRO
      *   TIPO SEGURO
      *   CODIGO SEGURADORA
      *   % ALIQUOTA
      *   VALOR COBERTURA
      *   NUMERO APOLICE
      *   DATA INICIO VIGENCIA AAAAMMDD
      *   DATA TERMINO VIGENCIA AAAAMMDD
      *   VALOR ADICIONAL PROAGRO
      *   FORMA COBRANCA SEGURO
      *
      *   TIPO DO REGISTRO 100
      *
      *   ESPECIE (' '-NORMAL / '2'-MORA)
      *   CONTROLES DA PARCELA
      *   DATA DA PARCELA DE LIBERACAO/PAGAMENTO
      *   DATA DA CONTABILIZACAO
      *   DATA DE/PARA - NOS ACERTOS
      *   CODIGO DE LANCAMENTO
      *   ORDEM DOS EVENTOS
      *   DEBITO/CREDITO/LIB/PAG/ETC
      *   NUMERO DA PARCELA
      *   VALOR PRINCIPAL CONTABILIZADO - LIB./PAGTO.
      *   VALOR PRINCIPAL ACERTADO
      *   VALOR AUXILIAR
      *   VALOR REC.PROPRIO DO MUTUARIO ORIGINAL
      *   VALOR REC.PROPRIO DO MUTUARIO CORRIGID
      *   % DA LIBERACAO C/ ORCADO/PORC. PRIM. PAGTO.
      *   MES DA LIBERACAO/DT. CEDULA 1 PAGTO.
      *   DIFERENCA DE DIAS/PRAZO ULTIMA LIB. - 1 PAGTO.
      *   AGENCIA DE CREDITO/DEBITO
      *   RAZAO DE CREDITO/DEBITO
      *   CONTA DE CREDITO/DEBITO
      *   VALOR PRINCIPAL CONTABILIZ. PROAGRO.
      *   VALOR PRINCIPAL ACERTADO    PROAGRO
      *   VALOR REC.PR.MUTUARIO ORIG. PROAGRO.
      *   VALOR REC.PR.MUTUARIO ACERT.PROAGRO
      *   PROAGRO PAGO ORIGINAL
      *   PROAGRO PAGO ACERTADO
      *   JUROS PAGO ORIGINAL
      *   JUROS PAGO ACERTADO
      *   CORRECAO PAGA ORIGINAL
      *   CORRECAO PAGA ACERTADO
      *   ASSIST.TECN. PAGA ORIGINAL
      *   ASSIST.TECN. PAGA ACERTADA
      *   CUSTAS PERICIAIS ORIGINAL
      *   CUSTAS PERICIAIS ACERTADO
      *   DESPESAS ORIGINAL
      *   DESPESAS ACERTADA
      *
      *
      *   I.O.F. PAGO ORIGINAL
      *   I.O.F. PAGO ACERTADO
      *   NUMERO DA PARCELA - RECURSOS EXTERNOS
      *   CODIGO DE LANCAMENTO DA MORA
      *   DEBITO/CREDITO-ENTR./BAIXA NA/DA MORA
      *   NUMERO DA PARCELA EM MORA
      *   CORRECAO REC. PROPRIOS PROAGRO ORIGINAL
      *   CORRECAO REC. PROPRIOS PROAGRO ACERTADA
      *   RESERVA
      *
      *   AREA COMUM A TODOS OS REGISTROS
      *
      *   CODIGO CENTRO REGIONAL      - RURA0450
      *   ACERTOS DO REGISTRO 10
      *   DATA DOS ACERTOS
      *   CODIGO DOS ACERTOS
