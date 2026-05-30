      *================================================================*
      * DESCRICAO..: CONSULTAR DADOS COMPLETO DA OPERACAO              *
      * COPYBOOK...: DCOMWOPC - BOOK DE ACESSO AO PROGRAMA DCOM3OPC    *
      *              (OBS. BASEADO NO BOOK I#DCOMXJ)                   *
      * DATA.......: 15/12/2024                                        *
      * AUTOR......: LUCIANDRA SILVEIRA - CAPGEMINI                    *
      * COMPONENTE.: DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      *-> DCOMWOPC-E-HULT-ATULZ                                        *
      *                    = INDICA O TIMESTAMP DA OPERACAO HISTORICA  *
      *   OBRIGATORIO QUANDO FUNCAO = 03 E LOCAL DA OPERACAO = "H"     *
      *                                                                *
      *-> DCOMWOPC-E-FUNCAO= INDICA BASE DB2 PRIORITARIA PARA PESQUISA *
      *   OBRIGATORIO      -->  01 = CONSULTAR OPERACAO TRANSITORIA    *
      *                    -->  02 = CONSULTAR OPERACAO OFICIAL        *
      *                    -->  03 = CONSULTAR OPERACAO HISTORICA      *
      *                    -->  04 = CONSULTAR OPERACAO PRIORIZANDO    *
      **                             A PESQUISA EM BASE TRANSITORIA,   *
      **                             OFICIAL E HISTORICA, NESSA ORDEM. *
      *                                                                *
      *-> DCOMWOPC-E-DESCRICOES                                        *
      *   OBRIGATORIO      = INDICA SE AS DESCRICOES SERAO RETORNADAS  *
      *                    --> "S" = SIM                               *
      *                    --> "N" = NAO                               *
      *================================================================*
      *
          05 DCOMWOPC-HEADER.
             10 DCOMWOPC-E-COD-LAYOUT   PIC  X(008) VALUE 'DCOMWOPC'.
             10 DCOMWOPC-E-TAM-LAYOUT   PIC  9(005) VALUE 01800.
      *
          05 DCOMWOPC-REGISTRO.
             10 DCOMWOPC-BLOCO-ENTRADA.
      *-->      DADOS DA OPERACAO
                15 DCOMWOPC-E-DANO-OPER-DESC        PIC  9(004).
                15 DCOMWOPC-E-NSEQ-OPER-DESC        PIC  9(009).
                15 DCOMWOPC-E-HULT-ATULZ            PIC  X(026).
      *-->      OPCAO DE PESQUISA
                15 DCOMWOPC-E-FUNCAO                PIC  9(002).
                15 DCOMWOPC-E-DESCRICOES            PIC  X(001).
                15 DCOMWOPC-E-RESERVA               PIC  X(059).
      *
             10 DCOMWOPC-BLOCO-SAIDA.
                15 DCOMWOPC-S-LOCAL-OPERACAO        PIC  X(001).
                15 DCOMWOPC-S-CPRODT                PIC  9(003).
                15 DCOMWOPC-S-IPRODT                PIC  X(060).
                15 DCOMWOPC-S-IABREV-PRODT          PIC  X(012).
                15 DCOMWOPC-S-CSPROD-DESC-COML      PIC  9(003).
                15 DCOMWOPC-S-ISPROD-DESC-COML      PIC  X(040).
                15 DCOMWOPC-S-IRSUMO-SPROD-DESC     PIC  X(010).
                15 DCOMWOPC-S-DANO-OPER-DESC        PIC  9(004).
                15 DCOMWOPC-S-NSEQ-OPER-DESC        PIC  9(009).
                15 DCOMWOPC-S-CCONTR-CONVE-DESC     PIC  9(009).
                15 DCOMWOPC-S-CCONTR-LIM-DESC       PIC  9(009).
                15 DCOMWOPC-S-CVRSAO-CONTR-CONVE    PIC  9(003).
                15 DCOMWOPC-S-NSMULA-OPER-DESC      PIC  9(009).
                15 DCOMWOPC-S-CCART-NORML-DESC      PIC  X(005).
                15 DCOMWOPC-S-DANO-BASE             PIC  9(004).
                15 DCOMWOPC-S-CDEPDC                PIC  9(005).
                15 DCOMWOPC-S-CSEQ-STUDO            PIC  9(009).
                15 DCOMWOPC-S-VOPER-DESC-COML       PIC  9(015)V9(002).
                15 DCOMWOPC-S-VTAC-OPER-DESC        PIC  9(015)V9(002).
                15 DCOMWOPC-S-PTX-JURO-MES          PIC  9(006)V9(007).
                15 DCOMWOPC-S-PTX-JURO-ANO          PIC  9(006)V9(007).
                15 DCOMWOPC-S-VTARIF-REG-TITLO      PIC  9(015)V9(002).
                15 DCOMWOPC-S-DVCTO-FNAL-OPER       PIC  X(010).
                15 DCOMWOPC-S-VJURO-OPER-DESC       PIC  9(015)V9(002).
                15 DCOMWOPC-S-VIOF-OPER-DESC        PIC  9(015)V9(002).
                15 DCOMWOPC-S-QTITLO-CHEQ-PCELA     PIC  9(009).
                15 DCOMWOPC-S-VLIQ-OPER-DESC        PIC  9(015)V9(002).
                15 DCOMWOPC-S-DINIC-OPER-DESC       PIC  X(010).
                15 DCOMWOPC-S-VOPER-DESC-APURA      PIC  9(015)V9(002).
                15 DCOMWOPC-S-QTITLO-OPER-APURA     PIC  9(009).
                15 DCOMWOPC-S-DVCTO-FNAL-APURA      PIC  X(010).
                15 DCOMWOPC-S-TOPER-DESC-APURA      PIC  9(003).
                15 DCOMWOPC-S-TMED-PONDE-OPER       PIC  9(003).
                15 DCOMWOPC-S-PMIN-TX-JURO-REGRA    PIC  9(003)V9(003).
                15 DCOMWOPC-S-PMAX-TX-JURO-REGRA    PIC  9(003)V9(003).
                15 DCOMWOPC-S-CSIT-DESC-COML        PIC  9(003).
                15 DCOMWOPC-S-IRSUMO-SIT-DESC       PIC  X(015).
                15 DCOMWOPC-S-HSIT-DESC-COML        PIC  X(026).
                15 DCOMWOPC-S-VLIBRC-DESC-COML      PIC  9(015)V9(002).
                15 DCOMWOPC-S-VLIBRC-ANTCP-OPER     PIC  9(015)V9(002).
                15 DCOMWOPC-S-CBCO                  PIC  9(003).
                15 DCOMWOPC-S-CAG-BCRIA             PIC  9(005).
                15 DCOMWOPC-S-CCTA-BCRIA-CLI        PIC  9(013).
                15 DCOMWOPC-S-CBCO-OPER-OPER        PIC  9(003).
                15 DCOMWOPC-S-CAG-OPER-OPER         PIC  9(005).
                15 DCOMWOPC-S-CTPO-DESC-COML        PIC  9(003).
                15 DCOMWOPC-S-ITPO-DESC-COML        PIC  X(030).
                15 DCOMWOPC-S-IRSUMO-TPO-DESC       PIC  X(010).
                15 DCOMWOPC-S-CELMTO-DESC-COML      PIC  9(003).
                15 DCOMWOPC-S-CINDCD-DIGTC-AG       PIC  X(001).
                15 DCOMWOPC-S-CINDCD-ISENC-OPER     PIC  X(001).
                15 DCOMWOPC-S-CINDCD-AUTRZ-OPER     PIC  X(001).
                15 DCOMWOPC-S-CINDCD-MODLD-INCL     PIC  X(001).
                15 DCOMWOPC-S-CCTRO-CUSTO           PIC  X(004).
                15 DCOMWOPC-S-CINDCD-APROV-CREDT    PIC  X(001).
                15 DCOMWOPC-S-CMEIO-ENTRD-DESC      PIC  9(003).
                15 DCOMWOPC-S-IRSUMO-MEIO-ENTRD     PIC  X(015).
                15 DCOMWOPC-S-CGARNT                PIC  9(003).
                15 DCOMWOPC-S-ITPO-GARNT            PIC  X(040).
                15 DCOMWOPC-S-QPCELA-OPER           PIC  9(005).
                15 DCOMWOPC-S-NAUTRZ-TRNSM-ESCR     PIC  9(007).
                15 DCOMWOPC-S-CINDCD-PGDOR-JURO     PIC  X(001).
                15 DCOMWOPC-S-CTPO-TX-JURO-DESC     PIC  9(003).
                15 DCOMWOPC-S-ITPO-TX-JURO-DESC     PIC  X(030).
                15 DCOMWOPC-S-IRSUMO-TPO-TX-JURO    PIC  X(015).
                15 DCOMWOPC-S-CSGL-TPO-TX-JURO      PIC  X(006).
                15 DCOMWOPC-S-PALIQT-IOF-DESC       PIC  9(003)V9(005).
                15 DCOMWOPC-S-PTX-CUSTO-MES         PIC  9(003)V9(006).
                15 DCOMWOPC-S-PTX-CUSTO-ANO         PIC  9(003)V9(006).
                15 DCOMWOPC-S-VJURO-CUSTO-OPER      PIC  9(015)V9(002).
                15 DCOMWOPC-S-VPSENT-OPER-DESC      PIC  9(015)V9(002).
                15 DCOMWOPC-S-CINDCD-VALDC-ROTRO    PIC  X(001).
                15 DCOMWOPC-S-DVALDC-ROTRO-ELMTO    PIC  X(010).
                15 DCOMWOPC-S-CFUNC-VALDC-ROTRO     PIC  9(009).
                15 DCOMWOPC-S-DULT-ADTTO-OPER       PIC  X(010).
                15 DCOMWOPC-S-CINDCD-MOTVO-ADTTO    PIC  X(001).
                15 DCOMWOPC-S-HULT-ATULZ            PIC  X(026).
                15 DCOMWOPC-S-CFUNC-BDSCO           PIC  9(009).
                15 DCOMWOPC-S-CTERM                 PIC  X(008).
                15 DCOMWOPC-S-CCONTR-CORP-OPER      PIC  9(007).
                15 DCOMWOPC-S-DINCL-REG             PIC  X(010).
                15 DCOMWOPC-S-CCART-LIM-CREDT       PIC  X(005).
                15 DCOMWOPC-S-ESPECIE-TITLO         PIC  9(003).
                15 DCOMWOPC-S-IPSSOA-DESC-COML      PIC  X(060).
                15 DCOMWOPC-S-CCNPJ-CPF             PIC  9(009).
                15 DCOMWOPC-S-CFLIAL-CNPJ           PIC  9(004).
                15 DCOMWOPC-S-CCTRL-CNPJ-CPF        PIC  9(002).
                15 DCOMWOPC-S-CDOCTO-ID             PIC  X(015).
                15 DCOMWOPC-S-NOME-BANCO            PIC  X(040).
                15 DCOMWOPC-S-NOME-AGENCIA          PIC  X(040).
                15 DCOMWOPC-S-CCHAVE-ELMTO-DESC     PIC  X(045).
                15 DCOMWOPC-S-CINDCD-ALIQT-REDZD    PIC  X(001).
                15 DCOMWOPC-S-PTX-JURO-DIA          PIC  9(003)V9(005).
                15 DCOMWOPC-S-CINDCD-FREQ-TX        PIC  X(002).
                15 DCOMWOPC-S-CET-AM                PIC  9(005)V9(002).
                15 DCOMWOPC-S-CET-AA                PIC  9(005)V9(002).
                15 DCOMWOPC-S-QTDE-BANDEIRAS        PIC  9(009).
                15 DCOMWOPC-S-CPSSOA-JURID-CONTR    PIC  9(010).
                15 DCOMWOPC-S-CTPO-CONTR-NEGOC      PIC  9(003).
                15 DCOMWOPC-S-NSEQ-CONTR-NEGOC      PIC  9(010).
                15 DCOMWOPC-S-CPRODT-SERVC-OPER     PIC  9(008).
                15 DCOMWOPC-S-CPPSTA-PRODT          PIC  9(019).
                15 DCOMWOPC-S-NSEQ-CONTR-LIM        PIC  9(017).
                15 DCOMWOPC-S-CVAR-PRODT-RURAL      PIC  9(005).
                15 DCOMWOPC-S-CNRO-REFT-BACEN       PIC  9(018).
                15 DCOMWOPC-S-CCESTA-SAFRA-RURAL    PIC  9(005).
                15 DCOMWOPC-S-ICESTA-SAFRA-RURAL    PIC  X(080).
                15 DCOMWOPC-S-CINDCD-ZNMTO          PIC  9(001).
                15 DCOMWOPC-S-DINDCD-ZNMTO          PIC  X(015).
                15 DCOMWOPC-S-CTPO-BNEFC-RURAL      PIC  9(002).
                15 DCOMWOPC-S-ITPO-BNEFC-RURAL      PIC  X(060).
                15 DCOMWOPC-S-NBASE-LEGAL-RENEG     PIC  9(005).
                15 DCOMWOPC-S-CREG-AMBTL-RURAL      PIC  X(041).
                15 DCOMWOPC-S-CORIGE-ANTEC-PGMD     PIC  9(001).
                15 DCOMWOPC-S-CCONTR-LIM-OPTIN      PIC  9(009).
                15 DCOMWOPC-S-CVRSAO-CONTR-OPTIN    PIC  9(003).
                15 DCOMWOPC-S-CINDCD-FORMA-LIQDC    PIC  X(001).
                15 DCOMWOPC-S-CINDCD-UTILZ-LIM      PIC  X(001).
                15 DCOMWOPC-S-CINDCD-DEB-PCIAL      PIC  X(001).
                15 DCOMWOPC-S-CINDCD-TENTV-DEB      PIC  X(001).
                15 DCOMWOPC-S-CPSSOA-JURID-CONTR    PIC  9(010).
                15 DCOMWOPC-S-CTPO-CONTR-NEGOC      PIC  9(003).
                15 DCOMWOPC-S-NSEQ-CONTR-NEGOC      PIC  9(010).
                15 DCOMWOPC-S-CPRODT-SERVC-OPER     PIC  9(008).
                15 DCOMWOPC-S-CPPSTA-PRODT          PIC  9(019).
                15 DCOMWOPC-S-NSEQ-CONTR-LIM        PIC  9(017).
                15 DCOMWOPC-S-RESERVA               PIC  X(197).
      *
