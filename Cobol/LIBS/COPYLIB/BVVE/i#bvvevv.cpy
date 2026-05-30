      ******************************************************************
      *    I#BVVEVV - LAY-OUT DO ARQUIVO DE ENVIO DE PEDIDO DE CARTAO  *
      *    LRECL: 350                                                  *
      *                                                                *
      *    H0 - HEADER DO ARQUIVO                                      *
      *    D1 - CADASTRO DO CONTRATANTE PESSOA JURIDICA                *
      *         POSSUI PARTES 1 E 2                                    *
      *    D2 - BENEFICIOS MANTIDOS PELA EMPRESA CONTRATANTE           *
      *    D3 - CADASTRO DOS INTERLOCUTORES DO CLIENTE CONTRATANTE     *
      *    D4 - DADOS GERAIS DO CONTRATO (PRODUTO CONTRATADO)          *
      *    D5 - OPCOES E PRECOS CONTRATADOS (TARIFAS)                  *
      *    T1 - TRAILLER DO ARQUIVO                                    *
      *                                                                *
      ******************************************************************
       01  ENVIO-PEDIDO-CARTAO-H0.
           03  ENV-TIPO-H0                 PIC X(002).
      *        FIXO "H0"
           03  ENV-SUBTIPO-H0              PIC 9(001).
      *        FIXO "1"
           03  ENV-DT-REF-INTERFACE-H0     PIC 9(008).
           03  ENV-NUM-REMESSA-DIA-H0      PIC 9(003).
           03  ENV-LIT-CONTRATO-H0         PIC X(030).
           03  ENV-CANAL-ENT-H0            PIC X(004).
      *        FIXO "I002"
      *        INTERFACE "002"
           03  ENV-COD-BCO-REM-H0          PIC 9(004).
           03  ENV-COD-AG-REM-H0           PIC 9(005).
           03  ENV-NOME-BCO-H0             PIC X(040).
           03  ENV-CNPJ-H0                 PIC 9(008).
           03  ENV-FILIAL-H0               PIC 9(004).
           03  ENV-CONTROLE-H0             PIC 9(002).
           03  ENV-DDD-H0                  PIC X(005).
           03  ENV-TEL-H0                  PIC X(012).
           03  ENV-FAX-H0                  PIC X(012).
           03  ENV-NOME-CONTAT0-H0         PIC X(040).
           03  ENV-TEL-CONTATO-H0          PIC X(012).
           03  ENV-RAMAL-CONTATO-H0        PIC X(006).
           03  ENV-EMAIL-CONTATO-H0        PIC X(040).
           03  ENV-NUM-REM-CBSS-H0         PIC 9(010).
           03  ENV-TIPO-LAYOUT-H0          PIC X(001).
      *        "B" -> BANCO
      *        "E" -> EMPRESA
           03  ENV-VERSAO-LAYOUT-H0        PIC 9(003).
      *        FIXO 18
           03  FILLER                      PIC X(042).
           03  ENV-NUM-SEQ-H0              PIC 9(006).
           03  ENV-COD-RETORNO-H0          PIC X(050).

      *----------------------------------------------------------------*

       01  ENVIO-PEDIDO-CARTAO-D1-1        REDEFINES
           ENVIO-PEDIDO-CARTAO-H0.
           03  ENV-TIPO-D1                 PIC X(002).
      *        FIXO "D1"
           03  ENV-SUBTIPO-D1              PIC 9(001).
      *        FIXO "1"
           03  ENV-TIPO-PESSOA-D1          PIC X(001).
      *        FIXO "J"
           03  ENV-COD-OPERACAO-D1         PIC X(001).
      *        "N" -> CLIENTE E/OU CONTRATO NOVO
      *        "A" -> ALTERACAO CLIENTE E/OU CONTRATO
           03  ENV-CNPJ-D1                 PIC 9(008).
           03  ENV-FILIAL-D1               PIC 9(004).
           03  ENV-CONTROLE-D1             PIC 9(002).
           03  ENV-RAZAO-SOCIAL-D1         PIC X(040).
           03  ENV-NOME-FANTASIA-D1        PIC X(020).
           03  ENV-TIPO-EMPRESA-D1         PIC 9(003).
           03  FILLER                      PIC X(001).
           03  ENV-RESTRICAO-FINANC-D1     PIC X(001).
      *        "S" -> POSSUI RESTRICAO FINANCEIRA
      *        "N" -> NAO POSSUI RESTRICAO FINANCEIRA
           03  ENV-COD-RESTRICAO-D1        PIC 9(002).
           03  ENV-CLASS-SECAO-CNAE-D1     PIC X(001).
           03  ENV-CLASS-DIVISAO-CNAE-D1   PIC 9(002).
           03  ENV-QTD-FUNC-D1             PIC 9(006).
           03  ENV-FATURAMENTO-ANUAL-D1    PIC 9(013).
           03  ENV-INSCRICAO-PAT           PIC X(020).
           03  FILLER                      PIC X(040).
           03  ENV-NOME-GRP-ECONOMICO-D1   PIC X(040).
           03  FILLER                      PIC X(003).
           03  ENV-NOME-FIL-CONTR-D1       PIC X(035).
           03  ENV-INSCR-EST-FIL-CONTR-D1  PIC X(020).
           03  ENV-INSCR-MUN-FIL-CONTR-D1  PIC X(020).
           03  FILLER                      PIC X(008).
           03  ENV-NUM-SEQ-D1              PIC 9(006).
           03  ENV-COD-ERROS-D1            PIC X(050).

      *----------------------------------------------------------------*

       01  ENVIO-PEDIDO-CARTAO-D1-2        REDEFINES
           ENVIO-PEDIDO-CARTAO-H0.
           03  ENV-TIPO-D1-2               PIC X(002).
      *        FIXO "D1"
           03  ENV-SUBTIPO-D1-2            PIC 9(001).
      *        FIXO "2"
           03  ENV-LOGRADOURO-D1-2         PIC X(050).
           03  ENV-NUMERO-LOGDR-D1-2       PIC X(005).
           03  ENV-COMPL-END-D1-2          PIC X(025).
           03  ENV-BAIRRO-D1-2             PIC X(030).
           03  ENV-MUNICIPIO-D1-2          PIC X(028).
           03  ENV-UF-D1-2                 PIC X(002).
           03  ENV-CEP-D1-2                PIC 9(008).
           03  ENV-DDD-D1-2                PIC X(005).
           03  ENV-TEL1-D1-2               PIC X(012).
           03  ENV-TEL2-D1-2               PIC X(012).
           03  ENV-FAX-D1-2                PIC X(012).
           03  FILLER                      PIC X(040).
           03  ENV-LIMITE-CREDITO-D1-2     PIC 9(013).
           03  ENV-DT-INI-LIM-CRED-D1-2    PIC 9(008).
           03  ENV-DT-VENC-LIM-CRED-D1-2   PIC 9(008).
           03  ENV-COD-EMPR-LIM-CRED-D1-2  PIC 9(004).
      *        CODIGO DO BANCO OU 9 + CODIGO DA EMPRESA QUE FORNECEU O
      *        LIMITE DE CREDITO.
           03  ENV-LIM-POR-CONTRATO-D1-2   PIC X(001).
      *        "S" -> SIM
      *        "N" -> NAO
           03  ENV-MOD-LIM-CRED-D1-2       PIC X(001).
      *        "S" -> SIM
      *        "N" -> NAO
           03  FILLER                      PIC X(027).
           03  ENV-NUM-SEQ-D1-2            PIC 9(006).
           03  ENV-COD-ERROS-D1-2          PIC X(050).

      *----------------------------------------------------------------*

       01  ENVIO-PEDIDO-CARTAO-D2          REDEFINES
           ENVIO-PEDIDO-CARTAO-H0.
           03  ENV-TIPO-D2                 PIC X(002).
      *        FIXO "D2"
           03  ENV-SUBTIPO-D2              PIC 9(001).
      *        FIXO "1"
           03  FILLER                      PIC X(291).
           03  ENV-NUM-SEQ-D2              PIC 9(006).
           03  ENV-COD-ERROS-D2            PIC X(050).

      *----------------------------------------------------------------*

       01  ENVIO-PEDIDO-CARTAO-D3-1        REDEFINES
           ENVIO-PEDIDO-CARTAO-H0.
           03  ENV-TIPO-D3                 PIC X(002).
      *        FIXO "D3"
           03  ENV-SUBTIPO-D3              PIC 9(001).
      *        FIXO "1"
           03  ENV-NUM-CONTRATO-D3         PIC 9(011).
           03  ENV-CNPJ-D3                 PIC 9(008).
           03  ENV-FILIAL-D3               PIC 9(004).
           03  ENV-CONTROLE-D3             PIC 9(002).
           03  ENV-CPF-D3                  PIC 9(011).
           03  ENV-INTERD-FUNC-EMPRESA-D3  PIC X(001).
      *        "S" -> SIM
      *        "N" -> NAO
           03  ENV-NOME-INTERD-D3          PIC X(035).
           03  ENV-CPF-INTERD-D3           PIC 9(011).
           03  ENV-DT-NASC-INTERD-D3       PIC 9(008).
           03  ENV-CARGO-INTERD-D3         PIC 9(001).
      *        1 -> PROPRIETARIO
      *        2 -> DIRETORIA
      *        3 -> GERENCIA
      *        4 -> SUPERVISAO
      *        5 -> OUTROS
           03  ENV-INTERO-FUNC-EMPRESA-D3  PIC X(001).
      *        "S" -> SIM
      *        "N" -> NAO
           03  ENV-CPF-INTERO-D3           PIC 9(011).
           03  ENV-DT-NASC-INTERO-D3       PIC 9(008).
           03  ENV-CARGO-INTERO-D3         PIC 9(001).
      *        1 -> PROPRIETARIO
      *        2 -> DIRETORIA
      *        3 -> GERENCIA
      *        4 -> SUPERVISAO
      *        5 -> OUTROS
           03  ENV-TEL-CEL-INTERO-D3       PIC X(012).
           03  FILLER                      PIC X(115).
           03  ENV-DDD-INTERD-D3           PIC X(005).
           03  ENV-TEL-INTERD-D3           PIC X(012).
           03  ENV-RAMAL-INTERD-D3         PIC X(006).
           03  ENV-TEL-CEL-INTERD-D3       PIC X(012).
           03  ENV-FAX-INTERD-D3           PIC X(012).
           03  FILLER                      PIC X(004).
           03  ENV-NUM-SEQ-D3              PIC 9(006).
           03  ENV-COD-ERROS-D3            PIC X(050).

      *----------------------------------------------------------------*

       01  ENVIO-PEDIDO-CARTAO-D3-2        REDEFINES
           ENVIO-PEDIDO-CARTAO-H0.
           03  ENV-TIPO-D3-2               PIC X(002).
      *        FIXO "D3"
           03  ENV-SUBTIPO-D3-2            PIC 9(001).
      *        FIXO "2"
           03  ENV-RAMAL-FAX-INTERD-D3-2   PIC X(006).
           03  ENV-EMAIL-INTERD-D3-2       PIC X(040).
           03  ENV-LOCINT-INTERD-D3-2      PIC X(040).
           03  ENV-NOME-INTERO-D3-2        PIC X(035).
           03  ENV-LOCINT-INTERO-D3-2      PIC X(040).
           03  ENV-DDD-INTERO-D3-2         PIC X(005).
           03  ENV-TEL1-INTERO-D3-2        PIC X(012).
           03  ENV-RAMAL1-INTERO-D3-2      PIC X(006).
           03  ENV-TEL2-INTERO-D3-2        PIC X(012).
           03  ENV-RAMAL2-INTERO-D3-2      PIC X(006).
           03  ENV-FAX-INTERO-D3-2         PIC X(012).
           03  ENV-RAMAL-FAX-INTERO-D3-2   PIC X(006).
           03  ENV-EMAIL-INTERO-D3-2       PIC X(040).
           03  FILLER                      PIC X(031).
           03  ENV-NUM-SEQ-D3-2            PIC 9(006).
           03  ENV-COD-ERROS-D3-2          PIC X(050).

      *----------------------------------------------------------------*

       01  ENVIO-PEDIDO-CARTAO-D4-1        REDEFINES
           ENVIO-PEDIDO-CARTAO-H0.
           03  ENV-TIPO-D4                 PIC X(002).
      *        FIXO "D4"
           03  ENV-SUBTIPO-D4              PIC 9(001).
      *        FIXO "1"
           03  ENV-NUM-CONTRATO-D4         PIC 9(011).
           03  ENV-NUM-FICHA-PROP-D4       PIC 9(010).
           03  ENV-CNPJ-D4                 PIC 9(008).
           03  ENV-FILIAL-D4               PIC 9(004).
           03  ENV-CONTROLE-D4             PIC 9(002).
           03  ENV-CPF-D4                  PIC 9(011).
           03  ENV-COD-PRODUTO-D4          PIC 9(003).
           03  ENV-COD-BANCO-D4            PIC 9(004).
           03  ENV-AGENCIA-D4              PIC 9(005).
           03  ENV-DV-AGENCIA-D4           PIC X(001).
           03  ENV-COD-ID-VENDEDOR-D4      PIC X(020).
           03  ENV-TP-COD-ID-D4            PIC X(001).
      *        "S" -> SIM
      *        "N" -> NAO
           03  ENV-NOME-VENDEDOR-D4        PIC X(040).
           03  ENV-DDD-VENDEDOR-D4         PIC X(005).
           03  ENV-TEL1-VENDEDOR-D4        PIC X(012).
           03  ENV-RAMAL-VENDEDOR-D4       PIC X(006).
           03  ENV-DT-ASS-CONTRATO-D4      PIC 9(008).
           03  ENV-QTD-FILIAIS-CONT-D4     PIC 9(005).
           03  ENV-QTD-PREV-USU-D4         PIC 9(006).
           03  ENV-VL-MEDIO-BENEF-D4       PIC 9(010).
           03  ENV-FORMA-PGTO-D4           PIC 9(001).
      *        1 -> ANTECIPADO
      *        2 -> A PRAZO
           03  ENV-MEIO-PGTO-D4            PIC 9(001).
      *        1 -> CREDITO EM CONTA
      *        2 -> DEBITO EM CONTA
      *        3 -> COBRANCA BANCARIA
      *        4 -> CARTAO DE CREDITO
      *        5 -> BOLETO WEB
           03  ENV-BCO-DEB-CONTA-D4        PIC 9(004).
           03  ENV-AG-DEB-CONTA-D4         PIC 9(005).
           03  ENV-DV-AG-DEB-CONTA-D4      PIC X(001).
           03  FILLER                      PIC X(008).
           03  ENV-BCO-COB-BANCARIA-D4     PIC 9(004).
           03  ENV-FATURAMENTO-ELETR-D4    PIC X(001).
      *        "S" -> SIM
      *        "N" -> NAO
           03  ENV-EMBOSS-EMPR-D4          PIC X(019).
           03  ENV-ENTREGA-CENTRAL-D4      PIC X(001).
      *        "S" -> SIM
      *        "N" -> NAO
           03  ENV-CNPJ-FILIAL-D4          PIC 9(014).
           03  ENV-FATUR-CENTRAL-D4        PIC X(001).
      *        "S" -> SIM
      *        "N" -> NAO
           03  ENV-CNPJ-FILIAL-FATUR-D4    PIC 9(014).
           03  ENV-CNPJ-FILIAL-COB-D4      PIC 9(014).
           03  ENV-COD-AREA-COMERC-D4      PIC 9(004).
           03  ENV-COD-PARCEIRO-EMIS-D4    PIC 9(003).
      *        1  -> CBSS
      *        10 -> CARTOES
           03  ENV-TP-MERCADO-D4           PIC 9(002).
      *        01 -> FUNCIONARIOS DOS SOCIOS
      *        02 -> MERCADO
           03  ENV-NEGOCIACAO-D4           PIC 9(001).
      *        1  -> NORMAL
      *        2  -> LICITACAO
           03  ENV-NUM-CONTA-DEBITO-D4     PIC 9(013).
           03  ENV-DV-CONTA-DEBITO-D4      PIC X(001).
           03  FILLER                      PIC X(007).
           03  ENV-NUM-SEQ-D4              PIC 9(006).
           03  ENV-COD-ERROS-D4            PIC X(050).

      *----------------------------------------------------------------*

       01  ENVIO-PEDIDO-CARTAO-D4-2        REDEFINES
           ENVIO-PEDIDO-CARTAO-H0.
           03  ENV-TIPO-D4-2               PIC X(002).
      *        FIXO "D4"
           03  ENV-SUBTIPO-D4-2            PIC 9(001).
      *        FIXO "2"
           03  ENV-ENTREGA-IND-RES-D4-2    PIC X(001).
      *        "S" -> SIM
      *        "N" -> NAO
           03  ENV-PERMIS-USU-REEM-D4-2    PIC X(001).
      *        "S" -> SIM
      *        "N" -> NAO
           03  ENV-TP-CARTAO-D4-2          PIC 9(001).
      *        1 -> MONO APLICATIVO
      *        2 -> MULTI APLICATIVO HOSPEDEIRO
      *        3 -> MULTI APLICATIVO HOSPEDE
      *        FIXO "1"
           03  ENV-COD-TECNO-D4-2          PIC 9(002).
      *        1 -> TARJA MAGNETICA
      *        2 -> HIBRIDO
      *        FIXO "1"
           03  ENV-VL-CRED-CONCED-D4-2     PIC 9(013).
           03  ENV-DT-INI-LIM-CRED-D4-2    PIC 9(008).
           03  ENV-DT-VENC-LIM-CRED-D4-2   PIC 9(008).
           03  ENV-DT-VAL-PRAZO-PGTO-D4-2  PIC 9(008).
           03  ENV-DT-VENC-PRAZO-PGTO-D4-2 PIC 9(008).
           03  ENV-PRAZO-PGTO-DIAS-D4-2    PIC 9(003).
           03  ENV-RAZAO-SOCIAL-EMP-D4-2   PIC X(040).
           03  ENV-NOME-FILIAL-D4-2        PIC X(035).
           03  ENV-COD-AUTORIZ-D4-2        PIC X(010).
           03  ENV-NUM-CCREDITO-D4-2       PIC 9(016).
           03  ENV-DT-VAL-CCREDITO-D4-2    PIC 9(004).
      *        FORMATO -> MMAA
           03  ENV-CVV2-CCREDITO-D4-2      PIC 9(003).
           03  FILLER                      PIC X(130).
           03  ENV-NUM-SEQ-D4-2            PIC 9(006).
           03  ENV-COD-ERROS-D4-2          PIC X(050).

      *----------------------------------------------------------------*

       01  ENVIO-PEDIDO-CARTAO-D5          REDEFINES
           ENVIO-PEDIDO-CARTAO-H0.
           03  ENV-TIPO-D5                 PIC X(002).
      *        FIXO "D5"
           03  ENV-SUBTIPO-D5              PIC 9(001).
      *        FIXO "1"
           03  ENV-NUM-CONTRATO-D5         PIC 9(011).
           03  ENV-COD-PRODUTO-D5          PIC 9(003).
           03  ENV-CNPJ-D5                 PIC 9(008).
           03  ENV-FILIAL-D5               PIC 9(004).
           03  ENV-CONTROLE-D5             PIC 9(002).
           03  ENV-CPF-D5                  PIC 9(011).
           03  ENV-COD-TARIFA-D5           PIC 9(003).
           03  FILLER                      PIC X(001).
           03  ENV-DT-INI-VALID-OP-D5      PIC 9(008).
           03  ENV-DT-FIM-VALID-OP-D5      PIC 9(008).
           03  ENV-VL-UNITARIO-D5          PIC 9(011).
           03  ENV-PERCENTUAL-D5           PIC 9(005).
           03  ENV-PERCENTUAL-BONI-D5      PIC 9(005).
           03  ENV-QTD-MESES-DESC-D5       PIC 9(002).
           03  ENV-QTD-MESES-INSEN-D5      PIC 9(002).
           03  ENV-QTD-MESES-BONI-D5       PIC 9(002).
           03  FILLER                      PIC X(205).
           03  ENV-NUM-SEQ-D5              PIC 9(006).
           03  ENV-COD-ERROS-D5            PIC X(050).

      *----------------------------------------------------------------*
       01  ENVIO-PEDIDO-CARTAO-T1          REDEFINES
           ENVIO-PEDIDO-CARTAO-H0.
           03  ENV-TIPO-T1                 PIC X(002).
      *        FIXO "T1"
           03  ENV-SUBTIPO-T1              PIC 9(001).
      *        FIXO "1"
           03  ENV-QTDE-T1                 PIC 9(006).
           03  ENV-QTDE-H0-ERRO-T1         PIC 9(006).
           03  ENV-QTDE-D1-ERRO-T1         PIC 9(006).
           03  ENV-QTDE-D2-ERRO-T1         PIC 9(006).
           03  ENV-QTDE-D3-ERRO-T1         PIC 9(006).
           03  ENV-QTDE-D4-ERRO-T1         PIC 9(006).
           03  ENV-QTDE-D5-ERRO-T1         PIC 9(006).
           03  ENV-QTDE-T1-ERRO-T1         PIC 9(006).
           03  FILLER                      PIC X(243).
           03  ENV-NUM-SEQ-T1              PIC 9(006).
           03  ENV-COD-ERROS-T1            PIC X(050).
