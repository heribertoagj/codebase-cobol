      ******************************************************************
      *    I#BVVEPC - LAY-OUT DO ARQUIVO DE PEDIDO DE CARTAO           *
      *    LRECL: 1500                                                 *
      ******************************************************************
       01  CADA-CONTRATO.
           05  CONTRATO-CH.
               10  CONTR-D1-D3-D4-D5-CH.
                   15  CONTR-CH-CNPJ              PIC 9(008).
                   15  CONTR-CH-FILIAL            PIC 9(004).
                   15  CONTR-CH-CTRL              PIC 9(002).
               10  CONTR-D4-D5-CH.
                   15  CONTR-CH-PRODUTO           PIC 9(003).
           05  CONTR-D3-D4-D5.
               10  CONTR-NRO-CONTRATO             PIC 9(011).
           05  CONTR-STATUS-PEDIDO.
               10  CONTR-STATUS                   PIC X(001).
      *            P - PENDENTE ENVIO
      *            E - EM ANALISE
      *            I - INCONSISTENTE
      *            S - RENOVACAO SUSPENSA
           05  CONTR-D1.
               10  CONTR-RAZAO-SOCIAL             PIC X(040).
               10  CONTR-NOME-FANTASIA            PIC X(020).
               10  CONTR-TIPO-EMPRESA             PIC 9(003).
               10  CONTR-REST-EMPRESA             PIC 9(002).
               10  CONTR-QTD-FUNC-EMPRESA         PIC 9(006).
               10  CONTR-NOME-FILIAL-CONTRATANTE  PIC X(035).
               10  CONTR-LOGRADOURO               PIC X(050).
               10  CONTR-NRO-LOGRADOURO           PIC X(005).
               10  CONTR-COMPL-LOGRADOURO         PIC X(025).
               10  CONTR-MUNICIPIO                PIC X(028).
               10  CONTR-ESTADO                   PIC X(002).
               10  CONTR-CEP                      PIC 9(008).
               10  CONTR-DDD                      PIC X(005).
               10  CONTR-TELEFONE                 PIC X(012).
               10  CONTR-LIMITE-CREDITO           PIC 9(013).
               10  CONTR-DT-INICIO-VL-CRED        PIC 9(008).
               10  CONTR-DT-FIM-VL-CRED           PIC 9(008).
               10  CONTR-MODIFICAR-LIM-CRED       PIC X(001).
               10  CONTR-INTER-D-FUNC             PIC X(001).
               10  CONTR-NOME-INTER-D             PIC X(035).
               10  CONTR-CPF-INTER-D              PIC 9(011).
               10  CONTR-DT-NASC-INTER-D          PIC 9(008).
               10  CONTR-DDD-INTER-D              PIC X(005).
               10  CONTR-TELEFONE-INTER-D         PIC X(012).
               10  CONTR-INTER-O-FUNC             PIC X(001).
               10  CONTR-CPF-INTER-O              PIC 9(011).
               10  CONTR-DT-NASC-INTER-O          PIC 9(008).
               10  CONTR-NOME-INTER-O             PIC X(035).
               10  CONTR-DDD-INTER-O              PIC X(005).
               10  CONTR-TELEFONE-INTER-O         PIC X(012).
           05  CONTR-D4.
               10  CONTR-BANCO-VENDEU             PIC 9(004).
               10  CONTR-AGENCIA-VENDEU           PIC 9(005).
               10  CONTR-CPF-VENDEDOR             PIC 9(011).
               10  CONTR-NOME-VENDEDOR            PIC X(040).
               10  CONTR-DDD-VENDEDOR             PIC 9(005).
               10  CONTR-TELEFONE-VENDEDOR        PIC 9(012).
               10  CONTR-DT-ASSINATURA-CONTRATO   PIC 9(008).
               10  CONTR-VLR-MEDIO-BENEFI         PIC 9(008)V99.
               10  CONTR-FORMA-PAGTO              PIC 9(001).
               10  CONTR-MEIO-PAGTO               PIC 9(001).
               10  CONTR-AGENCIA-DEBTO-CTA        PIC 9(005).
               10  CONTR-CONTA-DEBTO-CTA          PIC 9(013).
               10  CONTR-DIG-CONTA-DEBTO-CTA      PIC X(001).
               10  CONTR-NOME-EMBOSSING           PIC X(019).
               10  CONTR-ENTREGA-CENTRAL          PIC X(001).
               10  CONTR-CNPJ-ENTREGA             PIC 9(014).
               10  CONTR-FATURA-CENTRAL           PIC X(001).
               10  CONTR-CNPJ-FATURA              PIC 9(014).
               10  CONTR-CNPJ-COBRANCA            PIC 9(014).
               10  CONTR-FUNC-PEDIR-REEMISSAO     PIC X(001).
               10  CONTR-PRAZO-PAGAMENTO          PIC X(003).
               10  CONTR-COD-AUT-TARIFA           PIC X(010).
           05  CONTR-D5.
               10  CONTR-D5-TABELA-TARIFAS        OCCURS 50.
                   15  CONTR-TARIFA               PIC 9(003).
                   15  CONTR-VALOR-TARIFA         PIC 9(009)V99.
           05  CONTR-FLAG-ERRO.
               10  CONTR-NRO-LOGRADOURO-F         PIC X(001).
               10  CONTR-MUNICIPIO-F              PIC X(001).
               10  CONTR-ESTADO-F                 PIC X(001).
               10  CONTR-CEP-F                    PIC X(001).
               10  CONTR-DDD-F                    PIC X(001).
               10  CONTR-TELEFONE-F               PIC X(001).
               10  CONTR-NRO-CONTRATO-F           PIC X(001).
               10  CONTR-INTER-D-FUNC-F           PIC X(001).
               10  CONTR-NOME-INTER-D-F           PIC X(001).
               10  CONTR-CPF-INTER-D-F            PIC X(001).
               10  CONTR-DT-NASC-INTER-D-F        PIC X(001).
               10  CONTR-DDD-INTER-D-F            PIC X(001).
               10  CONTR-TELEFONE-INTER-D-F       PIC X(001).
               10  CONTR-INTER-O-FUNC-F           PIC X(001).
               10  CONTR-NOME-INTER-O-F           PIC X(001).
               10  CONTR-CPF-INTER-O-F            PIC X(001).
               10  CONTR-DT-NASC-INTER-O-F        PIC X(001).
               10  CONTR-DDD-INTER-O-F            PIC X(001).
               10  CONTR-TELEFONE-INTER-O-F       PIC X(001).
               10  CONTR-BANCO-VENDEU-F           PIC X(001).
               10  CONTR-QTD-FUNC-BENEFI-F        PIC X(001).
               10  CONTR-VLR-MEDIO-BENEFI-F       PIC X(001).
               10  CONTR-FORMA-PAGTO-F            PIC X(001).
               10  CONTR-MEIO-PAGTO-F             PIC X(001).
               10  CONTR-NOME-EMBOSSING-F         PIC X(001).
               10  CONTR-ENTREGA-CENTRAL-F        PIC X(001).
               10  CONTR-CNPJ-ENTREGA-F           PIC X(001).
               10  CONTR-FATURA-CENTRAL-F         PIC X(001).
               10  CONTR-CNPJ-FATURA-F            PIC X(001).
               10  CONTR-CNPJ-COBRANCA-F          PIC X(001).
               10  CONTR-FUNC-PEDIR-REEMISSAO-F   PIC X(001).
           05  CONTR-MODIFICAR-INTER              PIC X(001).
           05  CONTR-COD-AUT-CONTA                PIC X(010).
           05  CONTR-D3-CONTINUA.
               10  CONTR-EMAIL-INTER-D            PIC X(040).
               10  CONTR-EMAIL-INTER-O            PIC X(040).
           05  FILLER                             PIC X(033).
