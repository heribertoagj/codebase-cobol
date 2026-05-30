      ******************************************************************
      *    I#BVVEPC - LAY-OUT DO ARQUIVO DE PEDIDO DE CARTAO           *
      *    LRECL: 1500                                                 *
      ******************************************************************
       01  CADA-PEDIDO-CARTAO.
           05  CADA-PDCT-CH.
               10  CADA-D1-D3-D4-D5-CH.
                   15  CADA-PDCT-CH-CNPJ-CONTR    PIC 9(008).
                   15  CADA-PDCT-CH-FILIAL-CONTR  PIC 9(004).
                   15  CADA-PDCT-CH-CTRL-CONTR    PIC 9(002).
               10  CADA-D4-D5-CH.
                   15  CADA-PDCT-CH-PRODUTO       PIC 9(003).
           05  CADA-D3-D4-D5.
               10  CADA-NRO-CONTRATO              PIC 9(011).
           05  CADA-STATUS-PEDIDO.
               10  CADA-STATUS                    PIC X(001).
      *            P - PENDENTE ENVIO
      *            E - EM ANALISE
      *            I - INCONSISTENTE
      *            S - RENOVACAO SUSPENSA
4SYSJT*            L - LIMITE PENDENTE
4SYSJT*            R - LIMITE REPROVADO
           05  CADA-D1.
               10  CADA-RAZAO-SOCIAL              PIC X(040).
               10  CADA-NOME-FANTASIA             PIC X(020).
               10  CADA-TIPO-EMPRESA              PIC 9(003).
               10  CADA-REST-EMPRESA              PIC 9(002).
               10  CADA-QTD-FUNC-EMPRESA          PIC 9(006).
               10  CADA-NOME-FILIAL-CONTRATANTE   PIC X(035).
               10  CADA-LOGRADOURO                PIC X(050).
               10  CADA-NRO-LOGRADOURO            PIC X(005).
               10  CADA-COMPL-LOGRADOURO          PIC X(025).
               10  CADA-MUNICIPIO                 PIC X(028).
               10  CADA-ESTADO                    PIC X(002).
               10  CADA-CEP                       PIC 9(008).
               10  CADA-DDD                       PIC X(005).
               10  CADA-TELEFONE                  PIC X(012).
               10  CADA-LIMITE-CREDITO            PIC 9(013).
               10  CADA-DT-INICIO-VL-CRED         PIC 9(008).
               10  CADA-MODIFICAR-LIM-CRED        PIC X(001).
           05  CADA-D3.
               10  CADA-INTER-D-FUNC              PIC X(001).
               10  CADA-NOME-INTER-D              PIC X(035).
               10  CADA-CPF-INTER-D               PIC 9(011).
               10  CADA-DT-NASC-INTER-D           PIC 9(008).
               10  CADA-DDD-INTER-D               PIC X(005).
               10  CADA-TELEFONE-INTER-D          PIC X(012).
               10  CADA-INTER-O-FUNC              PIC X(001).
               10  CADA-CPF-INTER-O               PIC 9(011).
               10  CADA-DT-NASC-INTER-O           PIC 9(008).
               10  CADA-NOME-INTER-O              PIC X(035).
               10  CADA-DDD-INTER-O               PIC X(005).
               10  CADA-TELEFONE-INTER-O          PIC X(012).
           05  CADA-D4.
               10  CADA-BANCO-VENDEU              PIC 9(004).
               10  CADA-AGENCIA-VENDEU            PIC 9(005).
               10  CADA-CPF-VENDEDOR              PIC 9(011).
               10  CADA-NOME-VENDEDOR             PIC X(040).
               10  CADA-DDD-VENDEDOR              PIC 9(005).
               10  CADA-DT-ASSINATURA-CONTRATO    PIC 9(008).
               10  CADA-QTD-FUNC-BENEFI           PIC 9(006).
               10  CADA-VLR-MEDIO-BENEFI          PIC 9(008)V99.
               10  CADA-FORMA-PAGTO               PIC 9(001).
               10  CADA-MEIO-PAGTO                PIC 9(001).
               10  CADA-AGENCIA-DEBTO-CTA         PIC 9(005).
               10  CADA-CONTA-DEBTO-CTA           PIC 9(013).
               10  CADA-DIG-CONTA-DEBTO-CTA       PIC X(001).
               10  CADA-NOME-EMBOSSING            PIC X(019).
               10  CADA-ENTREGA-CENTRAL           PIC X(001).
               10  CADA-CNPJ-ENTREGA              PIC 9(014).
               10  CADA-FATURA-CENTRAL            PIC X(001).
               10  CADA-CNPJ-FATURA               PIC 9(014).
               10  CADA-CNPJ-COBRANCA             PIC 9(014).
               10  CADA-FUNC-PEDIR-REEMISSAO      PIC X(001).
               10  CADA-PRAZO-PAGAMENTO           PIC X(003).
               10  CADA-COD-AUT-TARIFA            PIC X(010).
           05  CADA-D5.
               10  CADA-D5-TABELA-TARIFAS         OCCURS 50.
                   15  CADA-TARIFA                PIC 9(003).
           05  CADA-FLAG-ERRO.
               10  CADA-LOGRADOURO-F              PIC X(001).
               10  CADA-NRO-LOGRADOURO-F          PIC X(001).
               10  CADA-MUNICIPIO-F               PIC X(001).
               10  CADA-ESTADO-F                  PIC X(001).
               10  CADA-CEP-F                     PIC X(001).
               10  CADA-DDD-F                     PIC X(001).
               10  CADA-TELEFONE-F                PIC X(001).
               10  CADA-NRO-CONTRATO-F            PIC X(001).
               10  CADA-INTER-D-FUNC-F            PIC X(001).
               10  CADA-NOME-INTER-D-F            PIC X(001).
               10  CADA-CPF-INTER-D-F             PIC X(001).
               10  CADA-DT-NASC-INTER-D-F         PIC X(001).
               10  CADA-DDD-INTER-D-F             PIC X(001).
               10  CADA-TELEFONE-INTER-D-F        PIC X(001).
               10  CADA-INTER-O-FUNC-F            PIC X(001).
               10  CADA-NOME-INTER-O-F            PIC X(001).
               10  CADA-CPF-INTER-O-F             PIC X(001).
               10  CADA-DT-NASC-INTER-O-F         PIC X(001).
               10  CADA-DDD-INTER-O-F             PIC X(001).
               10  CADA-BANCO-VENDEU-F            PIC X(001).
               10  CADA-AGENCIA-VENDEU-F          PIC X(001).
               10  CADA-QTD-FUNC-BENEFI-F         PIC X(001).
               10  CADA-VLR-MEDIO-BENEFI-F        PIC X(001).
               10  CADA-FORMA-PAGTO-F             PIC X(001).
               10  CADA-MEIO-PAGTO-F              PIC X(001).
               10  CADA-NOME-EMBOSSING-F          PIC X(001).
               10  CADA-ENTREGA-CENTRAL-F         PIC X(001).
               10  CADA-CNPJ-ENTREGA-F            PIC X(001).
               10  CADA-FATURA-CENTRAL-F          PIC X(001).
               10  CADA-CNPJ-FATURA-F             PIC X(001).
               10  CADA-CNPJ-COBRANCA-F           PIC X(001).
               10  CADA-FUNC-PEDIR-REEMISSAO-F    PIC X(001).
           05  CADA-MODIFICAR-INTER               PIC X(001).
           05  CADA-COD-AUT-CONTA                 PIC X(010).
           05  CADA-D3-CONTINUA.
               10  CADA-EMAIL-INTER-D             PIC X(040).
               10  CADA-EMAIL-INTER-O             PIC X(040).
4SYSJT     05  CADA-FUNCIONARIO                   PIC X(009).
           05  FILLER                             PIC X(024).
