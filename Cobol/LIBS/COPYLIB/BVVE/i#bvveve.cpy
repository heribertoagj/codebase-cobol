      ******************************************************************
      *    I#BVVEVE - TABELA DE ERROS RETORNADOS PELA VISA-VALE NO     *
      *               PROCESSAMENTO DAS PROPOSTAS ELETRONICAS          *
      *    LRECL: 1500                                                 *
      ******************************************************************
       01  FILLER.
           03  TABELA-ERROS-VV.
               05  FILLER              PIC X(104) VALUE
             '0001DATA DE REFERENCIA INVALIDA NO HEADER
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0002DESCRICAO DO ARQUIVO INVALIDA NO HEADER
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0003CANAL DE ENTRADA INVALIDO NO HEADER
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0004BANCO REMETENTE NAO NUMERICO NO HEADER
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0005AGENCIA REMETENTE NAO NUMERICA NO HEADER
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0006NOME DO BANCO NAO PREENCHIDO NO HEADER
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0007DDD DA AGENCIA NAO NUMERICO NO HEADER
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0008TELEF DA AGENCIA NAO NUMERICO NO HEADER
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0009NM PESSOA CONTATO NAO PREENCHIDO HEADER
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0010TELEF PESSOA CONTATO NAO NUMERICO HEADER
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0011QTD TOTAL REGISTROS INVALIDA NO TRAILLER
      -      '                          TRAILLER            '.
               05  FILLER              PIC X(104) VALUE
             '0012TIPO DE REGISTRO INVALIDO
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0013DO REGISTRO INVALIDO
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0014QUANTIDADE DE REGISTRO HEADER INVALIDO
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0015QUANTIDADE DE REGISTROS TRAILLER INVALIDO
      -      '                          TRAILLER            '.
               05  FILLER              PIC X(104) VALUE
             '0016NUMERO SEQUENCIAL DE REGISTROS INVALIDO
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0017BANCO / AGENCIA OU EMPRESA / FILIAL INEXISTENTE NO HEA
      -      'DER                       HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0018NUMERO DA REMESSA NAO NUMERICO NO HEADER
      -      '                          HEADER              '.
               05  FILLER              PIC X(104) VALUE
             '0019TIPO DE PESSOA INVALIDA NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0020COD DA OPERACAO INVALIDO NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0021RAIZ CNPJ NAO NUMERICO NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0022FILIAL CNPJ NAO NUMERICO NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0023DIGITO CNPJ NAO NUMERICO NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0024RZ SOCIAL NAO PREENCHIDO NO REGISTRO D11
      -      '                          RAZAO SOCIAL        '.
               05  FILLER              PIC X(104) VALUE
             '0025NM FANTASIA NAO PREENCHIDO NO REGISTRO D11
      -      '                          NOME FANTASIA       '.
               05  FILLER              PIC X(104) VALUE
             '0026TIPO DE EMPRESA INVALIDA NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0027ID EMPR HOLDING INVALIDA NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0028ID EMPR RESTRICAO FINANC NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0029RZ RESTRICAO INVALIDA NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0030SECAO EMPRESA NAO NUMERICO NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0031SECAO EMPRESA INVALIDO NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0032CD DIVISAO EMP NAO NUMERICO NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0033CD DIVISAO EMPR INVALIDO NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0034QTD FUNC EMPR NAO NUMERICO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0035FAT ANUAL EMPRESA NAO NUMER REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0036%PARTIC ACION NAO NUMERICA NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0037NM FILIAL NAO PREENCHIDA REGISTRO D11
      -      '                          NOME FILIAL         '.
               05  FILLER              PIC X(104) VALUE
             '0038QTD REGISTROS D11 E D12 ESTA DIFERENTE
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0039QTD REGISTROS D13 E D32 ESTA DIFERENTE
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0040QTD REGISTROS D41 E D42 ESTA DIFERENTE
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0041REGISTROS D11 E D12 ESTAO FORA DE ORDEM
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0042REGISTROS D31 E D32 ESTAO FORA DE ORDEM
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0043REGISTROS D41 E D42 ESTAO FORA DE ORDEM
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0044TIPO/NM LOGR NAO PREENCHIDO REGISTRO D12
      -      '                          LOGRADOURO          '.
               05  FILLER              PIC X(104) VALUE
             '0045NR LOGRAD NAO PREENCHIDO REGISTRO D12
      -      '                          NRO LOGRADOURO      '.
               05  FILLER              PIC X(104) VALUE
             '0046MUNICIPIO NAO PREENCHIDO REGISTRO D12
      -      '                          MUNICIPIO           '.
               05  FILLER              PIC X(104) VALUE
             '0047SIGLA DO UF NAO PREENCHIDO REGISTRO D12
      -      '                          ESTADO              '.
               05  FILLER              PIC X(104) VALUE
             '0048CEP NAO NUMERICO REGISTRO D12
      -      '                          CEP                 '.
               05  FILLER              PIC X(104) VALUE
             '0049DDD NAO PREENCHIDO REGISTRO D12
      -      '                          DDD                 '.
               05  FILLER              PIC X(104) VALUE
             '0050TELEF FIXO 1 NAO PREENCHIDO REGISTRO D12
      -      '                          TELEFONE            '.
               05  FILLER              PIC X(104) VALUE
             '0051LIMITE CREDITO NAO NUMERICO REGISTRO D12
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0052DT INICIO VALIDADE LIMITE CREDITO EMPRESA INVALIDA REG
      -      'ISTRO D12                                     '.
               05  FILLER              PIC X(104) VALUE
             '0053VENCTO LIMITE CREDITO INVALIDA REGISTRO D12
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0054INDICADOR DE LIM CRED POR CONTR INVALIDO REGISTRO D12
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0055RAIZ CNPJ NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0056FILIAL NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0057DIGITO NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0058NR CPF NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0059COD PROD MERCADO 1 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0060COD PROD MERCADO 2 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0061COD PROD MERCADO 3 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0062COD PROD MERCADO 4 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0063COD PROD MERCADO 5 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0064COD PROD MERCADO 6 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0065COD PROD MERCADO 7 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0066COD PROD MERCADO 8 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0067COD PROD MERCADO 9 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0068COD PROD MERCADO 10 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0069COD PROD MERCADO 11 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0070COD PROD MERCADO 12 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0071COD PROD MERCADO 13 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0072COD PROD MERCADO 14 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0073COD PROD MERCADO 15 NAO NUMERICO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0074COD OPERACAO PROD MERCADO 1 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0075COD OPERACAO PROD MERCADO 2 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0076COD OPERACAO PROD MERCADO 3 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0077COD OPERACAO PROD MERCADO 4 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0078COD OPERACAO PROD MERCADO 5 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0079COD OPERACAO PROD MERCADO 6 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0080COD OPERACAO PROD MERCADO 7 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0081COD OPERACAO PROD MERCADO 8 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0082COD OPERACAO PROD MERCADO 9 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0083COD OPERACAO PROD MERCADO 10 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0084COD OPERACAO PROD MERCADO 11 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0085COD OPERACAO PROD MERCADO 12 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0086COD OPERACAO PROD MERCADO 13 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0087COD OPERACAO PROD MERCADO 14 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0088COD OPERACAO PROD MERCADO 15 INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0089NUMERO DO CONTRATO NAO NUMERICO NO REGISTRO D31
      -      '                          NRO CONTRATO        '.
               05  FILLER              PIC X(104) VALUE
             '0090RAIZ DO CNPJ NAO NUMERICO NO REGISTRO D31
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0091FILIAL DO CNPJ NAO NUMERICO NO REGISTRO D31
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0092DIGITO DO CNPJ NAO NUMERICO NO REGISTRO D31
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0093CPF NAO NUMERICO NO REGISTRO D31
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0094INDIC. INTERLOC. DECISAO E FUNCIONARIO INVALIDO NO REG
      -      'ISTRO D31                 INTER D FUNC        '.
               05  FILLER              PIC X(104) VALUE
             '0095NOME INTERLOCUTOR DECISAO NAO PREENCHIDO NO REGISTRO D
      -      '31                        NOME INTER D        '.
               05  FILLER              PIC X(104) VALUE
             '0096CPF INTERLOCUTOR DECISAO NAO PREENCHIDO NO REGISTRO D3
      -      '1                         CPF INTER D         '.
               05  FILLER              PIC X(104) VALUE
             '0097DT NASCTO. INTERLOCUTOR DECISAO INVALIDA NO REGISTRO D
      -      '31                        DT NASC INTER D     '.
               05  FILLER              PIC X(104) VALUE
             '0098NIVEL CARGO INTERLOCUTOR DECISAO INVALIDO NO REGISTRO
      -      'D31                                           '.
               05  FILLER              PIC X(104) VALUE
             '0099TIPO E NOME LOGRAD. INTERLOCUTOR DECISAO NAO PREENCHID
      -      'O NO REGISTRO D31                             '.
               05  FILLER              PIC X(104) VALUE
             '0100NR LOGRAD.INTERLOCUTOR DECISAO NAO PREENCHIDO NO REGIS
      -      'TRO D31                                       '.
               05  FILLER              PIC X(104) VALUE
             '0101MUNICIPIO INTERLOCUTOR DECISAO NAO PREENCHIDO NO REGIS
      -      'TRO D31                                       '.
               05  FILLER              PIC X(104) VALUE
             '0102SIGLA ESTADO INTERLOCUTOR DECISAO NAO PREENCHIDO NO RE
      -      'GISTRO D31                                    '.
               05  FILLER              PIC X(104) VALUE
             '0103CEP INTERLOCUTOR DECISAO NAO NUMERICO NO REGISTRO D31
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0104DDD INTERLOCUTOR DECISAO NAO PREENCHIDO NO REGISTRO D3
      -      '1                                             '.
               05  FILLER              PIC X(104) VALUE
             '0105TELEFONE FIXO INTERLOCUTOR DECISAO NAO PREENCHIDO NO R
      -      'EGISTRO D31               TELEFONE INTER D    '.
               05  FILLER              PIC X(104) VALUE
             '0106NOME INTERLOCUTOR OPERACAO NAO PREENCHIDO NO REGISTRO
      -      'D32                       NOME INTER O        '.
               05  FILLER              PIC X(104) VALUE
             '0107DDD INTERLOCUTOR OPERACAO NAO PREENCHIDO NO REGISTRO D
      -      '32                                            '.
               05  FILLER              PIC X(104) VALUE
             '0108TELEFONE FIXO 1 INTERLOCUTOR OPERACAO NAO PREENCHIDO N
      -      'O REGISTRO D32            TELEFONE INTER O    '.
               05  FILLER              PIC X(104) VALUE
             '0109NUMERO DO CONTRATO NAO NUMERICO NO REGISTRO D41
      -      '                          CONTRATO            '.
               05  FILLER              PIC X(104) VALUE
             '0110NUMERO DA FICHA PROPOSTA NAO NUMERICO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0111RAIZ DO CNPJ NAO NUMERICO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0112FILIAL DO CNPJ NAO NUMERICO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0113DIGITO DO CNPJ NAO NUMERICO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0114CPF NAO NUMERICO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0115CODIGO DO PRODUTO NAO NUMERICO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0116CODIGO DO BANCO NAO NUMERICO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0117CODIGO DA AGENCIA BANCARIA NAO NUMERICO NO REGISTRO D4
      -      '1                                             '.
               05  FILLER              PIC X(104) VALUE
             '0118CODIGO DO IDENTIFICADOR DO VENDEDOR NAO PREENCHIDO NO
      -      'REGISTRO D41                                  '.
               05  FILLER              PIC X(104) VALUE
             '0119CODIGO INDICADOR DA IDENTIFICACAO DO VENDEDOR INVALIDO
      -      'NO REGISTRO D41                               '.
               05  FILLER              PIC X(104) VALUE
             '0120NOME DO VENDEDOR PF NAO PREENCHIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0121DDD DO VENDEDOR PF NAO PREENCHIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0122TELEFONE FIXO DO VENDEDOR PF NAO PREENCHIDO NO REGISTR
      -      'O D41                                         '.
               05  FILLER              PIC X(104) VALUE
             '0123RAMAL DO VENDEDOR PF NAO PREENCHIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0124DATA DA ASSINATURA DO CONTRATO INVALIDA NO REGISTRO D4
      -      '1                                             '.
               05  FILLER              PIC X(104) VALUE
             '0125QUANTIDADE DE FILIAIS PREVISTA INVALIDA NO REGISTRO D4
      -      '1                                             '.
               05  FILLER              PIC X(104) VALUE
             '0126QUANTIDADE TOTAL DE USUARIOS NAO NUMERICA NO REGISTRO
      -      'D41                       QTD FUNC BENEF      '.
               05  FILLER              PIC X(104) VALUE
             '0127VL MEDIO MENSAL INDIVIDUAL INVALIDO NO REGISTRO D41
      -      '                          VLR MEDIO BENEF     '.
               05  FILLER              PIC X(104) VALUE
             '0128INDICADOR DE PAGTO ANTECIPADO INVALIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0129INDICADOR DE MEIO DE PAGTO INVALIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0130COD BANCO PARA DEBITO EM C/C INVALIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0131COD AGENCIA PARA DEBITO C/C INVALIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0132NUMERO DA C/C PARA DEBITO INVALIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0133COD BANCO PARA COBRANCA BANCARIA INVALIDO NO REGISTRO
      -      'D41                                           '.
               05  FILLER              PIC X(104) VALUE
             '0134INDICADOR DE FATURAMENTO ELETRONICO INVALIDO NO REGIST
      -      'RO D41                                        '.
               05  FILLER              PIC X(104) VALUE
             '0135NOME DE EMBOSSING DA EMPRESA NAO PREENCHIDO NO REGISTR
      -      'O D41                     NOME EMBOSSING      '.
               05  FILLER              PIC X(104) VALUE
             '0136INDICADOR DE ENTREGA CENTRALIZADA INVALIDO NO REGISTRO
      -      'D41                       ENTREGA CENTRAL     '.
               05  FILLER              PIC X(104) VALUE
             '0137CNPJ DA FILIAL DE ENTREGA NAO NUMERICO NO REGISTRO D41
      -      '                          CNPJ ENTREGA        '.
               05  FILLER              PIC X(104) VALUE
             '0138INDICADOR DE FATURAMENTO CENTRALIZADO INVALIDO NO REGI
      -      'STRO D41                  FATURA CENTRAL      '.
               05  FILLER              PIC X(104) VALUE
             '0139CNPJ DA FILIAL DE FATURAMENTO NAO NUMERICO NO REGISTRO
      -      ' D41                      CNPJ FATURA         '.
               05  FILLER              PIC X(104) VALUE
             '0140CNPJ DA FILIAL DE COBRANCA NAO NUMERICO NO REGISTRO D4
      -      '1                         CNPJ COBRANCA       '.
               05  FILLER              PIC X(104) VALUE
             '0141INDICADOR DE ENTREGA NA RESIDENCIA INVALIDO NO REGISTR
      -      'O D42                                         '.
               05  FILLER              PIC X(104) VALUE
             '0142INDICADOR PERMISSAO USUARIO PEDIR REEMISSAO DE CARTAO
      -      'INVALIDO NO REGISTRO D42  REEMISSAO           '.
               05  FILLER              PIC X(104) VALUE
             '0143TIPO DE CARTAO INVALIDO NO REGISTRO D42
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0144CODIGO DA TECNOLOGIA INVALIDO NO REGISTRO D42
      -      ' D41                                          '.
               05  FILLER              PIC X(104) VALUE
             '0145VL DE CREDITO CONCEDIDO PELO BANCO NAO NUMERICO NO REG
      -      'ISTRO D42                                     '.
               05  FILLER              PIC X(104) VALUE
             '0146DATA DE INICIO DE VALIDADE LIMITE CREDITO INVALIDA NO
      -      'REGISTRO D42                                  '.
               05  FILLER              PIC X(104) VALUE
             '0147DATA DE VENCTO LIMITE DE CREDITO NAO NUMERICA NO REGIS
      -      'TRO D42                                       '.
               05  FILLER              PIC X(104) VALUE
             '0148DATA DE INICIO DE VALIDADE PRAZO PAGTO INVALIDA NO REG
      -      'ISTRO D42                                     '.
               05  FILLER              PIC X(104) VALUE
             '0149DATA DE VENCTO PRAZO DE PAGTO INVALIDA NO REGISTRO D42
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0150PRAZO DE PAGTO NAO NUMERICO NO REGISTRO D42
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0151NUMERO DO CONTRATO NAO NUMERICO NO REGISTRO D51
      -      '                          NRO CONTRATO        '.
               05  FILLER              PIC X(104) VALUE
             '0152CODIGO DO PRODUTO CONTRATO NAO NUMERICO NO REGISTRO D5
      -      '1                                             '.
               05  FILLER              PIC X(104) VALUE
             '0153RAIZ NO CNPJ DA EMPRESA NAO NUMERICO NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0154FILIAL DO CNPJ NAO NUMERICO NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0155DIGITO DO CNPJ NAO NUMERICO NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0156CPF NAO NUMERICO NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0157CODIGO DA TARIFA NAO NUMERICO NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0158DATA DE INICIO DE VALIDADE DA OPCAO INVALIDA NO REGIST
      -      'RO D51                                        '.
               05  FILLER              PIC X(104) VALUE
             '0159DATA DE FIM DE VALIDADE DA OPCAO INVALIDA NO REGISTRO
      -      'D51                                           '.
               05  FILLER              PIC X(104) VALUE
             '0160VALOR UNITARIO CONTRATADO NAO NUMERICO NO REGISTRO D51
      -      'ISTRO D42                                     '.
               05  FILLER              PIC X(104) VALUE
             '0161PERCENTUAL CONTRATADO NAO NUMERICO NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0162PERCENTUAL DE BONIFICACAO NAO NUMERICO NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0163QTD DE MESES DESCONTO NAO NUMERICO NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0164QTD DE MESES ISENCAO NAO NUMERICO NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0165QTD DE MESES BONIFICACAO NAO NUMERICO NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0166RAIZ NO CNPJ NAO NUMERICO NO HEADER
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0167FILIAL DO CNPJ NAO NUMERICO NO HEADER
      -      'TRO D42                                       '.
               05  FILLER              PIC X(104) VALUE
             '0168DIGITO DO CNPJ NAO NUMERICO NO HEADER
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0169INSCRICAO MUNICIPAL OU INSCRICAO ESTADUAL NAO PREENCHI
      -      'DAS NO REGISTRO D11                           '.
               05  FILLER              PIC X(104) VALUE
             '0170COD BANCO OU EMPRESA QUE DEU LIMITE DE CREDITO INEXIST
      -      'ENTE NO REGISTRO D12                          '.
               05  FILLER              PIC X(104) VALUE
             '0171COD BANCO OU EMPRESA QUE DEU LIM CRED NAO NUMERICO NO
      -      'REGISTRO D12                                  '.
               05  FILLER              PIC X(104) VALUE
             '0172CODIGO DO PRODUTO MERCADO INEXISTENTE
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0173CNPJ INVALIDO NO REGISTRO D11
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0174CNPJ INVALIDO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0175CNPJ INVALIDO NO REGISTRO D31
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0176CNPJ INVALIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0177CNPJ FILIAL DE ENTREGA INVALIDO NO REGISTRO D41
      -      '                          CNPJ ENTREGA        '.
               05  FILLER              PIC X(104) VALUE
             '0178CNPJ FILIAL DE FATURAMENTO INVALIDO NO REGISTRO D41
      -      '                          CNPJ FATURA         '.
               05  FILLER              PIC X(104) VALUE
             '0179CNPJ FILIAL DE COBRANCA INVALIDO NO REGISTRO D41
      -      '                          CNPJ COBRANCA       '.
               05  FILLER              PIC X(104) VALUE
             '0180CNPJ INVALIDO NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0181CODIGO DO IDENTIFICADOR DO VENDEDOR INVALIDO NO REGIST
      -      'RO D41                                        '.
               05  FILLER              PIC X(104) VALUE
             '0182CPF DO INTERLOCUTOR DE DECISAO INVALIDO NO REGISTRO D3
      -      '1                         CPF INTER D         '.
               05  FILLER              PIC X(104) VALUE
             '0183% DO CONTRATADO DEVE ESTAR ENTRE 0 E 100 NO REGISTRO D
      -      '51                                            '.
               05  FILLER              PIC X(104) VALUE
             '0184% DE BONIFICACAO DEVE ESTAR ENTRE 0 E 100 NO REGISTRO
      -      'D51                                           '.
               05  FILLER              PIC X(104) VALUE
             '0185NUMERO DO CONTRATO INEXISTENTE NO REGISTRO D31
      -      '                          NRO CONTRATO        '.
               05  FILLER              PIC X(104) VALUE
             '0186NUMERO DO CONTRATO INEXISTENTE NO REGISTRO D41
      -      '                          NRO CONTRATO        '.
               05  FILLER              PIC X(104) VALUE
             '0187NUMERO DO CONTRATO INEXISTENTE NO REGISTRO D51
      -      '                          NRO CONTRATO        '.
               05  FILLER              PIC X(104) VALUE
             '0188CODIGO DO PRODUTO INEXISTENTE NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0189CODIGO DO PRODUTO INEXISTENTE NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0190CODIGO DA TARIFA INEXISTENTE PARA O PRODUTO ESPECIFICA
      -      'DO NO REGISTRO D51                            '.
               05  FILLER              PIC X(104) VALUE
             '0191BANCO / AGENCIA OU EMPRESA / FILIAL QUE VENDEU INEXIST
      -      'ENTE NO REGISTRO D41                          '.
               05  FILLER              PIC X(104) VALUE
             '0192BANCO / AGENCIA PARA DEBITO EM C/C INEXISTENTE NO REGI
      -      'STRO D41                                      '.
               05  FILLER              PIC X(104) VALUE
             '0193BANCO PARA COBRANCA BANCARIA INEXISTENTE NO REGISTRO D
      -      '1                                             '.
               05  FILLER              PIC X(104) VALUE
             '0194PROBLEMAS COM REGISTRO D11 OU D12
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0196PROBLEMAS COM REGISTRO D31 OU D32
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0198PROBLEMAS COM REGISTRO D41 OU D42
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0199PROBLEMAS COM REGISTRO D42
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0200QUANTIDADE MAXIMA DE DIAS DA 1A. PARCELA INVALIDA NO R
      -      'EGISTRO D42                                   '.
               05  FILLER              PIC X(104) VALUE
             '0201INDICADOR DE MODIFICACAO DE LIMITE DE CREDITO INVALIDO
      -      ' NO REGISTRO D12                              '.
               05  FILLER              PIC X(104) VALUE
             '0202VALORES NAO ESTAO DENTRO DO MINIMO E MAXIMO PERMITIDO
      -      'NO REGISTRO D51                               '.
               05  FILLER              PIC X(104) VALUE
             '0203COMPONENTE CANCELADO, COMPONENTE DO MESMO TIPO NESTE A
      -      'RQUIVO DO REGISTRO D51                        '.
               05  FILLER              PIC X(104) VALUE
             '0204LIMITE EMPRESA CONTROLADO POR CONTRATO NO REGISTRO D12
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0205COD BCO QUE DEU LIMITE DE CREDITO ESTA DIFERENTE COD B
      -      'CO VENDEU NO REGISTRO D41                     '.
               05  FILLER              PIC X(104) VALUE
             '0206DT INICIO E FIM VALID DENTRO DE MAIS DE UM PRAZO CADAS
      -      'TRADO NO REGISTRO D42                         '.
               05  FILLER              PIC X(104) VALUE
             '0207COMPONENTE SEM IDENTIFICACAO DO CONTRATO OU CONTRATO I
      -      'NEXISTENTE NO REGISTRO D51                    '.
               05  FILLER              PIC X(104) VALUE
             '0208CNPJ NAO LOCALIZADO NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0209CNPJ INEXISTENTE NO REGISTRO D31
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0210CNPJ INEXISTENTE NO REGISTRO D21
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0211CNPJ INEXISTENTE NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0212CNPJ INEXISTENTE NO REGISTRO D51
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0213CONJUNTO DE REGISTRO INCOMPLETO
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0214FILIAL JA ESTA SENDO ABRANGIDA POR OUTRO CONTRATO NO R
      -      'EGISTRO D41                                   '.
               05  FILLER              PIC X(104) VALUE
             '0215TELEFONE FIXO 2 INVALIDO NO REGISTRO D12
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0216NUMERO DO FAX INVALIDO NO REGISTRO D12
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0217NR RAMAL INTERLOCUTOR DECISAO INVALIDO NO REGISTRO D31
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0218NR CELULAR INTERLOCUTOR DECISAO INVALIDO NO REGISTRO D
      -      '31                                            '.
               05  FILLER              PIC X(104) VALUE
             '0219NR FAX INTERLOCUTOR DECISAO INVALIDO NO REGISTRO D31
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0220RAMAL DO FAX DO INTERLOCUTOR DECISAO INVALIDO NO REGIS
      -      'TRO D32                                       '.
               05  FILLER              PIC X(104) VALUE
             '0221RAMAL 1 DO INTERLOCUTOR DE OPERCAO INVALIDO NO REGISTR
      -      'O D31                                         '.
               05  FILLER              PIC X(104) VALUE
             '0222TELEFONE FIXO 2 DO INTERLOCUTOR DE OPERACAO INVALIDO N
      -      'O REGISTRO D32                                '.
               05  FILLER              PIC X(104) VALUE
             '0223NUMERO DO RAMAL 2 DO INTERLOCUTOR DE OPERACAO INVALIDO
      -      ' NO REGISTRO D32                              '.
               05  FILLER              PIC X(104) VALUE
             '0224NUMERO DO FAX DO INTERLOCUTOR DE OPERACAO INVALIDO NO
      -      'REGISTRO D32                                  '.
               05  FILLER              PIC X(104) VALUE
             '0225RAMAL DO FAX DO INTERLOCUTOR DE OPERACAO INVALIDO NO R
      -      'EGISTRO D32                                   '.
               05  FILLER              PIC X(104) VALUE
             '0226RAMAL DO VENDEDOR PF INVALIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0227PRAZO PAGTO REG D42 DEVE SER INFORMADO QDO FORMA DE PA
      -      'GTO FOR A PRAZO REG D41                       '.
               05  FILLER              PIC X(104) VALUE
             '0228PROBLEMAS NO PROCESSAMENTO DO ARQUIVO DE PRODUTO CONTR
      -      'ATADO                                         '.
               05  FILLER              PIC X(104) VALUE
             '0229BCO QUE DEU LIMITE CREDITO OBRIGATORIO QDO INFORMADO V
      -      'L NO REGISTRO D12                             '.
               05  FILLER              PIC X(104) VALUE
             '0230VL LIMITE OBRIGATORIO QDO INFORMADO BCO QUE DEU LIMITE
      -      ' CREDITO NO REGISTRO D12                      '.
               05  FILLER              PIC X(104) VALUE
             '0231FILIAL DE FATURAMENTO NAO PERTENCENTE A EMPRESA CONTRA
      -      'TANTE NO REG D41          CNPJ FATURA         '.
               05  FILLER              PIC X(104) VALUE
             '0232FILIAL DE ENTREGA NAO PERTENCENTE A EMPRESA CONTRATANT
      -      'E NO REG D41              CNPJ ENTREGA        '.
               05  FILLER              PIC X(104) VALUE
             '0233FILIAL DE ENTREGA INEXISTENTE NO REG D41
      -      '                          CNPJ ENTREGA        '.
               05  FILLER              PIC X(104) VALUE
             '0234FILIAL DE FATURAMENTO INEXISTENTE NO REG D41
      -      '                          CNPJ FATURA         '.
               05  FILLER              PIC X(104) VALUE
             '0235CODIGO DA AUTORIZACAO NAO NUMERICO REGISTRO D42
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0236COD AGENCIA DEBITO C/C INVALIDO PARA O BANCO INFORMADO
      -      ' NO REG D41                                   '.
               05  FILLER              PIC X(104) VALUE
             '0237NUMERO DA C/C INVALIDO PARA O BANCO INFORMADO NO REG D
      -      '41                                            '.
               05  FILLER              PIC X(104) VALUE
             '0238NR CONTRATO NAO PODE ESTAR PREENCHIDO QDO COD OPERACAO
      -      ' FOR = N NO REGISTRO D11  NRO CONTRATO        '.
               05  FILLER              PIC X(104) VALUE
             '0239NR CONTRATO DEVE ESTAR PREENCHIDO QDO COD OPERACAO FOR
      -      ' = A NO REGISTRO D11      NRO CONTRATO        '.
               05  FILLER              PIC X(104) VALUE
             '0240EMPRESA DA FILIAL DE COBRANCA INEXISTENTE NO REG D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0241CONTRATO P/ CNPJ-BANCO VENDEDOR-PRODUTO INFORMADO JA C
      -      'ADASTRADO NO REGISTRO D41 NRO CONTRATO        '.
               05  FILLER              PIC X(104) VALUE
             '0242TIPO DE LAYOUT INVALIDO NO HEADER
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0243NAO PODE HAVER LIMITE POR EMPRESA QDO TIPO DE LAYOUT F
      -      'OR BANCO NO REGISTRO D12                      '.
               05  FILLER              PIC X(104) VALUE
             '0244COD AREA COML NAO NUMERICO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0245COD PARCEIRO EMISSOR INVALIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0246COD TIPO MERCADO INVALIDO NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0247COD AREA COMERCIAL INEXISTENTE NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0248COD COLABORADOR INEXISTENTE NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0249EMPR CONTRATANTE DEVE SER DO GRUPO DE PARCEIROS QDO TP
      -      ' MERCADO=1 NO REGISTRO D41                    '.
               05  FILLER              PIC X(104) VALUE
             '0300EMPR CONTRAT NAO DEVE SER DO GRUPO  DE PARCEIROS QDO T
      -      'P MERCADO=2 REGISTRO D41                      '.
               05  FILLER              PIC X(104) VALUE
             '0301CONTRATO FOI INSERIDO SEM ASSOCIACAO - BIN NO REGISTRO
      -      ' D41                                          '.
               05  FILLER              PIC X(104) VALUE
             '0302BANCO INVALIDO PARA DEBITO EM CONTA NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0303FORMA DE NEGOCIACAO INVALIDA NO REGISTRO D41
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0304FORMA DE NEGOCIACAO INVALIDA PARA BANCOS NO REGISTRO D
      -      '41                                            '.
               05  FILLER              PIC X(104) VALUE
             '0305CNPJ RAIZ DA EMPRESA CONTRATANTE INVALIDO NO REGISTRO
      -      'D11                                           '.
               05  FILLER              PIC X(104) VALUE
             '0306O BANCO REMETENTE DIFERE DO BANCO VENDEDOR NO CONTRATO
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0307NAO FOI POSSIVEL FAZER A MANUTENCAO DO CONTRATO POIS O
      -      ' MESMO ESTA CANCELADO                         '.
               05  FILLER              PIC X(104) VALUE
             '0308EMPRESA NAO PERTENCE AO GRUPO PARCEIRO (MERCADO=2)
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0310FORMA DE PAGAMENTO INVALIDA PARA O EMISSOR SELECIONADO
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0311CONTRATO C/MEIO PAGTO CREDITO CONTA, FORMA PAGTO ANTEC
      -      'IPADO E ANTECIPADO S/ NOTA                    '.
               05  FILLER              PIC X(104) VALUE
             '0315INDIC. INTERLOC. OPERACAO E FUNCIONARIO INVALIDO NO RE
      -      'GISTRO D31                INTER D FUNC        '.
               05  FILLER              PIC X(104) VALUE
             '0316CPF INTERLOCUTOR OPERACAO NAO NUMERICO NO REGISTRO D31
      -      '                          CPF INTER O         '.
               05  FILLER              PIC X(104) VALUE
             '0317CPF DO INTERLOCUTOR DE OPERACAO INVALIDO NO REGISTRO D
      -      '31                        CPF INTER O         '.
               05  FILLER              PIC X(104) VALUE
             '0318DT NASCTO. INTERLOCUTOR OPERACAO INVALIDA NO REGISTRO
      -      'D31                       DT NASC INTER O     '.
               05  FILLER              PIC X(104) VALUE
             '0319NIVEL CARGO INTERLOCUTOR OPERACAO INVALIDO NO REGISTRO
      -      ' D31                                          '.
               05  FILLER              PIC X(104) VALUE
             '0320CELULAR INTERLOCUTOR OPERACAO INVALIDO NO REGISTRO D31
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0321NAO HA SIMULACAO REALIZADA PARA O CNPJ DO CONTRATO - R
      -      'EG D41 E D42                                  '.
               05  FILLER              PIC X(104) VALUE
             '0322COD DE AUTORIZACAO INVALIDO OU NAO ENCONTRADO P/ O CNP
      -      'J DO CONTRATO REG D41/D42                     '.
               05  FILLER              PIC X(104) VALUE
             '0323CODIGO DE AUTORIZACAO DO CONTRATO COM DATA DE EXPIRACA
      -      'O VENCIDA-REG D42                             '.
               05  FILLER              PIC X(104) VALUE
             '0324CNPJ DA EMPRESA DO CONTRATO NAO CONFERE NA SIMULACAO R
      -      'EG D41/D42                                    '.
               05  FILLER              PIC X(104) VALUE
             '0325PRAZO CONTRATADO FORA DO LIMITE DA POLITICA COMERCIAL
      -      '- REG D42                                     '.
               05  FILLER              PIC X(104) VALUE
             '0326PRAZO CONTRATADO DIFERE DO PRAZO SIMULADO-REG D42
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0327VALORES DE TARIFAS DIFERENTES DOS VALORES SIMULADOS -
      -      'REG D51                                       '.
               05  FILLER              PIC X(104) VALUE
             '0328VALORES DE TARIFAS FORA DOS LIMITES DA POLITICA COMERC
      -      'IAL-REG D51                                   '.
               05  FILLER              PIC X(104) VALUE
             '0329CONTRATO REJEITADO. TARIFA FORA DA POLITICA COMERCIAL-
      -      'REG D51                                       '.
               05  FILLER              PIC X(104) VALUE
             '0330CONTRATO REJEITADO. EMPRESA PERTENCENTE A UM GRUPO DE
      -      'NEG. CANCELADO D41/D42                        '.
               05  FILLER              PIC X(104) VALUE
             '0331ESTE CONTR. CANCELA O GRP DA QUAL FAZ PARTE, POIS A AU
      -      'TORIZ. NAO CONFERE D41/D42                  '.
               05  FILLER              PIC X(104) VALUE
             '0332MODALIDADE DE FATURAMENTO NAO PERMITIDA PARA O PRODUTO
      -      ' CONTRATADO                                   '.
               05  FILLER              PIC X(104) VALUE
             '0333MODALIDADE DE ENTREGA NAO PERMITIDA PARA O PRODUTO CON
      -      'TRATADO                                       '.
               05  FILLER              PIC X(104) VALUE
             '033INDICADOR DE PEDIDO DE REEMISSAO DE CARTAO PELO USUARIO
      -      ' NAO PERMITIDO                                '.
               05  FILLER              PIC X(104) VALUE
             '0335PRODUTO CONTRATADO FORA DO PERIODO DE VENDA
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0336CONTRATO CANCELADO POR TERMINO DO PERIODO DE VENDA NAO
      -      ' PODE SER ALTERADO                            '.
               05  FILLER              PIC X(104) VALUE
             '0337CODIGO DO EMISSOR INVALIDO PARA A EMPRESA
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '0338CONTRATO REJEITADO CONTRATANTE/PROD/VENDEDOR JA CADAST
      -      'ROU WEB REG D41                               '.
               05  FILLER              PIC X(104) VALUE
             '0339EMPRESA COM RAMO DE ATIVIDADE/SECAO CNAE NAO CORRESPON
      -      'DENTES NO REG D11                             '.
               05  FILLER              PIC X(104) VALUE
             '1024CONDICAO COMERCIAL NAO ESTA CONFIGURADA PARA O PRODUTO
      -      ' E TIPO DE PAGAMENTO ESPEC                    '.
               05  FILLER              PIC X(104) VALUE
             '1026ERRO AO TENTAR OBTER A SIMULACAO
      -      '                                              '.
               05  FILLER              PIC X(104) VALUE
             '1061TARIFA OBRIGATORIA PRODUTO NAO INFORMADA REG D511
      -      '                                              '.

           03  FILLER                     REDEFINES TABELA-ERROS-VV.
               05  TAB-ERROS-VV           OCCURS 286
                                          ASCENDING KEY TAB-COD-ERRO-VV
                                          INDEXED BY IND-ERR-VV.
                   07  TAB-COD-ERRO-VV    PIC X(04).
                   07  TAB-DESC-ERRO-VV   PIC X(80).
                   07  TAB-CAMPO-ERRO-VV  PIC X(20).
