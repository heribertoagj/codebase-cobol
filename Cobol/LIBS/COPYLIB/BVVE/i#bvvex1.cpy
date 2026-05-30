      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : RAMOS DE ATIVIDADE PARA GERACAO DA CARGA ZERO       *
      *  NOME INC: I#BVVEX1                                            *
      *  DATA    : JAN/2010                                            *
      *----------------------------------------------------------------*
      *   123456789012345678901234567890123456789012345678901234567890
       01  BVVEX1-DECRICOES-MOTIVO.
           05  FILLER                         PIC X(60)  VALUE
05007      "CODIGO TIPO DE PESSOA INVALIDA                            ".
            05  FILLER                         PIC X(60)  VALUE
05008      "CGC/CPF INVALIDO                                          ".
            05  FILLER                         PIC X(60)  VALUE
05009      "CODIGO DO RAMO DE ATIVIDADE INVALIDO                      ".
            05  FILLER                         PIC X(60)  VALUE
05010      "RAZAO SOCIAL NAO PREENCHIDA                               ".
            05  FILLER                         PIC X(60)  VALUE
05011      "DATA DE FUNDACAO INVALIDA                                 ".
            05  FILLER                         PIC X(60)  VALUE
05012      "CODIGO DO BANCO A CREDITO INVALIDO OU BANCO PREENC. E     ".
            05  FILLER                         PIC X(60)  VALUE
05013      "AGENCIA A CREDITO INVALIDA                                ".
            05  FILLER                         PIC X(60)  VALUE
05014      "CONTA CORRENTE A CREDITO INVALIDA                         ".
            05  FILLER                         PIC X(60)  VALUE
05015      "CODIGO BANCO/AGENCIA/CONTA CORRENTE REDESHOP DEBITO       ".
            05  FILLER                         PIC X(60)  VALUE
05017      "TIPO DE PESSOA FISICA NAO PODE SER CREDENCIADO POR ESTE   ".
            05  FILLER                         PIC X(60)  VALUE
05018      "CODIGO BANCO/AGENCIA/CONTA CORRENTE MAESTRO INVALIDO      ".
            05  FILLER                         PIC X(60)  VALUE
05019      "VENDA DE SERVICO NAO PERMITIDO PARA TIPO DE PESSOA        ".
            05  FILLER                         PIC X(60)  VALUE
05021      "CODIGO BANCO/AGENCIA/CONTA CORRENTE CONSTRUCARD INVALIDO  ".
            05  FILLER                         PIC X(60)  VALUE
05024      "ENDERECO NAO PREENCHIDO                                   ".
            05  FILLER                         PIC X(60)  VALUE
05026      "NUMERO DO ENDERECO NAO PREENCHIDO                         ".
            05  FILLER                         PIC X(60)  VALUE
05027      "BAIRRO NAO PREENCHIDO                                     ".
            05  FILLER                         PIC X(60)  VALUE
05028      "CIDADE NAO PREENCHIDA                                     ".
            05  FILLER                         PIC X(60)  VALUE
05029      "ESTADO (UNIDADE DA FEDERACAO) NAO PREENCHIDO              ".
            05  FILLER                         PIC X(60)  VALUE
05030      "CEP NAO PREENCHIDO                                        ".
            05  FILLER                         PIC X(60)  VALUE
05031      "COMPLEMENTO DO CEP NAO PREENCHIDO                         ".
            05  FILLER                         PIC X(60)  VALUE
05032      "ENDERECO DE CORRESPONDENCIA NAO PREENCHIDO                ".
            05  FILLER                         PIC X(60)  VALUE
05034      "NUMERO DO ENDERECO DE CORRESPONDENCIA NAO PREENCHIDO      ".
            05  FILLER                         PIC X(60)  VALUE
05035      "BAIRRO DE CORRESPONDENCIA NAO PREENCHIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05036      "CIDADE DE CORRESPONDENCIA NAO PREENCHIDA                  ".
            05  FILLER                         PIC X(60)  VALUE
05037      "ESTADO (UNIDADE DA FEDERACAO) DE CORRESPONDENCIA NAO      ".
            05  FILLER                         PIC X(60)  VALUE
05038      "CEP DE CORRESPONDENCIA NAO PREENCHIDO                     ".
            05  FILLER                         PIC X(60)  VALUE
05039      "COMPLEMENTO DO CEP DE CORRESPONDENCIA NAO PREENCHIDO      ".
            05  FILLER                         PIC X(60)  VALUE
05040      "NOME DA PESSOA DE CONTATO NAO PREENCHIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05044      "CODIGO DO DDD NAO PREENCHIDO                              ".
            05  FILLER                         PIC X(60)  VALUE
05045      "NUMERO DO TELEFONE NAO PREENCHIDO                         ".
            05  FILLER                         PIC X(60)  VALUE
05046      "NUMERO DO RAMAL NAO NUMERICO                              ".
            05  FILLER                         PIC X(60)  VALUE
05047      "DDD DO FAX PREENCHIDO E NUMERO DO FAX NAO PREENCHIDO      ".
            05  FILLER                         PIC X(60)  VALUE
05048      "NUMERO DO FAX NAO NUMERICO OU DDD DO FAX NAO              ".
            05  FILLER                         PIC X(60)  VALUE
05049      "DDD DO TELEFONE 2 PREENCHIDO E NUMERO DO TELEFONE 2 NAO   ".
            05  FILLER                         PIC X(60)  VALUE
05050      "NUMERO DO TELEFONE 2 NAO NUMERICO OU DDD DO TELEFONE 2    ".
            05  FILLER                         PIC X(60)  VALUE
05051      "NUMERO DO RAMAL 2 NAO NUMERICO                            ".
            05  FILLER                         PIC X(60)  VALUE
05053      "PLAQUETA PARTE 2 NAO PREENCHIDA                           ".
            05  FILLER                         PIC X(60)  VALUE
05055      "CODIGO DA GERENCIA INVALIDO                               ".
            05  FILLER                         PIC X(60)  VALUE
05056      "CODIGO DA CARTEIRA INVALIDO                               ".
            05  FILLER                         PIC X(60)  VALUE
05059      "CODIGO DO HORARIO DE FUNCIONAMENTO INVALIDO               ".
            05  FILLER                         PIC X(60)  VALUE
05062      "CODIGO DO TIPO DE ESTABELECIMENTO INVALIDO                ".
            05  FILLER                         PIC X(60)  VALUE
05063      "INDICADOR DE COMERCIALIZACAO INVALIDO                     ".
            05  FILLER                         PIC X(60)  VALUE
05068      "NOME COMERCIAL PARA FARURA NAO PREENCHIDO                 ".
            05  FILLER                         PIC X(60)  VALUE
05071      "TIPO DE CONSIGNACAO INVALIDO                              ".
            05  FILLER                         PIC X(60)  VALUE
05073      "INDICADOR DE IATA INVALIDO                                ".
            05  FILLER                         PIC X(60)  VALUE
05074      "LOCAL DE PAGAMENTO INVALIDO                               ".
            05  FILLER                         PIC X(60)  VALUE
05076      "PRAZO OU TAXA NAO PREENCHIDO OU INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05078      "PRAZO OU TAXA NAO PREENCHIDO OU INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05080      "PRAZO OU TAXA NAO PREENCHIDO OU INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05082      "PRAZO OU TAXA NAO PREENCHIDO OU INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05084      "PRAZO OU TAXA NAO PREENCHIDO OU INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05086      "PRAZO OU TAXA NAO PREENCHIDO OU INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05088      "PRAZO OU TAXA NAO PREENCHIDO OU INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05090      "PRAZO OU TAXA NAO PREENCHIDO OU INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05092      "PRAZO OU TAXA NAO PREENCHIDO OU INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05094      "INDICADOR DE SOLICITACAO DE TECNOLOGIA INVALIDO           ".
            05  FILLER                         PIC X(60)  VALUE
05095      "CODIGO DO EQUIPAMENTO INVALIDO                            ".
            05  FILLER                         PIC X(60)  VALUE
05096      "QUANTIDADE SUPEIROR AO LIMITE ESTABELECIDO PELA REDECARD  ".
            05  FILLER                         PIC X(60)  VALUE
05097      "PROPRIEDADE DO EQUIPAMENTO INVALIDO                       ".
            05  FILLER                         PIC X(60)  VALUE
05098      "CODIGO DO TIPO DE LINHA INVALIDO                          ".
            05  FILLER                         PIC X(60)  VALUE
05101      "ENDERECO DE INSTALACAO DE TECNOLOGIA NAO PREENCHIDO       ".
            05  FILLER                         PIC X(60)  VALUE
05103      "NUMERO DO ENDERECO DE INSTALACAO DE TECNOLOGIA NAO        ".
            05  FILLER                         PIC X(60)  VALUE
05104      "BAIRRO DE INSTALACAO DE TECNOLOGIA NAO PREENCHIDO         ".
            05  FILLER                         PIC X(60)  VALUE
05105      "CIDADE DE INSTALACAO DE TECNOLOGIA NAO PREENCHIDA         ".
            05  FILLER                         PIC X(60)  VALUE
05106      "ESTADO (UNIDADE DA FEDERACAO) DE INSTALACAO DE TECNOL.    ".
            05  FILLER                         PIC X(60)  VALUE
05107      "CEP DE INSTALACAO DE TECNOLOGIA NAO PREENCHIDO            ".
            05  FILLER                         PIC X(60)  VALUE
05108      "COMPLEMENTO DO CEP DE INSTALACAO DE TECNOLOGIA NAO        ".
            05  FILLER                         PIC X(60)  VALUE
05117      "PRECO DO ALUGUEL DO EQUIPAMENTO INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
05121      "QTDE DE PROPRIETARIOS EXCEDEU O LIMITE PERMITIDO          ".
            05  FILLER                         PIC X(60)  VALUE
05122      "NOME DO PROPRIETARIO INVALIDO                             ".
            05  FILLER                         PIC X(60)  VALUE
05123      "DATA DE NASCIMENTO DO PROPRIETARIO INVALIDA               ".
            05  FILLER                         PIC X(60)  VALUE
05124      "CPF DO PROPRIETARIO INVALIDO                              ".
            05  FILLER                         PIC X(60)  VALUE
05128      "CODIGO DO CANAL INVALIDO                                  ".
            05  FILLER                         PIC X(60)  VALUE
05129      "CELULA NAO NUMERICA                                       ".
            05  FILLER                         PIC X(60)  VALUE
05131      "CODIGO DA AGENCIA INVALIDA                                ".
            05  FILLER                         PIC X(60)  VALUE
05132      "CPF DO VENDEDOR INVALIDO                                  ".
            05  FILLER                         PIC X(60)  VALUE
05133      "CODIGO DO INSTALADOR DE EQUIPAMENTO INVALIDO              ".
            05  FILLER                         PIC X(60)  VALUE
05134      "DATA DE CADASTRAMENTO DA PROPOSTA INVALIDA                ".
            05  FILLER                         PIC X(60)  VALUE
05200      "PERCENTUAL DE PARTICIPACAO DO PROPRIETARIO INVALIDO       ".
            05  FILLER                         PIC X(60)  VALUE
05201      "PROBLEMA NOS CAMPOS RELACIONADO AOS PROPRIETARIOS         ".
            05  FILLER                         PIC X(60)  VALUE
05202      "PROPRIETARIO REPETIDO                                     ".
            05  FILLER                         PIC X(60)  VALUE
05203      "TIPO DE PESSOA JURÍDICA E PROPRIETARIO IGUAL AO           ".
            05  FILLER                         PIC X(60)  VALUE
05204      "TIPO DE PESSOA FÍSICA E PROPRIETARIO DIFERENTE DO CPF     ".
            05  FILLER                         PIC X(60)  VALUE
05205      "PERCENTUAL DE PARTICIPACAO MAIOR QUE 100%                 ".
            05  FILLER                         PIC X(60)  VALUE
05206      "PLAQUETA PARTE 1 INCONSISTENTE                            ".
            05  FILLER                         PIC X(60)  VALUE
05207      "PLAQUETA PARTE 2 INCONSISTENTE                            ".
            05  FILLER                         PIC X(60)  VALUE
05208      "NOME COMERCIAL PARA FATURA INCONSISTENTE                  ".
            05  FILLER                         PIC X(60)  VALUE
05300      "TIPO E SUBTIPO DE REGISTRO INEXISTENTE                    ".
            05  FILLER                         PIC X(60)  VALUE
05301      "SEQUENCIA DE TIPO E SUBTIPO INVALIDA                      ".
            05  FILLER                         PIC X(60)  VALUE
05302      "MOVIMENTO SEM REGISTRO TIPO 1 - HEADER                    ".
            05  FILLER                         PIC X(60)  VALUE
05303      "NUMERACAO DO LOTE NAO E NUMERICO OU ESTA ZERADA           ".
            05  FILLER                         PIC X(60)  VALUE
05304      "DATA DO LOTE NAO ESTA VALIDO                              ".
            05  FILLER                         PIC X(60)  VALUE
05305      "HORA DO LOTE NAO E NUMERICO OU ESTA ZERADA                ".
            05  FILLER                         PIC X(60)  VALUE
05306      "CODIGO DE ORIGEM NAO E NUMERICO                           ".
            05  FILLER                         PIC X(60)  VALUE
05307      "NUMERO DO MICROFILME NAO E NUMERICO OU ESTE ZERADA        ".
            05  FILLER                         PIC X(60)  VALUE
05308      "COD. IDENTIFIC. GERADOR ARQ. NAO NUMERICO OU ESTA ZERADA  ".
            05  FILLER                         PIC X(60)  VALUE
05309      "SEQUENCIA DO NUMERO DO LOTE NAO ESTA VALIDO               ".
            05  FILLER                         PIC X(60)  VALUE
05310      "CODIGO DO TIPO DE MOVIMENTO DIFERENTE DE 'E' E 'R'        ".
            05  FILLER                         PIC X(60)  VALUE
05311      "NUMERO DA SOLICITACAO (OS) NAO E NUMERICO                 ".
            05  FILLER                         PIC X(60)  VALUE
05312      "NUMERO DO PV NAO E NUMERICO                               ".
            05  FILLER                         PIC X(60)  VALUE
05313      "NUMERO DO PV DEVE ESTAR PREENCHIDO                        ".
            05  FILLER                         PIC X(60)  VALUE
05314      "CNPJ COM DAC ESTA INVALIDO                                ".
            05  FILLER                         PIC X(60)  VALUE
05315      "CPF COM DAC ESTA INVALIDO                                 ".
            05  FILLER                         PIC X(60)  VALUE
05316      "DATA DE FUNDACAO DO ESTABELECIMENTO ESTA INVALIDO         ".
            05  FILLER                         PIC X(60)  VALUE
05317      "CODIGO DA FILIAL DE FILIACAO NAO E NUMERICO               ".
            05  FILLER                         PIC X(60)  VALUE
05318      "CODIGO TIPO COMERC. MARKETING CATAL. DIFERENTE DE 'S'/'N' ".
            05  FILLER                         PIC X(60)  VALUE
05319      "CODIGO TIPO COMERC. MARKETING TEL. DIFERENTE DE 'S'/'N'   ".
            05  FILLER                         PIC X(60)  VALUE
05320      "COD. TIPO COMERC. MARKETING ELETRON.DIFERENTE DE 'S'/'N'  ".
            05  FILLER                         PIC X(60)  VALUE
05321      "DATA DE CADASTRAMENTO DA PROPOSTA ESTA INVALIDA           ".
            05  FILLER                         PIC X(60)  VALUE
05322      "COD.IND.LOCALIZACAO FISICA DIFER. DE 'S', 'R','M' E 'C'   ".
            05  FILLER                         PIC X(60)  VALUE
05323      "CONSERVACAO DA LOJA DIFERENTE DE 'B', 'M' E 'R'           ".
            05  FILLER                         PIC X(60)  VALUE
05324      "COD.IDENT. REGIAO LOJA DIFER.DE 'B', 'C', 'P', 'S' E ' '  ".
            05  FILLER                         PIC X(60)  VALUE
05325      "VALOR DA TAXA DE ADESAO NAO E NUMERICO OU ESTA ZERADO     ".
            05  FILLER                         PIC X(60)  VALUE
05326      "INDICADOR SE TRABALHA C/ CARTAO VISA DIFER. DE 'S' E 'N'  ".
            05  FILLER                         PIC X(60)  VALUE
05327      "INDICADOR SE TRABALHA C/ CARTAO AMEX DIFER. DE 'S' E 'N'  ".
            05  FILLER                         PIC X(60)  VALUE
05328      "INDIC. SE TRABALHA C/ CARTAO OUTROS DIFER. DE 'S' E 'N'   ".
            05  FILLER                         PIC X(60)  VALUE
05329      "END.CORRESPOND. E IGUAL AO COMERCIAL DIFER. DE 'S' E 'N'  ".
            05  FILLER                         PIC X(60)  VALUE
05330      "IDENTIFICA TIPO DE ENDERECO NAO E NUMER. OU ESTA ZERADO   ".
            05  FILLER                         PIC X(60)  VALUE
05331      "TIPO OPERACAO DOMICILIO BANCARIO NAO E NUMER. OU ESTA     ".
            05  FILLER                         PIC X(60)  VALUE
05332      "CODIGO CCA NAO E NUMERICO OU ESTA ZERADO                  ".
            05  FILLER                         PIC X(60)  VALUE
05333      "INDICADOR DE DEBITO OU CREDITO DIFER. DE 'C' ,'D' E ' '   ".
            05  FILLER                         PIC X(60)  VALUE
05334      "CODIGO FEAT NAO E NUMERICO OU ESTA ZERADO                 ".
            05  FILLER                         PIC X(60)  VALUE
05335      "TAXA CREDITO NAO E NUMERICO OU ESTA ZERADO                ".
            05  FILLER                         PIC X(60)  VALUE
05336      "VALOR LIMITE DE PARCELAS DO PATAMAR NAO E NUMERICO        ".
            05  FILLER                         PIC X(60)  VALUE
05337      "PRODUTO (CCA X FEAT) S/ASSOCIACAO C/DOMIC.BANC.OU VICE-   ".
            05  FILLER                         PIC X(60)  VALUE
05338      "PRODUTO(CCA X FEAT)C/DIAS PRAZO/VAL TX/TIPO OPE C/DADOS   ".
            05  FILLER                         PIC X(60)  VALUE
05339      "SEQUENCIA DE PATAMARES NAO E NUMERICO                     ".
            05  FILLER                         PIC X(60)  VALUE
05340      "VALOR DO PATAMAR DE NAO E NUMERICO                        ".
            05  FILLER                         PIC X(60)  VALUE
05341      "VALOR DO PATAMAR ATE NAO E NUMERICO                       ".
            05  FILLER                         PIC X(60)  VALUE
05342      "VALOR DA TAXA DE DESCONTO NAO E NUMERICO                  ".
            05  FILLER                         PIC X(60)  VALUE
05343      "PATAMAR SEM CORRESPONDENTE COM PRODUTO                    ".
            05  FILLER                         PIC X(60)  VALUE
05344      "DADOS DE PATAMAR INCOMPLETOS                              ".
            05  FILLER                         PIC X(60)  VALUE
05345      "NUMERO DO TELEFONE P/ INSTALACAO NAO E NUMERICO           ".
            05  FILLER                         PIC X(60)  VALUE
05346      "NUMERO DO DDD DO TELEFONE P/ INSTALACAO NAO PREENCHIDO    ".
            05  FILLER                         PIC X(60)  VALUE
05347      "NUMERO DO RAMAL P/ INSTALACAO NAO E NUMERICO              ".
            05  FILLER                         PIC X(60)  VALUE
05348      "CODIGO DO NUMERO DO RENPAC NAO E NUMERICO                 ".
            05  FILLER                         PIC X(60)  VALUE
05349      "DIA INICIO DE FUNCIONAMENTO NAO E NUMERICO                ".
            05  FILLER                         PIC X(60)  VALUE
05350      "DIA FINAL DE FUNCIONAMENTO NAO E NUMERICO                 ".
            05  FILLER                         PIC X(60)  VALUE
05351      "HORA DE INICIO DE FUNCIONAMENTO SEMANAL NAO E NUMERICO    ".
            05  FILLER                         PIC X(60)  VALUE
05352      "HORA FINAL DE FUNCIONAMENTO SEMANAL NAO E NUMERICO        ".
            05  FILLER                         PIC X(60)  VALUE
05353      "CODIGO DO REGIME DA TECNOLOGIA NAO E NUMERICO             ".
            05  FILLER                         PIC X(60)  VALUE
05354      "CODIGO DO CENTRO DE CUSTO NAO E NUMERICO                  ".
            05  FILLER                         PIC X(60)  VALUE
05355      "PRECO VIGENTE DO EQUIPAMENTO NA DATA NAO E NUMERICO       ".
            05  FILLER                         PIC X(60)  VALUE
05356      "CODIGO FILIAL TECNOLOGIA NAO E NUMERICO                   ".
            05  FILLER                         PIC X(60)  VALUE
05357      "END.INSTALACAO E IGUAL AO COMERC.DIFER. 'S' , 'N' OU ' '  ".
            05  FILLER                         PIC X(60)  VALUE
05358      "DATA DE NASCIMENTO DO PROPRIETARIO ESTA INVALIDA          ".
            05  FILLER                         PIC X(60)  VALUE
05359      "CODIGO DA CENTRALIZADORA NAO E NUMERICO                   ".
            05  FILLER                         PIC X(60)  VALUE
05360      "CODIGO DO TIPO DE MOVIMENTO DIFERENTE DE 'T'              ".
            05  FILLER                         PIC X(60)  VALUE
05361      "REGISTRO DETALHE C/NUMERO/DATA/HORA LOTE DIFERENTE DO     ".
            05  FILLER                         PIC X(60)  VALUE
05362      "TOTAL DE REG. DETALHES NAO CONFERE C/O TOTAL DO TRAILLER  ".
            05  FILLER                         PIC X(60)  VALUE
05363      "NR DO PV NAO PODE SER INFORMADO P/ UM CREDENCIAMENTO      ".
            05  FILLER                         PIC X(60)  VALUE
05364      "NR DO PV DEVE SER INFORMADO P/ UM RECREDENCIAMENTO        ".
            05  FILLER                         PIC X(60)  VALUE
05365      "CODIGO DE VENDA DE SERVICO INVALIDO                       ".
            05  FILLER                         PIC X(60)  VALUE
05366      "CODIGO DE REGIME NAO E NUMERICO                           ".
            05  FILLER                         PIC X(60)  VALUE
05367      "QTDE MINIMA DE CONSULTA DE SERVICO NAO E NUMERICO         ".
            05  FILLER                         PIC X(60)  VALUE
05368      "CONSTRUCARD DEVE SER SOMENTE BANCO 104 - CEF              ".
            05  FILLER                         PIC X(60)  VALUE
30008      "COD. DE FILIAL INEXISTENTE                                ".
            05  FILLER                         PIC X(60)  VALUE
30017      "PARA PESSOA FISICA PERMITIDO APENAS UM PROPRIETARIO       ".
            05  FILLER                         PIC X(60)  VALUE
30019      "FAIXA DE CEP DENTRO DE OUTRA FAIXA                        ".
            05  FILLER                         PIC X(60)  VALUE
30026      "NUCLEO INEXISTENTE                                        ".
            05  FILLER                         PIC X(60)  VALUE
30027      "CODIGO CEP INVALIDO                                       ".
            05  FILLER                         PIC X(60)  VALUE
30043      "COD. DE CANAL INEXISTENTE                                 ".
            05  FILLER                         PIC X(60)  VALUE
30047      "COD. DE CANAL X CELULA INEXISTENTE                        ".
            05  FILLER                         PIC X(60)  VALUE
30051      "COD. DE RAMO INEXISTENTE                                  ".
            05  FILLER                         PIC X(60)  VALUE
30057      "COD. DE BANCO INVALIDO                                    ".
            05  FILLER                         PIC X(60)  VALUE
30072      "COD. DA AGENCIA INEXISTENTE                               ".
            05  FILLER                         PIC X(60)  VALUE
30094      "COD. DE FILIAL ENCONTRA-SE CANCELADO                      ".
            05  FILLER                         PIC X(60)  VALUE
30121      "TIPO ESTABELECIMENTO INVALIDO                             ".
            05  FILLER                         PIC X(60)  VALUE
30122      "CODIGO REGIME INVALIDO                                    ".
            05  FILLER                         PIC X(60)  VALUE
30135      "CODIGO DO REGIME INVALIDO                                 ".
            05  FILLER                         PIC X(60)  VALUE
30136      "COD. DO GRUPO INVALIDO                                    ".
            05  FILLER                         PIC X(60)  VALUE
30139      "COD. DO GRUPO INEXISTENTE                                 ".
            05  FILLER                         PIC X(60)  VALUE
30143      "NAO EXISTE REGIME PADRAO ASSOCIADO AO RAMO                ".
            05  FILLER                         PIC X(60)  VALUE
30155      "TIPO DE OPERACAO INVALIDO                                 ".
            05  FILLER                         PIC X(60)  VALUE
30168      "CODIGO DO REGIME ENCONTRA-SE CANCELADO                    ".
            05  FILLER                         PIC X(60)  VALUE
30193      "COD. DA CENTRALIZADORA DE PAGAMENTO INEXISTENTE           ".
            05  FILLER                         PIC X(60)  VALUE
30209      "PONTO DE VENDA NAO CADASTRADO                             ".
            05  FILLER                         PIC X(60)  VALUE
30220      "T. OPER. DO BANCO INCOMPATIVEL COM MODO DE OP. DO PV      ".
            05  FILLER                         PIC X(60)  VALUE
30228      "CEP NAO PERTENCE A FILIAL INFORMADA                       ".
            05  FILLER                         PIC X(60)  VALUE
30244      "MATRIZ INVALIDA                                           ".
            05  FILLER                         PIC X(60)  VALUE
30247      "BANCO/AGENCIA INVALIDOS                                   ".
            05  FILLER                         PIC X(60)  VALUE
30248      "CENTRALIZADORA DE TIER PRICING INVALIDA                   ".
            05  FILLER                         PIC X(60)  VALUE
30249      "CENTRALIZADORA DE TIER PRICING INFORMADA TEM CENTRAL      ".
            05  FILLER                         PIC X(60)  VALUE
30250      "CENTRALIZADORA DE TIER PRICING CANCELADA                  ".
            05  FILLER                         PIC X(60)  VALUE
30261      "COD. DE RAMO CANCELADO                                    ".
            05  FILLER                         PIC X(60)  VALUE
30262      "COD. DE ZONA CANCELADO                                    ".
            05  FILLER                         PIC X(60)  VALUE
30263      "COD. DE NUCLEO CANCELADO                                  ".
            05  FILLER                         PIC X(60)  VALUE
30265      "CANAL/CELULA CANCELADO                                    ".
            05  FILLER                         PIC X(60)  VALUE
30266      "COD. DE GRUPO CANCELADO                                   ".
            05  FILLER                         PIC X(60)  VALUE
30272      "CODIGO DO REGIME NAO ESTA LIBERADO                        ".
            05  FILLER                         PIC X(60)  VALUE
30281      "PONTO DE VENDA ESTA NO ARQUIVO MORTO                      ".
            05  FILLER                         PIC X(60)  VALUE
30283      "NUMERO DA MATRIZ ENCONTRA-SE CANCELADO                    ".
            05  FILLER                         PIC X(60)  VALUE
30284      "COD. DA CENTRALIZADORA DE PAGAMENTO CANCELADA             ".
            05  FILLER                         PIC X(60)  VALUE
30285      "CENTR. DE PAGAMENTO INFORMADA TEM OUTRA CENTRALIZADO      ".
            05  FILLER                         PIC X(60)  VALUE
30286      "OPERACOES DA CENTR. PAGTO <> DAS DO PV                    ".
            05  FILLER                         PIC X(60)  VALUE
30288      "RADICAL DE CGC DIFERENTE DO PV MATRIZ                     ".
            05  FILLER                         PIC X(60)  VALUE
30291      "FORMA DE PAGTO INVALIDA PARA ESSE T. DE OPERACAO          ".
            05  FILLER                         PIC X(60)  VALUE
30293      "ZONA INVALIDA PARA ESSA FILIAL                            ".
            05  FILLER                         PIC X(60)  VALUE
30297      "NOME DA REGIONAL INVALIDO                                 ".
            05  FILLER                         PIC X(60)  VALUE
30312      "NAO EXISTE REGIME MINIMO ASSOCIADO AO RAMO                ".
            05  FILLER                         PIC X(60)  VALUE
30313      "NAO EXISTE REGIME DE EXCECAO ASSOCIADO AO RAMO            ".
            05  FILLER                         PIC X(60)  VALUE
30424      "OCORREU ERRO NAO PREVISTO NA LEITURA                      ".
            05  FILLER                         PIC X(60)  VALUE
30447      "CCA/FEATURE INVALIDA PARA RAMO DE ATIVIDADE               ".
            05  FILLER                         PIC X(60)  VALUE
30481      "CGC OU CPF INVALIDO                                       ".
            05  FILLER                         PIC X(60)  VALUE
30501      "CPF INVALIDO PARA MATRIZES/FILIAIS                        ".
            05  FILLER                         PIC X(60)  VALUE
30502      "RAMO DE ATIVIDADE INVALIDO PARA CPF.                      ".
            05  FILLER                         PIC X(60)  VALUE
30503      "PV AUTONOMO C/ RADICAL DE CGC IGUAL A UM PV MATRIZ        ".
            05  FILLER                         PIC X(60)  VALUE
30504      "LOCAL PAGAMENTO INVALIDO PARA AUTONOMOS                   ".
            05  FILLER                         PIC X(60)  VALUE
30505      "EXISTE DOMICILIO BANCARIO DUPLICADO P/CGC DIFERENTE       ".
            05  FILLER                         PIC X(60)  VALUE
30507      "CENTRALIZADORA PAGAMENTO NAO PERTENCE A REDE              ".
            05  FILLER                         PIC X(60)  VALUE
30511      "CPF DO PROPRIETARIO DEVE SER IGUAL AO CPF DO PV           ".
            05  FILLER                         PIC X(60)  VALUE
30512      "CGC DO PROPRIETARIO DEVE SER DIFERENTE DO CGC DO PV       ".
            05  FILLER                         PIC X(60)  VALUE
30515      "AGENCIA DE FILIACAO NAO CADASTRADA                        ".
            05  FILLER                         PIC X(60)  VALUE
30516      "CANAL/CELULA NAO TEM CONTRATO DE FILIACAO                 ".
            05  FILLER                         PIC X(60)  VALUE
30517      "INFORMAR AGENCIA DE FILIACAO                              ".
            05  FILLER                         PIC X(60)  VALUE
30523      "CELULA DEVE SER MESMO DO DOMICILIO BANCARIO               ".
            05  FILLER                         PIC X(60)  VALUE
30538      "RAMO NAO POSSUI CLASSIFICACAO DE RISCO.                   ".
            05  FILLER                         PIC X(60)  VALUE
30548      "OCORRENCIA NAO CADASTRADA                                 ".
            05  FILLER                         PIC X(60)  VALUE
30555      "NUMERO DE PV INVALIDO                                     ".
            05  FILLER                         PIC X(60)  VALUE
30556      "DOMICILIO BANCARIO INVALIDO                               ".
            05  FILLER                         PIC X(60)  VALUE
30557      "LOCAL DE PAGTO INVALIDO                                   ".
            05  FILLER                         PIC X(60)  VALUE
30559      "PV AUTONOMO COM RADICAL DE CGC DUPLICADO.                 ".
            05  FILLER                         PIC X(60)  VALUE
30562      "FEATURE PARCEIRO NAO AUTORIZ. PARA SOLICITACAO 2 VIA      ".
            05  FILLER                         PIC X(60)  VALUE
30616      "CONSIGNADOR SOMENTE PARA MATRIZ E AUTONOMO                ".
            05  FILLER                         PIC X(60)  VALUE
30617      "CONSIGNATARIO DEVE SER AUTONOMO                           ".
            05  FILLER                         PIC X(60)  VALUE
30618      "CONSIGNADOR INEXISTENTE                                   ".
            05  FILLER                         PIC X(60)  VALUE
30620      "CARTEIRA INVALIDA PARA ESTA GERENCIA                      ".
            05  FILLER                         PIC X(60)  VALUE
30621      "PV CONSIGNADOR CANCELADO                                  ".
            05  FILLER                         PIC X(60)  VALUE
30622      "PV NAO E CONSIGNADOR                                      ".
            05  FILLER                         PIC X(60)  VALUE
30623      "TIPO DE COMERCIALIZACAO INVALIDO                          ".
            05  FILLER                         PIC X(60)  VALUE
30627      "PV NAO HABILITADO PARA ENVIO DE MAQUINETA                 ".
            05  FILLER                         PIC X(60)  VALUE
30643      "PV CONSIGNADOR COM CGC DIFERENTE DO CONSIGNATARIO         ".
            05  FILLER                         PIC X(60)  VALUE
30644      "RAMO DO PV DEVE SER IGUAL AO DA CENTRALIZADORA            ".
            05  FILLER                         PIC X(60)  VALUE
30650      "DOMICILIO BANC DIFERENTE DA CENTRALIZADORA                ".
            05  FILLER                         PIC X(60)  VALUE
30652      "CENTRALIZADORA NAO POSSUI PROD. CRED                      ".
            05  FILLER                         PIC X(60)  VALUE
30665      "DUPLICACAO NAO PERMITIDA PARA ESTE CNPJ/CPF               ".
            05  FILLER                         PIC X(60)  VALUE
30666      "RAMO NAO E TARGET                                         ".
            05  FILLER                         PIC X(60)  VALUE
30667      "TIPO DE CONSIGNACAO INVALIDA                              ".
            05  FILLER                         PIC X(60)  VALUE
30669      "SERVICO NAO PERMITIDO PARA ESTE RAMO                      ".
            05  FILLER                         PIC X(60)  VALUE
30682      "VALOR DE TARIFA INVALIDA                                  ".
            05  FILLER                         PIC X(60)  VALUE
30683      "QUANTIDADE DE DIAS DA TARIFA INVALIDA                     ".
            05  FILLER                         PIC X(60)  VALUE
30688      "SEM ALCADA CADASTRADA                                     ".
            05  FILLER                         PIC X(60)  VALUE
30689      "RAMO NAO PERMITE UTILIZACAO DE TARIFA                     ".
            05  FILLER                         PIC X(60)  VALUE
30690      "RAMO NAO PERMITE UTILIZACAO DE TAXA                       ".
            05  FILLER                         PIC X(60)  VALUE
30691      "CEP ATENDIDO SOMENTE PELO CORREIO                         ".
            05  FILLER                         PIC X(60)  VALUE
30708      "CENTRALIZADORA DE MATERIAL NAO EXISTE                     ".
            05  FILLER                         PIC X(60)  VALUE
30709      "CENTRALIZADORA DE MATERIAL CANCELADA                      ".
            05  FILLER                         PIC X(60)  VALUE
30710      "CENTRALIZADORA DE MATERIAL E CENTRALIZADA                 ".
            05  FILLER                         PIC X(60)  VALUE
30711      "RADICAL DE CNPJ NAO PERTENCE A REDE                       ".
            05  FILLER                         PIC X(60)  VALUE
30713      "ATENCAO! TAXA DE FILIACAO DEVE SER COBRADA INTEGRAL       ".
            05  FILLER                         PIC X(60)  VALUE
30728      "RAMO INFORMADO NAO EXISTE NA REDE                         ".
            05  FILLER                         PIC X(60)  VALUE
30731      "SERVICO COBRANCA DOC NAO PERMITIDO P/ BCO SISTEMA         ".
            05  FILLER                         PIC X(60)  VALUE
30738      "RAMO INFORMADO NAO EXISTE NA REDE                         ".
            05  FILLER                         PIC X(60)  VALUE
30761      "SERVICO PAGAMENTO DE CONTAS EXIGE TECNOLOGIA PDV          ".
            05  FILLER                         PIC X(60)  VALUE
30806      "PV QUE OPERA EM US$ NAO DEVE TER TAXA DE ADESAO           ".
            05  FILLER                         PIC X(60)  VALUE
30807      "CNPJ/CPF NAO CONFERE COM A MAQUINETA                      ".
            05  FILLER                         PIC X(60)  VALUE
30811      "EXISTE PV COM CATEG 'E' NESTA REDE,VERIFIQUE ALCADA       ".
            05  FILLER                         PIC X(60)  VALUE
30812      "EXISTE PV CANCELADO NESTA REDE, VERIFIQUE!!!              ".
            05  FILLER                         PIC X(60)  VALUE
30814      "DATA ALT.DOMIC. PV CENTR.PGTO MENOR QUE 01/05/1998        ".
            05  FILLER                         PIC X(60)  VALUE
30815      "DATA ALT.DOMIC. PV MATRIZ MENOR QUE 01/05/1998            ".
            05  FILLER                         PIC X(60)  VALUE
30816      "REDE COM PVS CANC. FRAUDE E DOM.BANC. INFERIOR 05/98      ".
            05  FILLER                         PIC X(60)  VALUE
30850      "ERRO NA CHAMADA DO WF                                     ".
            05  FILLER                         PIC X(60)  VALUE
30869      "CELULA ENCONTRA-SE CANCELADA                              ".
            05  FILLER                         PIC X(60)  VALUE
30900      "CODIGO DA AGENCIA BANCARIA ENCONTRA-SE CANCELADO          ".
            05  FILLER                         PIC X(60)  VALUE
60001      "ESTABELECIMENTO JA ESTA NA BASE DE PROPOSTA DA REDECARD   ".
            05  FILLER                         PIC X(60)  VALUE
60002      "ESTABELECIMENTO JA ESTA NA BASE DE PROPOSTA DA REDECARD   ".
            05  FILLER                         PIC X(60)  VALUE
60003      "ESTABELECIMENTO JA ESTA NA BASE DE PROPOSTA DA REDECARD   ".
            05  FILLER                         PIC X(60)  VALUE
60004      "ESTABELECIMENTO JA ESTA NA BASE DE PROPOSTA DA REDECARD   ".
            05  FILLER                         PIC X(60)  VALUE
60005      "ESTABELECIMENTO JA ESTA NA BASE DE PROPOSTA DA REDECARD   ".
            05  FILLER                         PIC X(60)  VALUE
66661      "ESTABELECIMENTO JA ESTA NA BASE DE PROPOSTA DA REDECARD   ".
            05  FILLER                         PIC X(60)  VALUE
66662      "ESTABELECIMENTO JA ESTA CADASTRADO NA REDECARD            ".
            05  FILLER                         PIC X(60)  VALUE
66663      "CEP NAO ATENDIDO PELO CORREIO - POLÍTICA SITE VISIT       ".
            05  FILLER                         PIC X(60)  VALUE
66664      "CEP INFORMADO NAO TEM FILIAL CADASTRADA NA REDECARD       ".
            05  FILLER                         PIC X(60)  VALUE
66665      "NAO EXISTE ZONA CADASTRADA PARA A FILIAL DA REDECARD      ".
            05  FILLER                         PIC X(60)  VALUE
66666      "PRAZO OU TAXA NAO CADASTRADA NA REDECARD                  ".
            05  FILLER                         PIC X(60)  VALUE
66667      "ESTABELECIMENTO JA ESTA NA BASE DE PROPOSTA POR OUTRO     ".
            05  FILLER                         PIC X(60)  VALUE
66668      "ESTABELECIMENTO JA ESTA NA BASE DE PROPOSTA POR OUTRO     ".
            05  FILLER                         PIC X(60)  VALUE
66669      "ESTABELECIMENTO JA ESTA NA BASE DE PROPOSTA MAS NAO E     ".
            05  FILLER                         PIC X(60)  VALUE
66670      "ESTABELECIMENTO MATRIZ ESTA CANCELADO NA REDECARD         ".
            05  FILLER                         PIC X(60)  VALUE
66671      "ESTABELECIMENTO CANCELADO, RECREDENCIAMENTO NAO PODE SER  ".
            05  FILLER                         PIC X(60)  VALUE
66680      "NRO DE PARCELAS INICIAL DO PRIMEIRO PATAMAR DEVE SER 2    ".
            05  FILLER                         PIC X(60)  VALUE
66681      "NRO DE PATAMARES SUPERIOR AO LIMITE DE 3                  ".
            05  FILLER                         PIC X(60)  VALUE
66682      "NRO DE PARCELAS ENTRE PATAMARES NAO ESTA SUBSEQUENTE      ".
            05  FILLER                         PIC X(60)  VALUE
66683      "VALOR DA TAXA DOS PATAMARES NAO ESTA DIFERENCIADA         ".
            05  FILLER                         PIC X(60)  VALUE
66684      "CANAL NAO PERMITE TRABALHAR COM VARIOS PATAMARES          ".
            05  FILLER                         PIC X(60)  VALUE
66685      "QTDE DE PATAMARES MAIOR QUE O PERMITIDO                   ".
            05  FILLER                         PIC X(60)  VALUE
66686      "LIMITE DE PARCELAS INFORMADO CCA X FEAT FORA DOS LIMITES  ".
            05  FILLER                         PIC X(60)  VALUE
66687      "LIMITE DE PARCELAS DIFERENTE C/O ULTIMO LIMITE DOS PATAM. ".
            05  FILLER                         PIC X(60)  VALUE
66688      "PROPOSTA EM ANALISE PELA REDECARD                         ".
            05  FILLER                         PIC X(60)  VALUE
66700      "ESTABELECIMENTO NAO CADASTRADO                            ".
            05  FILLER                         PIC X(60)  VALUE
66701      "RADICAL DO CNPJ INFORMADO NAO PERTENCE AO NUMERO DO PV    ".
            05  FILLER                         PIC X(60)  VALUE
66702      "CNPJ/CPF INFORMADO NAO PERTENCE AO NUMERO DO PONTO DE     ".
            05  FILLER                         PIC X(60)  VALUE
66703      "VALOR DO ALUGUEL DA TECNOLOGIA NAO E O PADRAO             ".
            05  FILLER                         PIC X(60)  VALUE
66704      "EXISTE UMA VENDA DE TECNOLOGIA PENDENTE                   ".
            05  FILLER                         PIC X(60)  VALUE
66706      "VENDA 'POS',NAO FOI POSSIVEL.ESTAB.JA CONTA C/ESTA TECNOL.".
            05  FILLER                         PIC X(60)  VALUE
66707      "VENDA 'POS',NAO FOI POSSIVEL.CONSTA P/ESTAB.TECNOL. 'LTR' ".
            05  FILLER                         PIC X(60)  VALUE
66708      "VENDA 'LTR',NAO FOI POSSIVEL.ESTAB.JA CONTA C/ALGUMA TECN.".
            05  FILLER                         PIC X(60)  VALUE
66709      "ESTABELECIMENTO ESTA COM DADOS CADASTRAIS DESATUALIZADOS  ".
            05  FILLER                         PIC X(60)  VALUE
66710      "VENDA TECN., NAO FOI POSSIVEL. ESTAB.JA CONTA C/ALGUMA    ".
            05  FILLER                         PIC X(60)  VALUE
70000      "INDICADOR DE SEQUENCIA INVALIDO                           ".
            05  FILLER                         PIC X(60)  VALUE
70001      "NUMERO DA SOLICITACAO INVALIDO                            ".
            05  FILLER                         PIC X(60)  VALUE
70002      "DATA DA ABERTURA DA SOLICITACAO INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
70003      "DATA DE FUNDACAO DO SERASA INVALIDO                       ".
            05  FILLER                         PIC X(60)  VALUE
70004      "COD.DA IMPRESSORA FISCAL INVALIDO                         ".
            05  FILLER                         PIC X(60)  VALUE
70005      "VALOR DO FATURAMENTO MIN.MENSAL INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
70006      "COD. DA FINANCEIRA INVALIDO                               ".
            05  FILLER                         PIC X(60)  VALUE
70007      "COD. DA FINANCEIRA 2 INVALIDO                             ".
            05  FILLER                         PIC X(60)  VALUE
70008      "COD. DA FINANCEIRA 3 INVALIDO                             ".
            05  FILLER                         PIC X(60)  VALUE
70009      "CODIGO DA FASE DE FILIACAO INVALIDO                       ".
            05  FILLER                         PIC X(60)  VALUE
70010      "DATA DA SITUACAO DA PROPOSTA INVALIDO                     ".
            05  FILLER                         PIC X(60)  VALUE
70011      "COD. MOTIVO DA RECUSA INVALIDO                            ".
            05  FILLER                         PIC X(60)  VALUE
70012      "COD. PESO TARGET INVALIDO                                 ".
            05  FILLER                         PIC X(60)  VALUE
70013      "COD. PESO CNPJ INVALIDO                                   ".
            05  FILLER                         PIC X(60)  VALUE
70014      "COD. PESO GERAL INVALIDO                                  ".
            05  FILLER                         PIC X(60)  VALUE
70015      "DT EXECUCAO DA MATRIZ DE RISCO INVALIDO                   ".
            05  FILLER                         PIC X(60)  VALUE
70016      "COD. MES PARA RECEBER O RAV INVALIDO                      ".
            05  FILLER                         PIC X(60)  VALUE
70017      "DT. ULTIMA ATUALIZACAO INVALIDO                           ".
            05  FILLER                         PIC X(60)  VALUE
70018      "DT. INCLUSAO INVALIDO                                     ".
            05  FILLER                         PIC X(60)  VALUE
70019      "DT. DE ENVIO PARA FORCA DE VDAS INVALIDO                  ".
            05  FILLER                         PIC X(60)  VALUE
70020      "IND. CONF. DOMICILIO INVALIDO                             ".
            05  FILLER                         PIC X(60)  VALUE
70021      "DT. ENVIO P/ CONF. INVALIDO                               ".
            05  FILLER                         PIC X(60)  VALUE
70022      "DT. RETORNO DE CONF. INVALIDO                             ".
            05  FILLER                         PIC X(60)  VALUE
70023      "CODIGO DE REGIMES DE PRODUTOS INVALIDO                    ".
            05  FILLER                         PIC X(60)  VALUE
70024      "CODIGO DE REGIMES DE PATAMARES INVALIDO                   ".
            05  FILLER                         PIC X(60)  VALUE
70025      "QUANTIDADE DE CHECK-OUT INVALIDO                          ".
            05  FILLER                         PIC X(60)  VALUE
70026      "DT NASC. SERASA INVALIDO                                  ".
            05  FILLER                         PIC X(60)  VALUE
70027      "COD-PESO-CPF INVALIDO                                     ".
            05  FILLER                         PIC X(60)  VALUE
70028      "CODIGO DO PESO CPF SERASA INVALIDO                        ".
            05  FILLER                         PIC X(60)  VALUE
70035      "INDICADOR PINPAD INVALIDO.                                ".
            05  FILLER                         PIC X(60)  VALUE
70036      "CODIGO TIPO LIGACAO INVALIDO.                             ".
            05  FILLER                         PIC X(60)  VALUE
70040      "PRAZO MAESTRO CDC INVALIDO.                               ".
            05  FILLER                         PIC X(60)  VALUE
70041      "PRODUTO MAESTRO CDC SEM PRODUTO DEBITO A VISTA            ".
            05  FILLER                         PIC X(60)  VALUE
99995      "PROBLEMA NA INCLUSAO DA TABELA 1 - AVISAR REDECARD        ".
            05  FILLER                         PIC X(60)  VALUE
99996      "PROBLEMA NA INCLUSAO DA TABELA 2 - AVISAR REDECARD        ".
            05  FILLER                         PIC X(60)  VALUE
99997      "PROBLEMA NA INCLUSAO DA TABELA 5 - AVISAR REDECARD        ".
            05  FILLER                         PIC X(60)  VALUE
99998      "PROBLEMA NA INCLUSAO DA TABELA 7 - AVISAR REDECARD        ".
            05  FILLER                         PIC X(60)  VALUE
99999      "PROBLEMA NA INCLUSAO DA TABELA 10 - AVISAR REECARD        ".
       01  FILLER  REDEFINES  BVVEX1-DECRICOES-MOTIVO.
           05  BVVEX1-TABDESCR-MOTIVO      PIC X(60)  OCCURS  337.
      *
       01  BVVEX1-DADOS-MOTIVO.
           05  FILLER              PIC X(05)  VALUE "05007".
           05  FILLER              PIC X(05)  VALUE "05008".
           05  FILLER              PIC X(05)  VALUE "05009".
           05  FILLER              PIC X(05)  VALUE "05010".
           05  FILLER              PIC X(05)  VALUE "05011".
           05  FILLER              PIC X(05)  VALUE "05012".
           05  FILLER              PIC X(05)  VALUE "05013".
           05  FILLER              PIC X(05)  VALUE "05014".
           05  FILLER              PIC X(05)  VALUE "05015".
           05  FILLER              PIC X(05)  VALUE "05017".
           05  FILLER              PIC X(05)  VALUE "05018".
           05  FILLER              PIC X(05)  VALUE "05019".
           05  FILLER              PIC X(05)  VALUE "05021".
           05  FILLER              PIC X(05)  VALUE "05024".
           05  FILLER              PIC X(05)  VALUE "05026".
           05  FILLER              PIC X(05)  VALUE "05027".
           05  FILLER              PIC X(05)  VALUE "05028".
           05  FILLER              PIC X(05)  VALUE "05029".
           05  FILLER              PIC X(05)  VALUE "05030".
           05  FILLER              PIC X(05)  VALUE "05031".
           05  FILLER              PIC X(05)  VALUE "05032".
           05  FILLER              PIC X(05)  VALUE "05034".
           05  FILLER              PIC X(05)  VALUE "05035".
           05  FILLER              PIC X(05)  VALUE "05036".
           05  FILLER              PIC X(05)  VALUE "05037".
           05  FILLER              PIC X(05)  VALUE "05038".
           05  FILLER              PIC X(05)  VALUE "05039".
           05  FILLER              PIC X(05)  VALUE "05040".
           05  FILLER              PIC X(05)  VALUE "05044".
           05  FILLER              PIC X(05)  VALUE "05045".
           05  FILLER              PIC X(05)  VALUE "05046".
           05  FILLER              PIC X(05)  VALUE "05047".
           05  FILLER              PIC X(05)  VALUE "05048".
           05  FILLER              PIC X(05)  VALUE "05049".
           05  FILLER              PIC X(05)  VALUE "05050".
           05  FILLER              PIC X(05)  VALUE "05051".
           05  FILLER              PIC X(05)  VALUE "05053".
           05  FILLER              PIC X(05)  VALUE "05055".
           05  FILLER              PIC X(05)  VALUE "05056".
           05  FILLER              PIC X(05)  VALUE "05059".
           05  FILLER              PIC X(05)  VALUE "05062".
           05  FILLER              PIC X(05)  VALUE "05063".
           05  FILLER              PIC X(05)  VALUE "05068".
           05  FILLER              PIC X(05)  VALUE "05071".
           05  FILLER              PIC X(05)  VALUE "05073".
           05  FILLER              PIC X(05)  VALUE "05074".
           05  FILLER              PIC X(05)  VALUE "05076".
           05  FILLER              PIC X(05)  VALUE "05078".
           05  FILLER              PIC X(05)  VALUE "05080".
           05  FILLER              PIC X(05)  VALUE "05082".
           05  FILLER              PIC X(05)  VALUE "05084".
           05  FILLER              PIC X(05)  VALUE "05086".
           05  FILLER              PIC X(05)  VALUE "05088".
           05  FILLER              PIC X(05)  VALUE "05090".
           05  FILLER              PIC X(05)  VALUE "05092".
           05  FILLER              PIC X(05)  VALUE "05094".
           05  FILLER              PIC X(05)  VALUE "05095".
           05  FILLER              PIC X(05)  VALUE "05096".
           05  FILLER              PIC X(05)  VALUE "05097".
           05  FILLER              PIC X(05)  VALUE "05098".
           05  FILLER              PIC X(05)  VALUE "05101".
           05  FILLER              PIC X(05)  VALUE "05103".
           05  FILLER              PIC X(05)  VALUE "05104".
           05  FILLER              PIC X(05)  VALUE "05105".
           05  FILLER              PIC X(05)  VALUE "05106".
           05  FILLER              PIC X(05)  VALUE "05107".
           05  FILLER              PIC X(05)  VALUE "05108".
           05  FILLER              PIC X(05)  VALUE "05117".
           05  FILLER              PIC X(05)  VALUE "05121".
           05  FILLER              PIC X(05)  VALUE "05122".
           05  FILLER              PIC X(05)  VALUE "05123".
           05  FILLER              PIC X(05)  VALUE "05124".
           05  FILLER              PIC X(05)  VALUE "05128".
           05  FILLER              PIC X(05)  VALUE "05129".
           05  FILLER              PIC X(05)  VALUE "05131".
           05  FILLER              PIC X(05)  VALUE "05132".
           05  FILLER              PIC X(05)  VALUE "05133".
           05  FILLER              PIC X(05)  VALUE "05134".
           05  FILLER              PIC X(05)  VALUE "05200".
           05  FILLER              PIC X(05)  VALUE "05201".
           05  FILLER              PIC X(05)  VALUE "05202".
           05  FILLER              PIC X(05)  VALUE "05203".
           05  FILLER              PIC X(05)  VALUE "05204".
           05  FILLER              PIC X(05)  VALUE "05205".
           05  FILLER              PIC X(05)  VALUE "05206".
           05  FILLER              PIC X(05)  VALUE "05207".
           05  FILLER              PIC X(05)  VALUE "05208".
           05  FILLER              PIC X(05)  VALUE "05300".
           05  FILLER              PIC X(05)  VALUE "05301".
           05  FILLER              PIC X(05)  VALUE "05302".
           05  FILLER              PIC X(05)  VALUE "05303".
           05  FILLER              PIC X(05)  VALUE "05304".
           05  FILLER              PIC X(05)  VALUE "05305".
           05  FILLER              PIC X(05)  VALUE "05306".
           05  FILLER              PIC X(05)  VALUE "05307".
           05  FILLER              PIC X(05)  VALUE "05308".
           05  FILLER              PIC X(05)  VALUE "05309".
           05  FILLER              PIC X(05)  VALUE "05310".
           05  FILLER              PIC X(05)  VALUE "05311".
           05  FILLER              PIC X(05)  VALUE "05312".
           05  FILLER              PIC X(05)  VALUE "05313".
           05  FILLER              PIC X(05)  VALUE "05314".
           05  FILLER              PIC X(05)  VALUE "05315".
           05  FILLER              PIC X(05)  VALUE "05316".
           05  FILLER              PIC X(05)  VALUE "05317".
           05  FILLER              PIC X(05)  VALUE "05318".
           05  FILLER              PIC X(05)  VALUE "05319".
           05  FILLER              PIC X(05)  VALUE "05320".
           05  FILLER              PIC X(05)  VALUE "05321".
           05  FILLER              PIC X(05)  VALUE "05322".
           05  FILLER              PIC X(05)  VALUE "05323".
           05  FILLER              PIC X(05)  VALUE "05324".
           05  FILLER              PIC X(05)  VALUE "05325".
           05  FILLER              PIC X(05)  VALUE "05326".
           05  FILLER              PIC X(05)  VALUE "05327".
           05  FILLER              PIC X(05)  VALUE "05328".
           05  FILLER              PIC X(05)  VALUE "05329".
           05  FILLER              PIC X(05)  VALUE "05330".
           05  FILLER              PIC X(05)  VALUE "05331".
           05  FILLER              PIC X(05)  VALUE "05332".
           05  FILLER              PIC X(05)  VALUE "05333".
           05  FILLER              PIC X(05)  VALUE "05334".
           05  FILLER              PIC X(05)  VALUE "05335".
           05  FILLER              PIC X(05)  VALUE "05336".
           05  FILLER              PIC X(05)  VALUE "05337".
           05  FILLER              PIC X(05)  VALUE "05338".
           05  FILLER              PIC X(05)  VALUE "05339".
           05  FILLER              PIC X(05)  VALUE "05340".
           05  FILLER              PIC X(05)  VALUE "05341".
           05  FILLER              PIC X(05)  VALUE "05342".
           05  FILLER              PIC X(05)  VALUE "05343".
           05  FILLER              PIC X(05)  VALUE "05344".
           05  FILLER              PIC X(05)  VALUE "05345".
           05  FILLER              PIC X(05)  VALUE "05346".
           05  FILLER              PIC X(05)  VALUE "05347".
           05  FILLER              PIC X(05)  VALUE "05348".
           05  FILLER              PIC X(05)  VALUE "05349".
           05  FILLER              PIC X(05)  VALUE "05350".
           05  FILLER              PIC X(05)  VALUE "05351".
           05  FILLER              PIC X(05)  VALUE "05352".
           05  FILLER              PIC X(05)  VALUE "05353".
           05  FILLER              PIC X(05)  VALUE "05354".
           05  FILLER              PIC X(05)  VALUE "05355".
           05  FILLER              PIC X(05)  VALUE "05356".
           05  FILLER              PIC X(05)  VALUE "05357".
           05  FILLER              PIC X(05)  VALUE "05358".
           05  FILLER              PIC X(05)  VALUE "05359".
           05  FILLER              PIC X(05)  VALUE "05360".
           05  FILLER              PIC X(05)  VALUE "05361".
           05  FILLER              PIC X(05)  VALUE "05362".
           05  FILLER              PIC X(05)  VALUE "05363".
           05  FILLER              PIC X(05)  VALUE "05364".
           05  FILLER              PIC X(05)  VALUE "05365".
           05  FILLER              PIC X(05)  VALUE "05366".
           05  FILLER              PIC X(05)  VALUE "05367".
           05  FILLER              PIC X(05)  VALUE "05368".
           05  FILLER              PIC X(05)  VALUE "30008".
           05  FILLER              PIC X(05)  VALUE "30017".
           05  FILLER              PIC X(05)  VALUE "30019".
           05  FILLER              PIC X(05)  VALUE "30026".
           05  FILLER              PIC X(05)  VALUE "30027".
           05  FILLER              PIC X(05)  VALUE "30043".
           05  FILLER              PIC X(05)  VALUE "30047".
           05  FILLER              PIC X(05)  VALUE "30051".
           05  FILLER              PIC X(05)  VALUE "30057".
           05  FILLER              PIC X(05)  VALUE "30072".
           05  FILLER              PIC X(05)  VALUE "30094".
           05  FILLER              PIC X(05)  VALUE "30121".
           05  FILLER              PIC X(05)  VALUE "30122".
           05  FILLER              PIC X(05)  VALUE "30135".
           05  FILLER              PIC X(05)  VALUE "30136".
           05  FILLER              PIC X(05)  VALUE "30139".
           05  FILLER              PIC X(05)  VALUE "30143".
           05  FILLER              PIC X(05)  VALUE "30155".
           05  FILLER              PIC X(05)  VALUE "30168".
           05  FILLER              PIC X(05)  VALUE "30193".
           05  FILLER              PIC X(05)  VALUE "30209".
           05  FILLER              PIC X(05)  VALUE "30220".
           05  FILLER              PIC X(05)  VALUE "30228".
           05  FILLER              PIC X(05)  VALUE "30244".
           05  FILLER              PIC X(05)  VALUE "30247".
           05  FILLER              PIC X(05)  VALUE "30248".
           05  FILLER              PIC X(05)  VALUE "30249".
           05  FILLER              PIC X(05)  VALUE "30250".
           05  FILLER              PIC X(05)  VALUE "30261".
           05  FILLER              PIC X(05)  VALUE "30262".
           05  FILLER              PIC X(05)  VALUE "30263".
           05  FILLER              PIC X(05)  VALUE "30265".
           05  FILLER              PIC X(05)  VALUE "30266".
           05  FILLER              PIC X(05)  VALUE "30272".
           05  FILLER              PIC X(05)  VALUE "30281".
           05  FILLER              PIC X(05)  VALUE "30283".
           05  FILLER              PIC X(05)  VALUE "30284".
           05  FILLER              PIC X(05)  VALUE "30285".
           05  FILLER              PIC X(05)  VALUE "30286".
           05  FILLER              PIC X(05)  VALUE "30288".
           05  FILLER              PIC X(05)  VALUE "30291".
           05  FILLER              PIC X(05)  VALUE "30293".
           05  FILLER              PIC X(05)  VALUE "30297".
           05  FILLER              PIC X(05)  VALUE "30312".
           05  FILLER              PIC X(05)  VALUE "30313".
           05  FILLER              PIC X(05)  VALUE "30424".
           05  FILLER              PIC X(05)  VALUE "30447".
           05  FILLER              PIC X(05)  VALUE "30481".
           05  FILLER              PIC X(05)  VALUE "30501".
           05  FILLER              PIC X(05)  VALUE "30502".
           05  FILLER              PIC X(05)  VALUE "30503".
           05  FILLER              PIC X(05)  VALUE "30504".
           05  FILLER              PIC X(05)  VALUE "30505".
           05  FILLER              PIC X(05)  VALUE "30507".
           05  FILLER              PIC X(05)  VALUE "30511".
           05  FILLER              PIC X(05)  VALUE "30512".
           05  FILLER              PIC X(05)  VALUE "30515".
           05  FILLER              PIC X(05)  VALUE "30516".
           05  FILLER              PIC X(05)  VALUE "30517".
           05  FILLER              PIC X(05)  VALUE "30523".
           05  FILLER              PIC X(05)  VALUE "30538".
           05  FILLER              PIC X(05)  VALUE "30548".
           05  FILLER              PIC X(05)  VALUE "30555".
           05  FILLER              PIC X(05)  VALUE "30556".
           05  FILLER              PIC X(05)  VALUE "30557".
           05  FILLER              PIC X(05)  VALUE "30559".
           05  FILLER              PIC X(05)  VALUE "30562".
           05  FILLER              PIC X(05)  VALUE "30616".
           05  FILLER              PIC X(05)  VALUE "30617".
           05  FILLER              PIC X(05)  VALUE "30618".
           05  FILLER              PIC X(05)  VALUE "30620".
           05  FILLER              PIC X(05)  VALUE "30621".
           05  FILLER              PIC X(05)  VALUE "30622".
           05  FILLER              PIC X(05)  VALUE "30623".
           05  FILLER              PIC X(05)  VALUE "30627".
           05  FILLER              PIC X(05)  VALUE "30643".
           05  FILLER              PIC X(05)  VALUE "30644".
           05  FILLER              PIC X(05)  VALUE "30650".
           05  FILLER              PIC X(05)  VALUE "30652".
           05  FILLER              PIC X(05)  VALUE "30665".
           05  FILLER              PIC X(05)  VALUE "30666".
           05  FILLER              PIC X(05)  VALUE "30667".
           05  FILLER              PIC X(05)  VALUE "30669".
           05  FILLER              PIC X(05)  VALUE "30682".
           05  FILLER              PIC X(05)  VALUE "30683".
           05  FILLER              PIC X(05)  VALUE "30688".
           05  FILLER              PIC X(05)  VALUE "30689".
           05  FILLER              PIC X(05)  VALUE "30690".
           05  FILLER              PIC X(05)  VALUE "30691".
           05  FILLER              PIC X(05)  VALUE "30708".
           05  FILLER              PIC X(05)  VALUE "30709".
           05  FILLER              PIC X(05)  VALUE "30710".
           05  FILLER              PIC X(05)  VALUE "30711".
           05  FILLER              PIC X(05)  VALUE "30713".
           05  FILLER              PIC X(05)  VALUE "30728".
           05  FILLER              PIC X(05)  VALUE "30731".
           05  FILLER              PIC X(05)  VALUE "30738".
           05  FILLER              PIC X(05)  VALUE "30761".
           05  FILLER              PIC X(05)  VALUE "30806".
           05  FILLER              PIC X(05)  VALUE "30807".
           05  FILLER              PIC X(05)  VALUE "30811".
           05  FILLER              PIC X(05)  VALUE "30812".
           05  FILLER              PIC X(05)  VALUE "30814".
           05  FILLER              PIC X(05)  VALUE "30815".
           05  FILLER              PIC X(05)  VALUE "30816".
           05  FILLER              PIC X(05)  VALUE "30850".
           05  FILLER              PIC X(05)  VALUE "30869".
           05  FILLER              PIC X(05)  VALUE "30900".
           05  FILLER              PIC X(05)  VALUE "60001".
           05  FILLER              PIC X(05)  VALUE "60002".
           05  FILLER              PIC X(05)  VALUE "60003".
           05  FILLER              PIC X(05)  VALUE "60004".
           05  FILLER              PIC X(05)  VALUE "60005".
           05  FILLER              PIC X(05)  VALUE "66661".
           05  FILLER              PIC X(05)  VALUE "66662".
           05  FILLER              PIC X(05)  VALUE "66663".
           05  FILLER              PIC X(05)  VALUE "66664".
           05  FILLER              PIC X(05)  VALUE "66665".
           05  FILLER              PIC X(05)  VALUE "66666".
           05  FILLER              PIC X(05)  VALUE "66667".
           05  FILLER              PIC X(05)  VALUE "66668".
           05  FILLER              PIC X(05)  VALUE "66669".
           05  FILLER              PIC X(05)  VALUE "66670".
           05  FILLER              PIC X(05)  VALUE "66671".
           05  FILLER              PIC X(05)  VALUE "66680".
           05  FILLER              PIC X(05)  VALUE "66681".
           05  FILLER              PIC X(05)  VALUE "66682".
           05  FILLER              PIC X(05)  VALUE "66683".
           05  FILLER              PIC X(05)  VALUE "66684".
           05  FILLER              PIC X(05)  VALUE "66685".
           05  FILLER              PIC X(05)  VALUE "66686".
           05  FILLER              PIC X(05)  VALUE "66687".
           05  FILLER              PIC X(05)  VALUE "66688".
           05  FILLER              PIC X(05)  VALUE "66700".
           05  FILLER              PIC X(05)  VALUE "66701".
           05  FILLER              PIC X(05)  VALUE "66702".
           05  FILLER              PIC X(05)  VALUE "66703".
           05  FILLER              PIC X(05)  VALUE "66704".
           05  FILLER              PIC X(05)  VALUE "66706".
           05  FILLER              PIC X(05)  VALUE "66707".
           05  FILLER              PIC X(05)  VALUE "66708".
           05  FILLER              PIC X(05)  VALUE "66709".
           05  FILLER              PIC X(05)  VALUE "66710".
           05  FILLER              PIC X(05)  VALUE "70000".
           05  FILLER              PIC X(05)  VALUE "70001".
           05  FILLER              PIC X(05)  VALUE "70002".
           05  FILLER              PIC X(05)  VALUE "70003".
           05  FILLER              PIC X(05)  VALUE "70004".
           05  FILLER              PIC X(05)  VALUE "70005".
           05  FILLER              PIC X(05)  VALUE "70006".
           05  FILLER              PIC X(05)  VALUE "70007".
           05  FILLER              PIC X(05)  VALUE "70008".
           05  FILLER              PIC X(05)  VALUE "70009".
           05  FILLER              PIC X(05)  VALUE "70010".
           05  FILLER              PIC X(05)  VALUE "70011".
           05  FILLER              PIC X(05)  VALUE "70012".
           05  FILLER              PIC X(05)  VALUE "70013".
           05  FILLER              PIC X(05)  VALUE "70014".
           05  FILLER              PIC X(05)  VALUE "70015".
           05  FILLER              PIC X(05)  VALUE "70016".
           05  FILLER              PIC X(05)  VALUE "70017".
           05  FILLER              PIC X(05)  VALUE "70018".
           05  FILLER              PIC X(05)  VALUE "70019".
           05  FILLER              PIC X(05)  VALUE "70020".
           05  FILLER              PIC X(05)  VALUE "70021".
           05  FILLER              PIC X(05)  VALUE "70022".
           05  FILLER              PIC X(05)  VALUE "70023".
           05  FILLER              PIC X(05)  VALUE "70024".
           05  FILLER              PIC X(05)  VALUE "70025".
           05  FILLER              PIC X(05)  VALUE "70026".
           05  FILLER              PIC X(05)  VALUE "70027".
           05  FILLER              PIC X(05)  VALUE "70028".
           05  FILLER              PIC X(05)  VALUE "70035".
           05  FILLER              PIC X(05)  VALUE "70036".
           05  FILLER              PIC X(05)  VALUE "70040".
           05  FILLER              PIC X(05)  VALUE "70041".
           05  FILLER              PIC X(05)  VALUE "99995".
           05  FILLER              PIC X(05)  VALUE "99996".
           05  FILLER              PIC X(05)  VALUE "99997".
           05  FILLER              PIC X(05)  VALUE "99998".
           05  FILLER              PIC X(05)  VALUE "99999".
           05  FILLER              PIC X(05)  VALUE "05007".

        01  FILLER  REDEFINES  BVVEX1-DADOS-MOTIVO.
            05  BVVEX1-TABDADOS-MOTIVO  OCCURS  337.
                10  BVVEX1-TABDADOS-COD     PIC 9(05).
