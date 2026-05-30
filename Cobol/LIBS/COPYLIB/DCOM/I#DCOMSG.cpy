      *********************************************************
      * INCLUDE   : I#DCOMSG                                  *
      * SISTEMA   : DCOM - DESCONTO COMERCIAL                 *
      * DESCRICAO : INCLUDE DE MENSAGENS                      *
      * CRIADO POR: POLEN TECHNOLOGIES - NOVEMBRO / 2005      *
      *                                                       *
      * - O RANGE DE CODIGOS DO 01 ATE O 80, ESTA RESERVADO   *
      *   PARA A CAMADA DE ACESSO A DADOS.                    *
      *                                                       *
      * - O RANGE DE CODIGOS DO 81 ATE O 90, ESTA RESERVADO   *
      *   PARA A CAMADA DE COMUNICACAO.                       *
      *   (ROTEADORES DA WEB E PROGRAMAS IMS)                 *
      *                                                       *
      * - O RANGE DE CODIGOS DO 91 ATE O 99, ESTA RESERVADO   *
      *   PARA OS RETORNOS DE DB2.                            *
      *                                                       *
      * - DEMAIS MENSAGENS ATENDEM A CAMADA DE NEGOCIOS.      *
      *                                                       *
      *********************************************************
       01  SG-MENSAGENS.
           05  LITERAIS.
               10  FILLER    PIC X(83) VALUE
           '0001SOLICITACAO EFETUADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0002SOLICITACAO EFETUADA. AMOSTRAGEM CONTINUA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0003SOLICITACAO NAO EFETUADA. CHAVE INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0004SOLICITACAO NAO EFETUADA. CHAVE DUPLICADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0005INSTRUCAO NAO INFORMADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0006COD.TABELA LOG DESC DIF.DE <RESUMIDO> OU <COMPLETO>
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0007testes007
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0008testes008
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0009testes009
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0010testes010
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0011testes011
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0012testes012
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0013testes013
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0014testes014
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0015testes015
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0016testes016
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0017testes017
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0018testes018
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0019testes019
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0020testes020
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0021testes021
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0022testes022
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0023testes023
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0024testes024
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0025testes025
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0026testes026
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0027testes027
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0028testes028
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0029testes029
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0030testes030
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0031testes031
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0032testes032
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0033testes033
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0034testes034
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0035testes035
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0036testes036
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0037testes037
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0038testes038
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0039testes039
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0040testes040
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0041testes041
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0042testes042
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0043testes043
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0044testes044
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0045testes045
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0046testes046
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0047testes047
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0048testes048
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0049testes049
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0050CAMPOS DE ENTRADA INVALIDOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0051DATA DE INICIO DE VIGENCIA NAO PODE SER ALTERADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0052ALTERACAO DE SUBPRODUTO NAO PERMITIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0053DATA FIM MENOR QUE DATA FIM ANTERIOR INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0054NAO EXISTEM CONFIGURACOES CADASTRADAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0055NAO EXISTEM PROCESSAMENTOS CADASTRADOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0056TESTE056
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0057TESTE057
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0058TESTE058
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0059TESTE059
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0060TESTE060
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0061TESTE061
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0062TESTE062
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0063TESTE063
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0064TESTE064
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0065TESTE065
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0066TESTE066
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0067TESTE067
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0068TESTE068
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0069TESTE069
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0070TESTE070
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0071TESTE071
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0072TESTE072
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0073TESTE073
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0074TESTE074
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0075TESTE075
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0076TESTE076
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0077TESTE077
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0078TESTE078
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0079TESTE079
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0080TESTE080
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0081TESTE081
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0082TESTE082
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0083TESTE083
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0084TESTE084
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0085TESTE085
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0086TESTE086
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0087TESTE087
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0088TESTE088
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0089TESTE089
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0090TESTE090
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0091TESTE091
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0092TESTE092
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0093TESTE093
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0094TESTE094
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0095TESTE095
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0096TESTE096
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0097TESTE097
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0098ERRO INTEGRIDADE, DADOS NAO ENCONTRADOS
      -    '                       '.
               10  FILLER.
                   15  FILLER      PIC X(58) VALUE
           '0099FUNCAO INDISPONIVEL NO MOMENTO - COD. DE IDENTIFICACAO'.
                   15  FILLER      PIC X(01) VALUE SPACES.
                   15  WRK-COD-ID  PIC 9(04) VALUE ZEROS.
                   15  FILLER      PIC X(03) VALUE ' - '.
                   15  WRK-PROGRAM PIC X(08) VALUE SPACES.
                   15  FILLER      PIC X(09) VALUE SPACES.
               10  FILLER    PIC X(83) VALUE
           '0100TESTE100
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0101NAO HA OPERACOES PARA OS PARAMETROS INFORMADOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0102OPERACAO NAO CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0103OPERACAO NAO PERTENCE A ESTA AGENCIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0104OPERACAO NAO ESTA PENDENTE DE FORMALIZACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0105OPERACAO NAO FOI FORMALIZADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0106NAO HA OPERACOES COM PARCELAS PARA FORMALIZACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0107NAO HA PARCELAS PENDENTES DE FORMALIZACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0108PARCELA NAO ENCONTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0109PARCELA NAO ESTA PENDENTE DE FORMALIZACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '011OPERACAO/PARCELA NAO ESTA PENDENTE DE FORMALIZACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0111OPERACAO/PARCELA NAO ESTA FORMALIZADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0112FORMALIZACAO OK
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0113CANCELAMENTO DA FORMALIZACAO OK
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0114ERRO NO ACESSO AO MODULO PLCC9420
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0115CODIGO DO PRODUTO INVALIDO.
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0116CODIGO DO SUBPRODUTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0117ANO DA OPERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0118SEQUENCIA DA OPERACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0119ROTEIRO INCOMPLETO  DADOS BASICOS NAO INCLUIDOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0120ROTEIRO INCOMPLETO  MEIOS DE LIBERACAO NAO CONCLUIDOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0121ROTEIRO INCOMPLETO  DIGITACAO TITULOS NAO INCLUIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0122DADOS BASICOS ATUALIZADOS. NECESSARIO VALIDAR DADOS COMP
      -    'LEMENTARES             '.
               10  FILLER    PIC X(83) VALUE
           '0123ROTEIRO INCOMPLETO  AVALISTAS NAO INCLUIDOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0124ROTEIRO INCOMPLETO  TESTEMUNHAS NAO INCLUIDAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0125ROTEIRO INCOMPLETO  DEMONSTR. OPERACAO NAO INCLUIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0126VIGENCIA INICIAL DO SUBPRODUTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0127VIGENCIA FINAL DO SUBPRODUTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0128testes128
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0129DADO(S) DE ENTRADA INVALIDO(S)
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0130DADOS INCLUIDOS COM SUCESSO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0131DADOS ALTERADOS COM SUCESSO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0132DADOS EXCLUIDOS COM SUCESSO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0133SELECIONE UMA OPCAO DE PRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0134NAO EXISTEM TIPOS DE DESCONTO CADASTRADOS PARA O PRODUTO
      -    ' SELECIONADO           '.
               10  FILLER    PIC X(83) VALUE
           '0135PRODUTO NAO POSSUI ROTEIRO DISPONIVEL PARA INCLUSAO DE S
      -    'UBPRODUTO              '.
               10  FILLER    PIC X(83) VALUE
           '0136O CAMPO DENOMINACAO COMPLETA E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0137VALOR INFORMADO EM DENOMINACAO COMPLETA EXCEDEU O LIMITE
      -    ' DE 40 CARACTERES      '.
               10  FILLER    PIC X(83) VALUE
           '0138O CAMPO DENOMINACAO RESUMIDA E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0139VALOR INFORMADO EM DENOMINCAO RESUMIDA EXCEDEU O LIMITE
      -    'DE 10 CARACTERES       '.
               10  FILLER    PIC X(83) VALUE
           '0140SELECIONE UMA OPCAO DE TIPO DE DESCONTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0141O CAMPO SIGLA DO SUBPRODUTO E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0142VALOR INFORMADO EM SIGLA DO SUBPRODUTO EXCEDEU O LIMITE
      -    'DE 6 CARACTERES        '.
               10  FILLER    PIC X(83) VALUE
           '0143O CAMPO VIGENCIA DE E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0144A DATA INFORMADA E INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0145A DATA INICIO DA VIGENCIA DEVE SER MAIOR QUE A DATA ATUA
      -    'L                      '.
               10  FILLER    PIC X(83) VALUE
           '0146O CAMPO VIGENCIA ATE E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0147A DATA DE VIGENCIA ATE NAO PODE SER INFERIOR A DATA DE V
      -    'IGENCIA DE             '.
               10  FILLER    PIC X(83) VALUE
           '0148O CAMPO DESCRICAO DO SUBPRODUTO E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0149VALOR INFORMADA EM DESCRICAO SUBPRODUTO EXCEDEU O LIMITE
      -    ' DE 240 CARACTERES     '.
               10  FILLER    PIC X(83) VALUE
           '0150NAO EXISTEM SUBPRODUTOS CADASTRADOS PARA O PRODUTO SELEC
      -    'IONADO                 '.
               10  FILLER    PIC X(83) VALUE
           '0151SELECIONE UMA OPCAO DE SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0152SUBPRODUTO SELECIONADO INVALIDO PARA INCLUSAO COM BASE E
      -    'M                      '.
               10  FILLER    PIC X(83) VALUE
           '0153PERIODO DE VIGENCIA DO SUBPRODUTO ENCERRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0154A DATA FINAL DA VIGENCIA DEVE SER MAIOR OU IGUAL A DATA
      -    'ATUAL                  '.
               10  FILLER    PIC X(83) VALUE
           '0155DATA ALTERADA, SUBPRODUTO VALIDO ATE FINAL DO DIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0156SUBPRODUTO SELECIONADO JA POSSUI DADOS GERAIS CADASTRADO
      -    'S                      '.
               10  FILLER    PIC X(83) VALUE
           '0157SUBPRODUTO SELECIONADO NAO POSSUI DADOS GERAIS CADASTRAD
      -    'OS                     '.
               10  FILLER    PIC X(83) VALUE
           '0158SELECIONE UMA OPCAO DE TIPO DE PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0159SELECIONE UMA OPCAO DE PERMITE CONTRATO LIMITE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0160SELECIONE ENTRE SIM OU NAO PARA OBRIGATORIO EM PERMITE C
      -    'ONTRATO LIMITE         '.
               10  FILLER    PIC X(83) VALUE
           '0161SELECIONE UMA OPCAO DE PERMITE CONVENIO GERAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0162SELECIONE ENTRE SIM OU NAO PARA OBRIGATORIO EM PERMITE C
      -    'ONVENIO GERAL          '.
               10  FILLER    PIC X(83) VALUE
           '0163SELECIONE UMA OPCAO DE PERMITE CONVENIO CLIENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0164SELECIONE ENTRE SIM OU NAO PARA OBRIGATORIO EM PERMITE C
      -    'ONVENIO CLIENTE        '.
               10  FILLER    PIC X(83) VALUE
           '0165SELECIONE UMA OPCAO DE PERMITE ADITAMENTO DE PARCELA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0166SELECIONE UMA OPCAO DE PERMITE PRORROGACAO DE PARCELA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0167OBRIGATORIO INFORMAR VALOR OU COD VALOR(NTAX) EM TARIFA
      -    'PRORROGACAO PARCELA    '.
               10  FILLER    PIC X(83) VALUE
           '0168O VALOR INFORMADO E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0169VALOR INFORMADO EM VALOR EXCEDEU O LIMITE DE 16 CARACTER
      -    'ES                     '.
               10  FILLER    PIC X(83) VALUE
           '0170COD VALOR(NTAX) INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0171COD VALOR(NTAX) INFORMADO E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0172VALOR INFORMADO EM COD VALOR(NTAX) EXCEDEU O LIMITE DE 5
      -    ' CARACTERES            '.
               10  FILLER    PIC X(83) VALUE
           '0173SELECIONE UMA OPCAO DE PERMITE BAIXA ANTECIPADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0174SELECIONE UMA OPCAO DE BAIXA RESTRITA AO GESTOR
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0175SELECIONE UMA OPCAO DE PERMITE BAIXA PARCIAL PARCELA VEN
      -    'CIDA                   '.
               10  FILLER    PIC X(83) VALUE
           '0176SELECIONE UMA OPCAO DE PERMITE BAIXA POR RATEIO PARCELA
      -    'VENCIDA                '.
               10  FILLER    PIC X(83) VALUE
           '0177SELECIONE UMA OPCAO DE OBRIGATORIEDADE DA ANALISE DO SAC
      -    'ADO/EMITENTE           '.
               10  FILLER    PIC X(83) VALUE
           '0178SELECIONE UMA OPCAO DE OBRIGATORIEDADE DE CONSULTA AO CO
      -    'NE                     '.
               10  FILLER    PIC X(83) VALUE
           '0179SELECIONE UMA OPCAO DE RESPONSAVEL PELO LIMITE DE CREDIT
      -    'O                      '.
               10  FILLER    PIC X(83) VALUE
           '0180SUBPRODUTO SELECIONADO JA POSSUI DADOS OPERACIONAIS CADA
      -    'STRADOS                '.
               10  FILLER    PIC X(83) VALUE
           '0181SELECIONE UMA OPCAO DE TIPO DE JUROS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0182SELECIONE UMA OPCAO DE PAGADOR DE JUROS / DESPESAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0183SELECIONE UMA OPCAO DE EXIBE BANDA DE REDE NA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0184SELECIONE UMA OPCAO DE INDICE IOF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0185SELECIONE UMA OPCAO DE PERMITE DESCONTO AO SACADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0186SELECIONE UMA OPCAO DE PERMITE AGENCIA NEGOCIADORA DIFER
      -    'ENTE DA OPERADORA      '.
               10  FILLER    PIC X(83) VALUE
           '0187OBRIGATORIO PREENCHIMENTO DO CAMPO MINIMO EM VALOR DA OP
      -    'ERACAO DE DESCONTO     '.
               10  FILLER    PIC X(83) VALUE
           '0188VALOR MINIMO INFORMADO E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0189VALOR INFORMADO EM MINIMO EXCEDEU O LIMITE DE 16 CARACTE
      -    'RES                    '.
               10  FILLER    PIC X(83) VALUE
           '0190OBRIGATORIO PREENCHIMENTO DO CAMPO MAXIMO EM VALOR DA OP
      -    'ERACAO DE DESCONTO     '.
               10  FILLER    PIC X(83) VALUE
           '0191VALOR MAXIMO INFORMADO E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0192VALOR INFORMADO EM MAXIMO EXCEDEU O LIMITE DE 16 CARACTE
      -    'RES                    '.
               10  FILLER    PIC X(83) VALUE
           '0193MAXIMO NAO PODE SER INFERIOR AO MINIMO EM VALOR DA OPERA
      -    'CAO DE DESCONTO        '.
               10  FILLER    PIC X(83) VALUE
           '0194SELECIONE UMA OPCAO DE REGRA DE MORA LEGAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0195SELECIONE UMA OPCAO DE EM MORA EM REGRA DE MORA ESPECIAL
      -    ' (GERENCIAL)           '.
               10  FILLER    PIC X(83) VALUE
           '0196SELECIONE UMA OPCAO DE EM CA EM REGRA DE MORA ESPECIAL (
      -    'GERENCIAL)             '.
               10  FILLER    PIC X(83) VALUE
           '0197SUBPRODUTO SELECIONADO NAO POSSUI DADOS OPERACIONAIS CAD
      -    'ASTRADOS               '.
               10  FILLER    PIC X(83) VALUE
           '0198NAO EXISTEM RESGISTROS DISPONIVEIS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0199TABELA DE MEIO DE LIBERACAO VAZIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0200SELECIONE PELO MENOS UMA OPCAO DE MEIO DE LIBERACAO DA O
      -    'PERACAO                '.
               10  FILLER    PIC X(83) VALUE
           '0201NAO EXISTE MEIO DE PAGAMENTO PARA O SUBPRODUTO SELECIONA
      -    'DO                     '.
               10  FILLER    PIC X(83) VALUE
           '0202SUBPRODUTO SELECIONADO JA POSSUI MEIOS DE PAGAMENTO CADA
      -    'STRADOS                '.
               10  FILLER    PIC X(83) VALUE
           '0203SELECIONE UMA OPCAO DE MEIO DE PAGAMENTO - ATE VENCIMENT
      -    'O                      '.
               10  FILLER    PIC X(83) VALUE
           '0204SELECIONE CEDE./PROP. OU SAC./EMIT. P/ RESP. EM MEIO DE
      -    'PAGAM. ATE VENCIMENTO  '.
               10  FILLER    PIC X(83) VALUE
           '0205SELECIONE UMA OPCAO DE MEIO DE PAGAMENTO - APOS VENCIMEN
      -    'TO                     '.
               10  FILLER    PIC X(83) VALUE
           '0206SELECIONE CEDE./PROP. OU SAC./EMIT. P/ RESP. EM MEIO DE
      -    'PAGAM. APOS VENCIMENTO '.
               10  FILLER    PIC X(83) VALUE
           '0207TOTAL DE DIAS INFORMADO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0208VALOR INFORMADO EM DIAS EXCEDEU O LIMITE DE 3 CARACTERES
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0209SELECIONE ENTRE CORRIDOS OU UTEIS EM PRAZO PARA DEBITOS
      -    'DOS VENCIDOS           '.
               10  FILLER    PIC X(83) VALUE
           '0210SUBPRODUTO SELECIONADO NAO POSSUI MEIOS DE PAGAMENTOS CA
      -    'DASTRADOS              '.
               10  FILLER    PIC X(83) VALUE
           '0211TABELA DE AVISOS AO CLIENTE VAZIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0212ERRO AO ACESSAR OS DADOS DE SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0213ROTEIRO DE INCLUSAO NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0214ERRO AO ACESSAR OS DADOS DE SUBPRODUTO REGRA OPERACIONAL
      -     '                      '.
               10  FILLER    PIC X(83) VALUE
           '0215ERRO AO GRAVAR OS DADOS NO LOG
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0216ROTEIRO DE INCLUSAO DE SUBPRODUTO NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0217SELECIONE PELO MENOS UMA OPCAO DE TIPO DE AVISO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0218SELECIONE UMA OPCAO DE PERIODICIDADE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0219SELECIONE PELO MENOS UMA OPCAO DE AVISO AO CLIENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0220NUMERO DO BANCO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0221NUMERO DA AGENCIA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0222NUMERO DA CONTA-CORRENTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0223CNPJ/CPF INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0224NOME DO CLIENTE EM BRANCO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0225NUMERO DO CONTRATO/CONVENIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0226NUMERO DA VERSAO DO CONTRATO/CONVENIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0227NUMERO DA SIMULACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0228VALOR DA OPERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0229DATA DE INICIO DA OPERACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0230DATA DE VENCIMENTO FINAL DA OPERACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0231QUANTIDADE DE TITULOS INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0232CODIGO DO MEIO DE ENTRADA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0233ANO DA CONSULTA DE CREDITO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0234CODIGO DA DEPENDENCIA DA CONSULTA DE CREDITO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0235NUMERO DA CONSULTA DE CREDITO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0236ERRO AO ACESSAR OS DADOS DE SUBPRODUTO MEIOS DE PAGAMENT
      -    'O                      '.
               10  FILLER    PIC X(83) VALUE
           '0237SELECIONE UMA OPCAO DE FATO GERADOR CONTABIL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0238SUBPRODUTO SELECIONADO NAO POSSUI MEIOS DE ENTRADA CADAS
      -    'TRADOS                 '.
               10  FILLER    PIC X(83) VALUE
           '0239SELECIONE UMA OPCAO DE MEIO DE ENTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0240SELECIONE UMA OPCAO DE SITUACAO DA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0241O CAMPO TRILHA CONTABIL E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0242CODIGO DA TRILHA CONTABIL INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0243VALOR INFORMADO EM TRILHA CONTABIL EXCEDEU O LIMITE DE 7
      -    ' CARACTERES            '.
               10  FILLER    PIC X(83) VALUE
           '0244OPERACAO NAO PERMITE ESTORNO, LIBERACAO EFETUADA ATRAVES
      -    ' DE DOC                '.
               10  FILLER    PIC X(83) VALUE
           '0245OPERACAO NAO PERMITE ESTORNO, LIBERACAO EFETUADA ATRAVES
      -    ' DE TED                '.
               10  FILLER    PIC X(83) VALUE
           '0246SUBPRODUTO JA APROVADO, NAO PERMITE EXCLUSAO DE TODOS OS
      -    ' MEIOS DE LIBERACAO    '.
               10  FILLER    PIC X(83) VALUE
           '0247A DATA FINAL DA VIGENCIA DEVE SER MAIOR QUE A DATA ATUAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0248TRILHA CONTABIL JA CADASTRADA PARA OS PARAMETROS SELECIO
      -    'NADOS                  '.
               10  FILLER    PIC X(83) VALUE
           '0249ERRO AO ACESSAR OS DADOS DE SUBPRODUTO MEIOS DE LIBERACA
      -    'O                      '.
               10  FILLER    PIC X(83) VALUE
           '0250O CAMPO NOVA TRILHA CONTABIL E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0251VALOR INFORMADO EM NOVA TRILHA CONTABIL EXCEDEU O LIMITE
      -    ' DE 7 CARACTERES       '.
               10  FILLER    PIC X(83) VALUE
           '0252NAO EXISTEM MEIOS DE ENTRADA CADASTRADOS PARA O TIPO DE
      -    'DESCONTO DO SUBPRODUTO '.
               10  FILLER    PIC X(83) VALUE
           '0253SELECIONE PELO MENOS UMA OPCAO DE MEIO DE ENTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0254SELECIONE UMA OPCAO DE PERMITE LIBERACAO ANTECIPADA DA O
      -    'PERACAO                '.
               10  FILLER    PIC X(83) VALUE
           '0255O CAMPO PERCENTUAL MAXIMO DO VALOR DA OPERACAO E OBRIGAT
      -    'ORIO                   '.
               10  FILLER    PIC X(83) VALUE
           '0256PERCENTUAL MAXIMO DO VALOR DA OPERACAO INFORMADO E INVAL
      -    'IDO                    '.
               10  FILLER    PIC X(83) VALUE
           '0257VALOR EM PERCENTUAL MAXIMO DO VALOR DA OPERACAO EXCEDEU
      -    'LIMITE DE 5 CARACTERES '.
               10  FILLER    PIC X(83) VALUE
           '0258OBRIGATORIO PREENCHIMENTO DO CAMPO MINIMO EM PRAZO DE VE
      -    'NCIMENTO DE TITULOS    '.
               10  FILLER    PIC X(83) VALUE
           '0259VALOR MINIMO DE VENCIMENTO DE TITULOS NAO PERMITIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0260VALOR MINIMO EM PRAZO DE VENCIMENTO DE TITULOS EXCEDEU L
      -    'IMITE DE 5 CARACTERES  '.
               10  FILLER    PIC X(83) VALUE
           '0261ERRO AO ACESSAR OS DADOS DE SUBPRODUTO AVISO CLIENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0262OBRIGATORIO PREENCHIMENTO DO CAMPO MAXIMO EM PRAZO DE VE
      -    'NCIMENTO DE TITULOS    '.
               10  FILLER    PIC X(83) VALUE
           '0263VALOR MAXIMO DE VENCIMENTO DE TITULOS NAO PERMITIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0264VALOR MAXIMO EM PRAZO DE VENCIMENTO DE TITULOS EXCEDEU L
      -    'IMITE DE 5 CARACTERES  '.
               10  FILLER    PIC X(83) VALUE
           '0265SUBPRODUTO JA APROVADO, NAO PERMITE EXCLUSAO DE TODOS OS
      -    ' AVISOS AO CLIENTE     '.
               10  FILLER    PIC X(83) VALUE
           '0266MAXIMO NAO PODE SER INFERIOR AO MINIMO EM PRAZO DE VENCI
      -    'MENTO DE TITULOS       '.
               10  FILLER    PIC X(83) VALUE
           '0267SELECIONE ENTRE DIAS UTEIS OU DIAS CORRIDOS EM PRAZO DE
      -    'VENCIMENTO DE TITULOS  '.
               10  FILLER    PIC X(83) VALUE
           '0268OBRIGATORIO PREENCHIMENTO DO CAMPO MAXIMO EM PRAZO DE AC
      -    'OLHIMENTO DE TITULOS   '.
               10  FILLER    PIC X(83) VALUE
           '0269VALOR MAXIMO DE PRAZO DE ACOLHIMENTO DE TITULOS E INVALI
      -    'DO                     '.
               10  FILLER    PIC X(83) VALUE
           '0270VALOR MAXIMO EM PRAZO ACOLHIMENTO DE TITULOS EXCEDEU O L
      -    'IMITE DE 3 CARACTERES  '.
               10  FILLER    PIC X(83) VALUE
           '0271SELECIONE ENTRE DIAS UTEIS OU DIAS CORRIDOS EM PRAZO DE
      -    'ACOLHIMENTO DE TITULOS '.
               10  FILLER    PIC X(83) VALUE
           '0272OBRIGATORIO PREENCHIMENTO CAMPO ATE EM VALOR LIMITE DIV
      -    'ERG. OPERACAO X TITULO '.
               10  FILLER    PIC X(83) VALUE
           '0273VALOR ATE INFORMADO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0274VALOR INFORMADO PARA O CAMPO ATE EXCEDEU O LIMITE DE 16
      -    'CARACTERES             '.
               10  FILLER    PIC X(83) VALUE
           '0275SELECIONE UMA OPCAO DE OBRIGATORIEDADE DE GARANTIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0276SELECIONE UMA OPCAO DE OBRIGATORIESADE EMISSAO DE NOTA P
      -    'ROMISSORIA             '.
               10  FILLER    PIC X(83) VALUE
           '0277SELECIONE UMA OPCAO DE FORMALIZACAO DA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0278SELECIONE UMA OPCAO DE TIPO DA FORMALIZACAO DA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0279O CAMPO COD CARTEIRA REGISTRO DESCONTO NO DESTINO E OBRI
      -    'GATORIO                '.
               10  FILLER    PIC X(83) VALUE
           '0280COD CARTEIRA REGISTRO DESCONTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0281VALOR INFORMADO COD CARTEIRA REGISTRO DESCONTO EXCEDEU L
      -    'IMITE DE 4 CARACTERES  '.
               10  FILLER    PIC X(83) VALUE
           '0282COD CARTEIRA REGISTRO DESCONTO INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0283O CAMPO COD CARTEIRA NORMAL E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0284COD CARTEIRA NORMAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0285VALOR INFORMADO COD CARTEIRA NORMAL EXCEDEU LIMITE DE 4
      -    'CARACTERES             '.
               10  FILLER    PIC X(83) VALUE
           '0286COD CARTEIRA NORMAL INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0287O CAMPO COD CARTEIRA VENCIDA E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0288COD CARTEIRA VENCIDA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0289VALOR INFORMADO COD CARTEIRA VENCIDA EXCEDEU LIMITE DE 4
      -    ' CARACTERES            '.
               10  FILLER    PIC X(83) VALUE
           '0290COD CARTEIRA VENCIDA INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0291SELECIONE UMA OPCAO DE TAC DA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0292DESEJA EXCLUIR ESTE REGISTRO?
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0293TAMANHO INCORRETO CPF/CNPJ
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0294TAXA SEM ENQUADRAMENTO, UTILIZADA TAXA DO PRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0295BASE INDISPONIVEL, UTILIZADA TAXA DO PRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0296MEIO DE ENTRADA NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0297CONTRATO LIMITE NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0298REGRAS OPERACIONAIS NAO CADASTRADAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0299TAXA INVALIDA PARA A OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0300GARANTIA OBRIGATORIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0301OBRIGATORIO AUTORIZACAO DA TESOURARIA PARA A TAXA APLICA
      -    'DA                     '.
               10  FILLER    PIC X(83) VALUE
           '0302OBRIGATORIO INFORMAR VALOR OU COD VALOR(NTAX) EM TAC DA
      -    'OPERACAO               '.
               10  FILLER    PIC X(83) VALUE
           '0303VALOR INFORMADO EM TAC DA OPERACAO E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0304O VALOR INFORMADO PARA TAC OPERACAO EXCEDEU O LIMITE DE
      -    '16 CARACTERES          '.
               10  FILLER    PIC X(83) VALUE
           '0305COD VALOR(NTAX) INFORMADO EM TAC DA OPERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0306COD VALOR(NTAX) EM TAC DA OPERACAO EXCEDEU O LIMITE DE 5
      -    ' CARACTERES            '.
               10  FILLER    PIC X(83) VALUE
           '0307CODIGO VALOR (NTAX) EM TAC DA OPERACAO INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0308OBRIGATORIO PREENCHIMENTO DO CAMPO % VALOR OPERACAO(NTAX
      -    ') EM TAC DA OPERACAO   '.
               10  FILLER    PIC X(83) VALUE
           '0309% VALOR DA OPERACAO (NTAX) INFORMADO EM TAC DA OPERACAO
      -    'INVALIDO               '.
               10  FILLER    PIC X(83) VALUE
           '0310% VALOR OPERACAO (NTAX) EM TAC DA OPERACAO EXCEDEU O LIM
      -    'ITE DE 5 CARACTERES    '.
               10  FILLER    PIC X(83) VALUE
           '0311% VALOR OPERACAO (NTAX) EM TAC DA OPERACAO INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0312OBRIGATORIO PREENCHIMENTO DO CAMPO COD VALOR (NTAX) MINI
      -    'MO EM TAC DA OPERACAO  '.
               10  FILLER    PIC X(83) VALUE
           '0313COD VALOR (NTAX) MINIMO INFORMADO EM TAC DA OPERACAO INV
      -    'ALIDO                  '.
               10  FILLER    PIC X(83) VALUE
           '0314COD VALOR (NTAX) MINIMO EM TAC DA OPERACAO EXCEDEU O LIM
      -    'ITE DE 5 CARACTERES    '.
               10  FILLER    PIC X(83) VALUE
           '0315COD VALOR (NTAX) MINIMO EM TAC DA OPERACAO INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0316OBRIGATORIO PREENCHIMENTO DO CAMPO COD VALOR (NTAX) MAXI
      -    'MO EM TAC DA OPERACAO  '.
               10  FILLER    PIC X(83) VALUE
           '0317COD VALOR (NTAX) MAXIMO INFORMADO EM TAC DA OPERACAO INV
      -    'ALIDO                  '.
               10  FILLER    PIC X(83) VALUE
           '0318COD VALOR (NTAX) MAXIMO EM TAC DA OPERACAO EXCEDEU O LIM
      -    'ITE DE 5 CARACTERES    '.
               10  FILLER    PIC X(83) VALUE
           '0319VALOR(NTAX)MAX NAO PODE SER INFERIOR VALOR(NTAX)MIN EM T
      -    'AC DA OPERACAO         '.
               10  FILLER    PIC X(83) VALUE
           '0320COD VALOR (NTAX) MAXIMO EM TAC DA OPERACAO INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0321SELECIONE UMA OPCAO DE TAC DO CONTRATO LIMITE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0322OBRIGATORIO INFORMAR VALOR OU COD VALOR(NTAX) EM TAC DO
      -    'CONTRATO LIMITE        '.
               10  FILLER    PIC X(83) VALUE
           '0323VALOR INFORMADO EM TAC DO CONTRATO LIMITE E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0324O VALOR INFORMADO PARA TAC CONTRATO LIMITE EXCEDEU O LIM
      -    'ITE DE 16 CARACTERES   '.
               10  FILLER    PIC X(83) VALUE
           '0325COD VALOR(NTAX) INFORMADO EM TAC DO CONTRATO LIMITE INVA
      -    'LIDO                   '.
               10  FILLER    PIC X(83) VALUE
           '0326COD VALOR(NTAX) EM TAC DO CONTRATO LIMITE EXCEDEU O LIMI
      -    'TE DE 5 CARACTERES     '.
               10  FILLER    PIC X(83) VALUE
           '0327CODIGO VALOR (NTAX) EM TAC DO CONTRATO LIMITE INEXISTENT
      -    'E                      '.
               10  FILLER    PIC X(83) VALUE
           '0328OBRIGATORIO PREENCHIMENTO CAMPO % VALOR OPERACAO(NTAX) E
      -    'M TAC CONTRATO LIMITE  '.
               10  FILLER    PIC X(83) VALUE
           '0329% VALOR DA OPERACAO (NTAX) INFORMADO EM TAC DO CONTRATO
      -    'LIMITE INVALIDO        '.
               10  FILLER    PIC X(83) VALUE
           '0330% VALOR OPERACAO(NTAX) EM TAC DO CONTRATO LIMITE EXCEDEU
      -    ' O LIMITE 5 CARACTERES '.
               10  FILLER    PIC X(83) VALUE
           '0331% VALOR OPERACAO (NTAX) EM TAC DO CONTRATO LIMITE INEXIS
      -    'TENTE                  '.
               10  FILLER    PIC X(83) VALUE
           '0332OBRIGATORIO PREENCHIMENTO CAMPO COD VALOR (NTAX) MINIMO
      -    'EM TAC CONTRATO LIMITE '.
               10  FILLER    PIC X(83) VALUE
           '0333COD VALOR (NTAX) MINIMO INFORMADO EM TAC DO CONTRATO LIM
      -    'ITE INVALIDO           '.
               10  FILLER    PIC X(83) VALUE
           '0334COD VALOR(NTAX) MINIMO EM TAC DO CONTRATO LIMITE EXCEDEU
      -    ' LIMITE DE 5 CARACTERES'.
               10  FILLER    PIC X(83) VALUE
           '0335COD VALOR (NTAX) MINIMO EM TAC DO CONTRATO LIMITE INEXIS
      -    'TENTE                  '.
               10  FILLER    PIC X(83) VALUE
           '0336OBRIGATORIO PREENCHIMENTO CAMPO COD VALOR (NTAX) MAXIMO
      -    'EM TAC CONTRATO LIMITE '.
               10  FILLER    PIC X(83) VALUE
           '0337COD VALOR (NTAX) MAXIMO INFORMADO EM TAC DO CONTRATO LIM
      -    'ITE INVALIDO           '.
               10  FILLER    PIC X(83) VALUE
           '0338COD VALOR(NTAX) MAXIMO EM TAC DO CONTRATO LIMITE EXCEDEU
      -    ' LIMITE DE 5 CARACTERES'.
               10  FILLER    PIC X(83) VALUE
           '0339COD VALOR(NTAX)MAX NAO PODE SER INFERIOR COD VALOR(NTAX)
      -    'MIN TAC CONTRATO LIMITE'.
               10  FILLER    PIC X(83) VALUE
           '0340COD VALOR (NTAX) MAXIMO EM TAC DO CONTRATO LIMITE INEXIS
      -    'TENTE                  '.
               10  FILLER    PIC X(83) VALUE
           '0341SELECIONE UMA OPCAO DE PERMITE COBRANCA DE TARIFA NA TRA
      -    'NSFERENCIA             '.
               10  FILLER    PIC X(83) VALUE
           '0342OBRIGATORIO INFORMAR VALOR OU COD VALOR(NTAX) EM TARIFA
      -    'VALOR TRANSFERENCIA    '.
               10  FILLER    PIC X(83) VALUE
           '0343VALOR INFORMADO EM TARIFA DE VALOR NA TRANFERENCIA E INV
      -    'ALIDO                  '.
               10  FILLER    PIC X(83) VALUE
           '0344VALOR INFORMADO EM TARIFA VALOR NA TRANSFERENCIA EXCEDEU
      -    'LIMITE DE 16 CARACTERES'.
               10  FILLER    PIC X(83) VALUE
           '0345COD VALOR(NTAX) INFORMADO EM TARIFA DE VALOR NA TRANSFER
      -    'ENCIA INVALIDO         '.
               10  FILLER    PIC X(83) VALUE
           '0346COD VALOR(NTAX) EM TARIFA VALOR NA TRANSFERENCIA EXCEDEU
      -    'LIMITE DE 5 CARACTERES '.
               10  FILLER    PIC X(83) VALUE
           '0347CODIGO VALOR (NTAX) EM TARIFA DE VALOR NA TRANSFERENCIA
      -    'INEXISTENTE            '.
               10  FILLER    PIC X(83) VALUE
           '0348SELECIONE UMA OPCAO DE TIPO DE CONTA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0349O CAMPO PRIMEIRA RAZAO E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0350VALOR INFORMADO PARA PRIMEIRA RAZAO E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0351O VALOR INFORMADO PARA PRIMEIRA RAZAO EXCEDEU O LIMITE D
      -    'E 3 CARACTERES         '.
               10  FILLER    PIC X(83) VALUE
           '0352O CAMPO SEGUNDA RAZAO E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0353VALOR INFORMADO PARA SEGUNDA RAZAO E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0354O VALOR INFORMADO PARA SEGUNDA RAZAO EXCEDEU O LIMITE DE
      -    ' 3 CARACTERES          '.
               10  FILLER    PIC X(83) VALUE
           '0355O CAMPO CONTA E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0356VALOR INFORMADO PARA CONTA E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0357O VALOR INFORMADO PARA CONTA EXCEDEU O LIMITE DE 7 CARAC
      -    'TERES                  '.
               10  FILLER    PIC X(83) VALUE
           '0358MEIO DE ENTRADA NAO POSSUI REGISTROS DISPONIVEIS PARA AL
      -    'TERACAO                '.
               10  FILLER    PIC X(83) VALUE
           '0359MEIO DE ENTRADA NAO POSSUI REGISTROS DISPONIVEIS PARA EX
      -    'CLUSAO                 '.
               10  FILLER    PIC X(83) VALUE
           '0360SUBPRODUTO JA APROVADO, NAO PERMITE EXCLUSAO DE TODOS OS
      -    ' MEIOS DE ENTRADA      '.
               10  FILLER    PIC X(83) VALUE
           '0361DADOS INVALIDOS PARA TRILHA CONTABIL SELECIONADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0362MEIO DE ENTRADA NAO POSSUI REGISTROS DISPONIVEIS PARA CO
      -    'NSULTA                 '.
               10  FILLER    PIC X(83) VALUE
           '0363STATUS DO 1UBPRODUTO NAO POSSUI ROTEIRO DE CADASTRAMENTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0364PRODUTO NAO POSSUI ROTEIRO DISPONIVEL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0365SELECIONE PELO MENOS UMA OPCAO DE AGRUPAMENTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0366SELECIONE UMA OPCAO DE TIPO DE TAXA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0367O CAMPO INICIO DA VIGENCIA E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0368VIGENCIA INICIAL DEVE SER MAIOR/IGUAL VIGENCIA INICIAL S
      -    'UBPRODUTO SELECIONADO  '.
               10  FILLER    PIC X(83) VALUE
           '0369JA EXISTE TAXA CADASTRADA PARA O INICIO DE VIGENCIA INFO
      -    'RMADO                  '.
               10  FILLER    PIC X(83) VALUE
           '0370SUBPRODUTO JA POSSUI CADASTRO VIGENTE PARA TIPO DE TAXA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0371SUBPRODUTO JA APROVADO, NAO PERMITE EXCLUSAO DE TODOS OS
      -    ' TIPOS DE TAXA         '.
               10  FILLER    PIC X(83) VALUE
           '0372TIPO DE TAXA POSSUI CRITERIO/BANDA CADASTRADO, NAO PODER
      -    ' SER EXCLUIDO          '.
               10  FILLER    PIC X(83) VALUE
           '0373SELECIONE PELO MENOS UMA OPCAO DE CARTEIRA SELECAO ORIGE
      -    'M                      '.
               10  FILLER    PIC X(83) VALUE
           '0374SELECIONE UMA OPCAO DE SEGMENTO DO CLIENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0375SELECIONE UMA OPCAO DE RATING
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0376VIGENCIA INICIAL DEVE SER MAIOR/IGUAL VIGENCIA INICIAL T
      -    'IPO DE TAXA SELECIONADO'.
               10  FILLER    PIC X(83) VALUE
           '0377VIGENCIA INICIAL DEVE SER MENOR/IGUAL VIGENCIA FINAL SUB
      -    'PRODUTO SELECIONADO    '.
               10  FILLER    PIC X(83) VALUE
           '0378VIGENCIA INICIAL DEVE SER MENOR/IGUAL VIGENCIA FINAL TIP
      -    'O DE TAXA SELECIONADO  '.
               10  FILLER    PIC X(83) VALUE
           '0379OBRIGATORIO PREENCHIMENTO DO CAMPO TAXA MAXIMA (NTAX)
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0380TAXA MAXIMA (NTAX) INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0381VALOR INFORMADO EM TAXA MAXIMA(NTAX) EXCEDEU O LIMITE DE
      -    ' 5 CARACTERES          '.
               10  FILLER    PIC X(83) VALUE
           '0382TAXA MAXIMA(NTAX) INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0383O CAMPO TAXA OU COD VALOR (NTAX) E OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0384O VALOR INFORMADO EM TAXA E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0385O VALOR INFORMADO EM TAXA EXCEDEU O LIMITE DE 6 CARACTER
      -    'ES                     '.
               10  FILLER    PIC X(83) VALUE
           '0386SELECIONE UMA OPCAO DE FREQUENCIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0387PARAMETROS SELECIONADOS JA POSSUEM CRITERIO DE TAXA CADA
      -    'STRADO                 '.
               10  FILLER    PIC X(83) VALUE
           '0388CRITERIO DE TAXA POSSUI BANDA CADASTRADA, NAO PODE SER E
      -    'XCLUIDO                '.
               10  FILLER    PIC X(83) VALUE
           '0389A DATA INICIO DA VIGENCIA DEVE SER MAIOR OU IGUAL A DATA
      -    ' ATUAL                 '.
               10  FILLER    PIC X(83) VALUE
           '0390CRITERIO DE TAXA POSSUI TAXA MINIMA P/ BANDA MAIOR QUE A
      -    ' TAXA MAXIMA INFORMADA '.
               10  FILLER    PIC X(83) VALUE
           '0391SELECIONE UMA OPCAO DE SUBPRODUTO OU UMA OPCAO DE SITUAC
      -    'AO                     '.
               10  FILLER    PIC X(83) VALUE
           '0392GARANTIA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0393CARTEIRA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0394ANO BASE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0395DEPDC INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0396CSEQ-STUDO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0397TAXA DE JUROS AO MES INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0398TAXA DE JUROS AO ANO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0399VALOR DO JUROS INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0400CODIGO DO AUTORIZANTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0401NOME DO AUTORIZANTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0402FLAG DO AVALISTA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0403DOCUMENTO DE IDENTIFICACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0404LOGRADOURO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0405CEP INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0406COMPLEMENTO DO CEP INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0407BAIRRO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0408MUNICIPIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0409UF INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0410INDICADOR DE CONJUGE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0411NOME DO CONJUGE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0412CPF DO CONJUGE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0413DOCUMENTO DE IDENTIFICACAO DO CONJUGE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0414CCLUB INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0415TIPO DE DESCONTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0416NUMERO DA PARCELA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0417SITUACAO DA PARCELA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0418NUMERO DO TITULO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0419SEU NUMERO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0420ESPECIE INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0421DATA EMISSAO DO TITULO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0422VENCIMENTO DO TITULO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0423VALOR DO TITULO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0424INDICADOR DE INSTRUCAO DE DESCONTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0425SEQUENCIA DA PARCELA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0426DATA DA INSTRUCAO DE DESCONTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0427VALOR DA INSTRUCAO DE DESCONTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0428PERCENTUAL DA INSTRUCAO DE DESCONTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0429INDICADOR DE SACADOR AVALISTA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0430CNPJ/CPF SACADOR AVALISTA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0431NOME DO SACADOR AVALISTA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0432TIPO DE CONVENIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0433CONVENIO GERAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0434CONTRATO LIMITE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0435INDICADOR DE ATUALIZACAO DO SACADO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0436BANDEIRA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0437ROTATIVO PARCELADO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0438VALOR MAXIMO DA OPERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0439VALOR MINIMO RECEBIVEL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0440VALOR MAXIMO RECEBIVEL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0441DATA VENCIMENTO MINIMO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0442DATA VENCIMENTO MAXIMA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0443PRAZO MINIMO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0444PRAZO MAXIMO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0445QUANTIDADE DE OCORRENCIAS PREENCHIDAS INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0446INDICADOR DE SELECAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0447CNPJ/CPF FORNECEDOR INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0448CNPJ/CPF CONVENIADA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0449CLASSIFICACAO POR VALOR INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0450CLASSIFICACAO POR VENCIMENTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0451QUANTIDADE A DESPREZAR INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0452MODALIDADE DE PAGAMENTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0453NUMERO DO FORNECEDOR INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0454IDENTIFICADOR DE SEQUENCIA DE PAGAMENTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0455DOCUMENTO FISCAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0456DATA DE VENCIMENTO DOCUMENTO FICAL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0457VALOR DO DOCUMENTO FISCAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0458ORIGEM DO PAGAMENTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0459DATA DE EMISSAO DO DOCUMENTO FISCAL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0460CONDICAO DE PAGAMENTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0461VALOR TOTAL ANTECIPADO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0462AGENCIA DO CEDENTE INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0463CONTA DO CEDENTE INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0464CNPJ/CPF AVALISTA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0465VIGENCIA INICIAL DEVE SER MAIOR/IGUAL VIGENCIA INICIAL D
      -    'O CRITERIO SELECIONADO '.
               10  FILLER    PIC X(83) VALUE
           '0466VIGENCIA INICIAL DEVE SER MENOR/IGUAL VIGENCIA FINAL DO
      -    'CRITERIO SELECIONADO   '.
               10  FILLER    PIC X(83) VALUE
           '0467E OBRIGATORIO PREENCHIMENTO DO CAMPO MINIMO EM VALOR DA
      -    'OPERACAO               '.
               10  FILLER    PIC X(83) VALUE
           '0468E OBRIGATORIO PREENCHIMENTO DO CAMPO MAXIMO EM VALOR DA
      -    'OPERACAO               '.
               10  FILLER    PIC X(83) VALUE
           '0469MAXIMO NAO PODE SER INFERIOR AO MINIMO EM VALOR DA OPERA
      -    'CAO                    '.
               10  FILLER    PIC X(83) VALUE
           '0470VALOR MINIMO EM PRAZO MEDIO DA OPERACAO E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0471E OBRIGATORIO O PREENCHIMENTO DO CAMPO MINIMO EM PRAZO M
      -    'EDIO DA OPERACAO       '.
               10  FILLER    PIC X(83) VALUE
           '0472VALOR MINIMO EM PRAZO MEDIO DA OPERACAO EXCEDEU O LIMITE
      -    ' DE 5 CARACTERES       '.
               10  FILLER    PIC X(83) VALUE
           '0473E OBRIGATORIO O PREENCHIMENTO DO CAMPO MAXIMO EM PRAZO M
      -    'EDIO DA OPERACAO       '.
               10  FILLER    PIC X(83) VALUE
           '0474VALOR MAXIMO EM PRAZO MEDIO DA OPERACAO E INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0475VALOR MAXIMO EM PRAZO MEDIO DA OPERACAO EXCEDEU O LIMITE
      -    ' DE 5 CARACTERES       '.
               10  FILLER    PIC X(83) VALUE
           '0476MAXIMO NAO PODE SER INFERIOR AO MINIMO EM PRAZO DE MEDIO
      -    ' DA OPERACAO           '.
               10  FILLER    PIC X(83) VALUE
           '0477E OBRIGATORIO O PREENCHIMENTO DO CAMPO MINIMO EM TAXA'.
               10  FILLER    PIC X(83) VALUE
           '0478PARAMETROS SELECIONADOS JA POSSUEM BANDAS PARA TAXA DE R
      -    'EDE CADASTRADA         '.
               10  FILLER    PIC X(83) VALUE
           '0479TAXA MINIMA NAO PODE SER SUPERIOR A TAXA MAXIMA DE REDE
      -    '(NTAX)                 '.
               10  FILLER    PIC X(83) VALUE
           '0480SELECIONE PELO MENOS UMA OPCAO DE BANDA PARA TAXA DE RED
      -    'E                      '.
               10  FILLER    PIC X(83) VALUE
           '0481PARAMETROS INFORMADOS JA POSSUEM RAZAO / CONTA CADASTRAD
      -    'A                      '.
               10  FILLER    PIC X(83) VALUE
           '0482MEIO DE ENTRADA JA CADASTRADO NAO PERMITE EXCLUSAO DE TO
      -    'DAS AS CONCILIACOES    '.
               10  FILLER    PIC X(83) VALUE
           '0483OPERACAO / PARCELAS NAO CADASTRADOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0484TIPO DE BAIXA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0485VALOR DE BAIXA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0486TIPO DE DEBITO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0487INDICADOR DE BAIXA NA COBRANCA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0488INDICADOR DE CONSOLIDACAO NO EXTRATO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0489JUSTIFICATIVA DE BAIXA OBRIGATORIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0490CONTA PARA DEBITO IRREGULAR. BAIXA INVALIDADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0491OPERACAO / PARCELAS NAO ESTAO EM CONDICOES DE BAIXAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0492OPERACAO / PARCELAS JA VALIDADAS. EXCLUSAO DE MOVIMENTO
      -    'DE BAIXA INVALIDO      '.
               10  FILLER    PIC X(83) VALUE
           '0493OPERACAO / PARCELAS JA BAIXADAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0494PARCELA NAO INFORMADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0495TIPO DE ISENCAO DESCONTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0496BAIXA DE OPERACAO/PARCELA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0497TIPO DO CONVENIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0498SEQUENCIA DA CONSULTA DE CREDITO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
      -    '0499CARTEIRA DA CONSULTA DE CREDITO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0500GARANTIA DA CONSULTA DE CREDITO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0501CONSULTA CONE PENDENTE DE AUTORIZACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0502SUBPRODUTO POSSUI TIPO DE TAXA FUTURA CADASTRADA, NAO PE
      -    'RMITE INCLUSAO         '.
               10  FILLER    PIC X(83) VALUE
           '0503DATA DE VENCIMENTO DA PARCELA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0504DATA DE VENCIMENTO PRORROGACAO DA PARCELA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0505TAXA DE JUROS AO MES - PRORROGACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0506VALOR DOS JUROS DA PARCELA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0507VALOR DO IOF - PRORROGACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0508VALOR DA TARIFA DE PRORROGACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0509VALOR DOS ENCARGOS DA PARCELA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0510FLAG CADU INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0511CONFIRMACAO DE EXCLUSAO DE MOVIMENTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0512EXISTEM OUTROS MOVIMENTOS PARA ESSA PARCELA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0513CANCELAMENTO POR RECURSO INDISPONIVEL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0514VALOR DO PRINCIPAL DA PARCELA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0515NAO HA PRODUTOS VALIDOS PARA DESCONTO CADASTRADOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0516CODIGO DO PRODUTO VALIDO PARA DESCONTO NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0517DATA DE MOVIMENTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0518CODIGO NTAX(MIN) INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0519CODIGO NTAX(MAX) INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0520NAO EXISTEM DOCUMENTOS DE FORMALIZACAO CADASTRADOS PARA
      -    'O PRODUTO SELECIONADO  '.
               10  FILLER    PIC X(83) VALUE
           '0521NAO EXISTEM DOCUMENTOS DISPONIVEIS PARA ESTA CONSULTA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0522NAO EXISTEM MEIOS DE ENTRADA CADASTRADOS PARA O PRODUTO
      -    'SELECIONADO            '.
               10  FILLER    PIC X(83) VALUE
           '0523NAO HA TIPOS DE DOCUMENTOS DE FORMALIZACAO CADASTRADOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0524NAO HA FATOS GERADORES CONTABEIS CADASTRADOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0525CODIGO DO FATO GERADOR CONTABIL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0526FATO GERADOR CONTABIL NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0527NAO HA REGRAS DE IOF CADASTRADAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0528CODIGO DA REGRA DO IOF INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0529DATA DE INICIO DA REGRA DO IOF INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0530CODIGO DA REGRA DO IOF NAO CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0531NAO HA REGRAS DE MORA LEGAL CADASTRADAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0532CODIGO DA REGRA DE MORA LEGAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0533CODIGO DA REGRA DE MORA LEGAL NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0534NAO HA REGRAS DE MORA GERENCIAL CADASTRADAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0535CODIGO DA REGRA DE MORA GERENCIAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0536CODIGO DA REGRA DE MORA GERENCIAL NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0537NAO EXISTEM REGRAS PARA COMISSAO DE CONVENIO CADASTRADAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0538CODIGO DO TIPO DE ISENCAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0539NAO HA ISENCAO DE ANALISE OU CONSULTA DE CREDITO GERAL C
      -    'ADASTRADO              '.
               10  FILLER    PIC X(83) VALUE
           '0540DATA DE INICIO DE VIGENCIA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0541CODIGO DO PRODUTO NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0542CODIGO DO SUBPRODUTO NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0543CODIGO DA ISENCAO NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0544DATA DE FIM DE VIGENCIA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0545NAO HA SUBPRODUTO ISENCAO PESSOA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0546OPERACAO ISENCAO PESSOA NAO CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0547NAO EXISTEM ISENCOES DE CONSULTA DE CREDITO PARA O CLIEN
      -    'TE INFORMADO           '.
               10  FILLER    PIC X(83) VALUE
           '0548PESSOA ISENCAO DESCONTO NAO CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0549CNPJ/CPF CEDENTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0550ISENCAO DA ANALISE DE RESTRICAO DO SACADO NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0551NAO HA AUTORIZACOES SOBRE OPERACOES ESPECIAIS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0552CODIGO DO FUNCIONARIO AUTORIZANTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0553CODIGO DO TIPO DE AUTORIZACAO ESPECIAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0554DATA DE INICIO DE AUTORIZACAO ESPECIAL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0555CODIGO DA FLAG/FUNCAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0556PARAMETRIZACAO DE CONTRATO LIMITE NAO CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0557TAC DO CONTRATO LIMITE NAO CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0558DATA DE INICIO DE VIGENCIA DEVE SER MENOR OU IGUAL A DAT
      -    'A FINAL DA VIGENCIA    '.
               10  FILLER    PIC X(83) VALUE
           '0559PRODUTO NAO PERMITE RENOVACAO AUTOMATICA DE CONTRATO SEL
      -    'ECIONE A OPCAO "NAO"   '.
               10  FILLER    PIC X(83) VALUE
           '0560TIPO DE TAC DE CONTRATO LIMITE INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0561VALOR DO CONTRATO LIMITE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0562CONTRATO LIMITE DEVE VINCULAR PELO MENOS UM SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0563CONTRATO LIMITE DEVE VINCULAR PELO MENOS UMA CONSULTA CO
      -    'NE                     '.
               10  FILLER    PIC X(83) VALUE
           '0564CODIGO DE AVISO AO CLIENTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0565PERIODICIDADE INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0566SITUACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0567MOMENTO DA ATUALIZACAO DE SITUACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0568DESCRICAO DO PRODUTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0569DESCRICAO ABREVIADA DO PRODUTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0570SIGLA DO PRODUTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0571TAXA MINIMA DE JUROS INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0572TAXA MAXIMA DE JUROS INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0573CODIGO MINIMO - NTAX INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0574CODIGO MAXIMO - NTAX INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0575O CAMPO MAX(NTAX) NAO PODE SER INFERIOR AO CAMPO MIN(NTA
      -    'X)                     '.
               10  FILLER    PIC X(83) VALUE
           '0576CODIGO DO PRODUTO JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0577ERRO AO ACESSAR OS DADOS - TENTE NOVAMENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0578ERRO AO ACESSAR OS DADOS DO LOG - TENTE NOVAMENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0579CODIGO DO TIPO DE DESCONTO JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0580TIPO NAO PODE SER EXCLUIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0581CODIGO DO TIPO DE PESSOA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0582DOCUMENTO DE FORMALIZACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0583EXISTE CONDICAO DE PAGAMENTO CADASTRADO PARA ESTE TIPO D
      -    'E DESCONTO             '.
               10  FILLER    PIC X(83) VALUE
           '0584CODIGO DA REGRA DE COMISSAO DE CONVENIO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0585CODIGO DO MEIO DE LIBERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0586AVISO AO CLIENTE NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0587SUBPRODUTO AVISO CLIENTE JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0588SUBPRODUTO ROTEIRO INCLUSAO NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0589ERRO AO ACESSAR SUBPRODUTO ROTEIRO INCLUSAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0590CODIGO INDICADOR DE VENCIMENTO DA PARCELA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0591DOCUMENTO DE FORMALIZACAO JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0592DESCRICAO DO FATO GERADOR CONTABIL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0593DESCRICAO RESUMIDA DO FATO GERADOR CONTABIL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0594FATO GERADOR CONTABIL JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0595EXISTE ROTEIRO DE TRILHA CONTABIL CADASTRADA PARA ESTE C
      -    'ODIGO DE FATO GERADOR  '.
               10  FILLER    PIC X(83) VALUE
           '0596DESCRICAO DA REGRA DO IOF INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0597INDICADOR DE PESSOA TOMADORA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0598CODIGO DA NATUREZA JURIDICA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0599TIPO DE RAMO DE ATIVIDADE INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0600CODIGO DO PORTE DA EMPRESA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0601CODIGO LOGICO DO VALOR INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0602VALOR REFERENCIA OPERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0603CODIGO LOGICO DO PRAZO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0604TAXA REFERENCIA OPERACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0605ALIQUOTA IOF INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0606INDICADOR DE FREQUENCIA DE ALIQUOTA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0607JA EXISTE REGRA COM OS PARAMETROS INFORMADOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0608DATA DE INICIO DE VIGENCIA OLD INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0609CODIGO DE REGRA DE ENQUADRAMENTO PARA COBRANCA DE IOF NA
      -    'O CADASTRADA           '.
               10  FILLER    PIC X(83) VALUE
           '0610DESCRICAO DA REGRA DE MORA LEGAL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0611INDICE DE CORRECAO DE DIVIDA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0612INDICE DE CALCULO DE MORA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0613INDICE DE CALCULO DE JURO DE MORA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0614CODIGO DA REGRA DE MORA GERENCIAL JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0615CODIGO DA REGRA DE MORA LEGAL JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0616DESCRICAO DA REGRA DE MORA LEGAL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0617DESCRICAO DA REGRA DE MORA GERENCIAL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0618INICIO DE VENCIMENTO DA PARCELA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0619FINAL DE VENCIMENTO DA PARCELA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0620INDICE DE RECALCULO DE VENCIMENTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0621EXISTE REGRA OPERACIONAL CADASTRADA PARA ESTA REGRA DE M
      -    'ORA GERENCIAL          '.
               10  FILLER    PIC X(83) VALUE
           '0622ERRO AO ACESSAR OS DADOS CONVENIO CLIENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0623EXISTE CONVENIO CLIENTE CADASTRADO PARA ESTA REGRA DE MO
      -    'RA GERENCIAL           '.
               10  FILLER    PIC X(83) VALUE
           '0624ERRO AO ACESSAR OS DADOS REGRA DE MORA GERENCIAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0625CAMPO COD INDICE CORRECAO(NTAX) OU COD DE JUROS (NTAX) E
      -    ' OBRIGATORIO           '.
               10  FILLER    PIC X(83) VALUE
           '0626NAO HA SUBPRODUTO CONCILIACAO CONTABIL CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0627CODIGO DA CRITICA DA COMISSAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0628INDICE DE CALCULO DA COMISSAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0629INDICE DE PERCENTUAL DE COMISSAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0630INDICE DE PAGAMENTO DE COMISSAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0631CODIGO DA REGRA DE COMISSAO DE CONVENIO JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0632CODIGO DA REGRA DE COMISSAO DE CONVENIO NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0633EXISTE CONVENIO CLIENTE CADASTRADO PARA ESTA REGRA DE CO
      -    'MISSAO                 '.
               10  FILLER    PIC X(83) VALUE
           '0634INDICE DE RESTRICAO DE VALOR INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0635VALOR DE RESTRICAO MAXIMO DA OPERACAO NAO PODE SER ZEROS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0636ERRO AO ACESSAR OS DADOS DA ISENCAO DA ANALISE DE RESTRI
      -    'CAO GERAL              '.
               10  FILLER    PIC X(83) VALUE
           '0637PERIODO INFORMADO JA POSSUI ISENCAO DA ANALISE DE RESTRI
      -    'CAO GERAL              '.
               10  FILLER    PIC X(83) VALUE
           '0638PERIODO INFORMADO JA POSSUI ISENCAO DE CONSULTA DE CREDI
      -    'TO GERAL               '.
               10  FILLER    PIC X(83) VALUE
           '0639CODIGO DA ISENCAO DA ANALISE DE RESTRICAO GERAL JA CADAS
      -    'TRADO                  '.
               10  FILLER    PIC X(83) VALUE
           '0640DATA DE INICIO DE VIGENCIA ANTERIOR INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0641ISENCAO DA ANALISE DE RESTRICAO GERAL NAO CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0642ISENCAO FINALIZA HOJE, P/ VALIDAR ALTERACAO, ATUALIZAR A
      -    ' DATA FINAL DA VIGENCIA'.
               10  FILLER    PIC X(83) VALUE
           '0643DATA ALTERADA, ISENCAO VALIDA ATE FINAL DO DIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0644ERRO AO ACESSAR OS DADOS DA VIGENCIA DO SUBPRODUTO PARA
      -    'PARA ENCERRAMENTO      '.
               10  FILLER    PIC X(83) VALUE
           '0645ERRO AO ACESSAR OS DADOS DA ISENCAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0646SUBPRODUTO ISENCAO DESCONTO NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0647ERRO AO ACESSAR OS DADOS SUBPRODUTO ISENCAO DESCONTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0648PERIODO JA ENCERRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0649ERRO AO EXCLUIR SUBPRODUTO ISENCAO DESCONTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0650ERRO AO ENCERRAR SUBPRODUTO ISENCAO DESCONTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0651DATA DE INICIO DE VIGENCIA DE ISENCAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0652DATA FINAL DE VIGENCIA DE ISENCAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0653CLIENTE JA POSSUI ISENCAO PARA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0654ERRO AO ACESSAR ISENCAO PARA OPERACAO PARA O CLIENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0655TIPO DE ISENCAO PARA PESSOA NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0656ERRO AO ACESSAR TIPO DE ISENCAO PARA PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0657ERRO AO OBTER A SEQUENCIA DO ELEMENTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0658ERRO AO ACESSAR OS DADOS ISENCAO DE PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0659VALOR DE RESTRICAO MAXIMO DA OPERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0660ISENCAO P/ SUBPRODUTO NO PERIODO NAO CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0661ERRO AO ACESSAR OS DADOS DE ISENCAO P/ SUBPRODUTO NO PER
      -    'IODO                   '.
               10  FILLER    PIC X(83) VALUE
           '0662SUBPRODUTO ISENCAO PESSOA JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0663ERRO AO INCLUIR SUBPRODUTO ISENCAO PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0664SUBPRODUTO ISENCAO PESSOA NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0665ERRO AO ACESSAR OS DADOS SUBPRODUTO ISENCAO PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0666VIGENCIA DO SUBPRODUTO ISENCAO PESSOA JA ENCERRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0667SUBPRODUTO ISENCAO PESSOA NAO ENCERRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0668ERRO AO ENCERRAR SUBPRODUTO ISENCAO PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0669SUBPRODUTO ISENCAO PESSOA NAO EXCLUIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0670ERRO AO EXCLUIR SUBPRODUTO ISENCAO PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0671PESSOA ISENCAO NAO EXCLUIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0672ERRO AO EXCLUIR PESSOA ISENCAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0673VIGENCIA JA ENCERRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0674PESSOA ISENCAO DESCONTO NAO CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0675ERRO AO VALIDAR PESSOA ISENCAO DESCONTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0676ERRO AO ENCERRAR OPERACAO ISENCAO PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0677ERRO AO EXCLUIR OPERACAO ISENCAO PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0678ERRO AO VALIDAR OPERACAO ISENCAO PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0679ERRO AO ENCERRAR PESSOA ISENCAO DESCONTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0680INDICADOR DE RESTRICAO DE CEDENTE/CLIENTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0681SEQUENCIA DE ISENCAO DE ANALISE DE RESTRICAO NAO CADASTR
      -    'ADA                    '.
               10  FILLER    PIC X(83) VALUE
           '0682ERRO DE OBTENCAO DE SEQUENCIA DE ISENCAO DE ANALISE DE R
      -    'ESTRICAO               '.
               10  FILLER    PIC X(83) VALUE
           '0683ERRO NO ENCERRAMENTO DA ISENCAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0684ERRO AO ACESSAR OS DADOS ENCERRAMENTO DA ISENCAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0685VIGENCIA SOLICITADA INVADE VIGENCIAS CADASTRAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0686AUTORIZACAO OPERACAO ESPECIAL JA CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0687ERRO AO ACESSAR OS DADOS AUTORIZACAO OPERACAO ESPECIAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0688TAXA MAXIMA DO CONTRATO LIMITE INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0689INDICE DE PRAZO DO CONTRATO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0690PRAZO DE CARENCIA DE RENOVACAO DO CONTRATO EXCEDIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0691INDICE DE PRAZO DE RENOVACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0692INDICE DE RENOVACAO AUTOMATICA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0693INDICE DE TARIFA DE ADITAMENTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0694DATA DE INICIO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0695VALOR MAXIMO DE RENOVACAO AUTOMATICA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0696CODIGO DE TIPO DE COBRANCA DE TAC INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0697CODIGO DE TAC DO CONTRATO LIMITE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0698CODIGO DE TAC PERCENTUAL DO CONTRATO LIMITE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0699CODIGO DE TAC MINIMO DO CONTRATO LIMITE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0700CODIGO DE TAC MAXIMO DO CONTRATO LIMITE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0701DATAS INVALIDAS INICIO DEVE SER MAIOR QUE A DATA ATUAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0702ERRO AO ACESSAR DADOS PARAMETRIZACAO CONTRATO LIMITE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0703ERRO AO ACESSAR OS DADOS TAC CONTRATO LIMITE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0704DATA DE INICIO DE PARAMETRIZACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0705CODIGO DE TRANSFERENCIA DE CREDITO EM ATRASO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0706INDICE DE PRAZO MORA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0707VALOR MAXIMO DE REDUCAO DE PARCELA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0708INDICADOR DE DEBITO RESIDUAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0709DATA FINAL DE PARAMETRIZACAO ANTIGA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0710DATA FINAL DE PARAMETRIZACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0711ERRO AO ACESSAR DADOS PARAMETRIZACAO PARCELA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0712PARAMETRIZACAO DO CONTRATO LIMITE JA CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0713ERRO NO ACESSO A TRIC
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0714ERRO NA BRAD0025-SOMA-DIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0715CODIGO DO PRODUTO BASE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0716CODIGO DO SUBPRODUTO BASE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0717DESCRICAO RESUMIDA DO 1UBPRODUTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0718CODIGO DA SIGLA DO SUBPRODUTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0719ERRO AO ACESSAR OS DADOS DE SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0720SITUACAO SUBPRODUTO BASE NAO PERMITE INCLUSAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0721ERRO AO ACESSAR OS DADOS DO ROTEIRO P/ CADASTRAMENTO DE
      -    'SUBPRODUTO             '.
               10  FILLER    PIC X(83) VALUE
           '0722NAO HA ROTEIRO P/ CADASTRAR SUBPRODUTO DO PRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0723SUBPRODUTO JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0724ERRO AO INSERIR OS DADOS DE SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '07252UBPRODUTO ROTEIRO INCLUSAO JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0726ERRO AO INSERIR OS DADOS SUBPRODUTO ROTEIRO INCLUSAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0727REGRA DE CONTRATACAO DO SUBPRODUTO NAO CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0728ERRO AO ACESSAR REGRA DE CONTRATACAO DO SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0729DESCRICAO DO SUBPRODUTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0730ERRO AO ATUALIZAR OS DADOS DE SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0731ERRO AO ATUALIZAR DADOS SUBPRODUTO REGRA CONTRATACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0732ERRO AO REGULARIZAR DADOS SUBPRODUTO REGRA CONTRATACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0733ERRO AO ACESSAR OS DADOS DE SUBPRODUTO REGRA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0734ERRO AO ATUALIZAR OS DADOS SUBPRODUTO REGRA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0735ERRO AO REGULARIZAR DADOS SUBPRODUTO REGRA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0736ERRO AO ACESSAR OS DADOS SUBPRODUTO MEIO DE LIBERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0737ERRO AO ATUALIZAR DADOS SUBPRODUTO MEIO DE LIBERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0738ERRO AO REGULARIZAR DADOS SUBPRODUTO MEIO DE LIBERACAOc
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0739ERRO AO ACESSAR OS DADOS SUBPRODUTO MEIO DE PAGAMENTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0740ERRO AO ATUALIZAR DADOS SUBPRODUTO MEIO DE PAGAMENTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0741ERRO AO REGULARIZAR DADOS SUBPRODUTO MEIO DE PAGAMENTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0742ERRO AO ACESSAR OS DADOS SUBPRODUTO MEIO DE ENTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0743ERRO AO ATUALIZAR DADOS SUBPRODUTO MEIO DE ENTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0744ERRO AO REGULARIZAR DADOS SUBPRODUTO MEIO DE ENTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0745ERRO AO ACESSAR OS DADOS SUBPRODUTO TAXA DE JUROS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0746ERRO AO ATUALIZAR DADOS SUBPRODUTO TAXA DE JUROS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0747ERRO AO REGULARIZAR DADOS SUBPRODUTO TAXA DE JUROS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0748ERRO AO ACESSAR OS DADOS SUBPRODUTO TAXA DEFINIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0749ERRO AO ATUALIZAR DADOS SUBPRODUTO TAXA DEFINIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0750ERRO AO REGULARIZAR DADOS SUBPRODUTO TAXA DEFINIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0751ERRO AO ACESSAR OS DADOS SUBPRODUTO TAXA DE REDE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0752ERRO AO ATUALIZAR DADOS SUBPRODUTO TAXA DE REDE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0753ERRO AO REGULARIZAR DADOS SUBPRODUTO TAXA DE REDE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0754ERRO AO ACESSAR DADOS SUBPRODUTO SELECAO DE CARTEIRA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0755ERRO AO ATUALIZAR DADOS SUBPRODUTO SELECAO DE CARTEIRA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0756ERRO AO REGULARIZAR DADOS SUBPRODUTO SELECAO CARTEIRA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0757ERRO AO ACESSAR DADOS SUBPRODUTO CONCILIACAO CONTABIL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0758ERRO AO ATUALIZAR DADO SUBPRODUTO CONCILIACAO CONTABIL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0759INCLUSAO INVALIDA. DADOS GERAIS JA INCLUIDOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0760REGRA DE CONTRATACAO DO SUBPRODUTO JA CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0761ERRO AO ACESSAR OS DADOS DE SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0762SUBPRODUTO MEIO ENTRADA JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0763INICIO DO MEIO DE LIBERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0764INDICADOR DE RESTRICAO DE CEDENTE/CLIENTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0765ISENCAO GERAL NAO DEVE FORNECER CNPJ/CPF CEDENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0766ISENCAO FINALIZA HOJE, P/ VALIDAR ALTERACAO, ATUALIZAR A
      -    ' DATA FINAL DE VIGENCIA'.
               10  FILLER    PIC X(83) VALUE
           '0767DATA ALTERADA. ISENCAO VALIDA ATE FINAL DO DIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0768CLIENTE JA POSSUI ISENCAO PARA O PERIODO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0769PESSOA NAO ENCONTRADA AO EXECUTAR ENCERRAMENTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0770ERRO AO ACESSAR OS DADOS - ENCERRAMENTO DE PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0771SACADO NAO ENCONTRADO AO EXECUTAR ENCERRAMENTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0772ERRO AO ACESSAR OS DADOS - ENCERRAMENTO DE SACADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0773ISENCAO NAO ENCONTRADA AO EXECUTAR EXCLUSAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0774ERRO AO ACESSAR OS DADOS - EXCLUSAO DE ISENCAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0775SACADO NAO ENCONTRADO AO EXECUTAR EXCLUSAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0776ERRO AO ACESSAR OS DADOS -EXCLUSAO DE SACADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0777ERRO NA OBTENCAO DA SEQUENCIA AUTOMATICA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0778ISENCAO JA CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0779ERRO AO ACESSAR OS DADOS INCLUSAO DE ISENCAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0780SACADO JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0781ERRO AO ACESSAR OS DADOS INCLUSAO DE SACADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0782ERRO AO RECUPERAR PESSOA ISENCAO DESCONTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0783ERRO AO RECUPERAR PROPONENTE-SACADO ISENCAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0784INDICADOR DE LIBERACAO ANTECIPADA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0785HORA DE INICIO DO MEIO DE ENTRADA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0786HORA FINAL DO MEIO DE ENTRADA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0787PRAZO MAXIMO DE LIBERACAO ANTECIPADA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0788TAXA MINIMA DE VENCIMENTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0789TAXA MAXIMA DE VENCIMENTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0790INDICADOR DE PRAZO DE VENCIMENTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0791PRAZO MAXIMO DE ACOLHIMENTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0792INDICADOR DE PRAZO DE ACOLHIMENTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0793VALOR MAXIMO DE DIVERGENCIA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0794INDICADOR DE OBRIGATORIEDADE DE GARANTIA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0795INDICADOR DE OBRIGATORIEDADE DE PROMISSORIA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0796INDICADOR DE FORMALIZACAO DA OPERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0797INDICADOR DE TIPO DE FORMULARIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0798SUBPRODUTO MEIO ENTRADA NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0799SUBPRODUTO AVISO CLIENTE NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0800CODIGO DO SEGMENTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0801CODIGO DO RATING INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0802DATA INICIAL DO CRITERIO DE REDE INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0803CODIGO DO TIPO DE AGRUPAMENTO DO ROTEIRO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0804SITUACAO DO SUBPRODUTO ATUALIZADA COM SUCESSO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0805AGRUPAMENTO(S) CONCLUIDO(S) COM SUCESSO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0806ERRO AO ACESSAR ROTEIRO DE INCLUSAO DE SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0807INICIO DO MEIO DE ENTRADA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0808FINAL DO MEIO DE ENTRADA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0809ERRO AO ACESSAR DESCRICAO CARTEIRA NORMAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0810ERRO AO ACESSAR DESCRICAO CARTEIRA VENCIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0811ERRO AO ACESSAR DESCRICAO CARTEIRA DESCONTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0812CODIGO DO PRODUTO COBRANCA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0813SUBPRODUTO MEIO ENTRADA NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0814SUBPRODUTO SELECAO CARTEIRA JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0815CODIGO DA COBRANCA DA TAC INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0816VALOR DA TAC INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0817CODIGO DA TAC INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0818CODIGO DA TAC (MIN) INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0819CODIGO DA TAC (MAX) INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0820CODIGO DO PERCENTUAL DA TAC INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0821CODIGO DA TAC CONTRATO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0822VALOR DA TAC OPERACAO EFETUADA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0823CODIGO DA TAC OPERACAO EFETUADA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0824CODIGO DA TAC (MIN) CONTRATO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0825CODIGO DA TAC (MAX) CONTRATO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0826CODIGO DA TAC (PERCENTUAL) CONTRATO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0827CODIGO DA TARIFA TRANSFERENCIA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0828VALOR DA TARIFA TRANSFERENCIA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0829CODIGO E VALOR DA TAC OPERACAO ZERADOS INVALIDOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0830CODIGO E VALOR DA TAC OPERACAO NAO ZERADOS INVALIDOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0831CODIGO VALOR PERC(NTAX) EM TAC DA OPERACAO INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0832CODIGO VALOR PERC(NTAX) TAC CONTR LIMITE INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0833CODIGO TARIFA TRANSFERENCIA CARTEIRA ZERADOS INVALIDOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0834DATA INICIAL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0835DATA INICIAL OLD INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0836CODIGO DO TIPO DE TAXA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0837CONTA CONTABIL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0838CRITING DO CLIENTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0839INICIO DO CRITERIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0840PERCENTUAL DA TAXA DE JUROS INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0841INDICADOR DE FREQUENCIA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0842INDICADOR DE TAXA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0843INICIO DA TAXA DE JUROS INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0844FINAL DA TAXA DE JUROS INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0845INICIO DO CRITERIO ANTERIOR INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0846CODIGO DO PRODUTO CONTABIL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0847CODIGO DA OPERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0848MEIO DE LIBERACAO NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0849ERRO AO ACESSAR MEIO DE LIBERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0850INDICADOR DE RESTRICAO DE VALOR DA OPERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0851VALOR DA RESTRICAO DA OPERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0852ERRO AO ACESSAR OS DADOS PARA VALIDACAO DA PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0853ERRO AO ACESSAR OS DADOS PARA VALIDACAO DO SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0854CLIENTE JA POSSUI ISENCAO P/ SUBPRODUTO NO PERIODO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0855ERRO NA OBTENCAO PROXIMO CODIGO SEQUENCIA DO ELEMENTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0856ISENCAO PESSOA JA CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0857ERRO AO ACESSAR OS DADOS DE ISENCAO PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0858SUBPRODUTO ISENCAO PESSOA JA CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0859ERRO AO ACESSAR DADOS 3UBPRODUTO ISENCAO PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0860INDICADOR DE RESTRICAO POR CEDENTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0861ERRO AO ACESSAR DADOS ISENCAO ANALISE RESTRICAO SACADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0862ISENCAO DA ANALISE DE RESTRICAO CEDENTE JA CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0863ERRO AO ACESSAR OS DADOS ISENCAO DA ANALISE DE RESTRICAO
      -    ' CEDENTE               '.
               10  FILLER    PIC X(83) VALUE
           '0864SITUACOES POR ELEMENTO NAO CADASTRADAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0865ERRO AO ACESSAR SITUACAO POR ELEMENTOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0866INDICADOR DE TAXA MAX REDE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0867XXXXXXXXXXXXXXXXXXXXXXXX POR ELEMENTOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0868XXXXXXXXXXXXXXXXXXXUACAO POR ELEMENTOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0869XXXXXXXXXXXXXXXXXXXXXXAO POR ELEMENTOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0870XXXXXXXXXXXXXXXXXXXUACAO POR ELEMENTOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0871PRAZO MAXIMO ACOLHIMENTO TITULO NAO PODE SER SUPERIOR PR
      -    'AZO MINIMO VENCIMENTO  '.
               10  FILLER    PIC X(83) VALUE
           '0872DATA DE INICIO MAIOR QUE A DATA DE VENCIMENTO DA OPERACA
      -    'O                      '.
               10  FILLER    PIC X(83) VALUE
           '0873DATA DE INICIO DA OPERACAO NAO PERMITIDA FORA DO INTERVA
      -    'LO D-1 ATE D+1         '.
               10  FILLER    PIC X(83) VALUE
           '0874DATA DE VENCIMENTO DA OPERACAO MAIOR QUE A DATA FINAL DO
      -    'SUBPRODUTO             '.
               10  FILLER    PIC X(83) VALUE
           '0875ALTERACAO DO MEIO DE ENTRADA NAO PERMITIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0876DADOS DE ENTRADA INCORRETO - BRAD0025
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0877DADOS DE ENTRADA INCORRETO - BRAD0015
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0878CNPJ/CPF INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0879QUANTIDADE DE AVALISTA INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0880AVALISTA NAO ENCONTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0881ERRO AO ATUALIZAR O TIPO DE PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0882ERRO AO ATUALIZAR A RELACAO PESSOA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0883CEP INCORRETO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0884CEP INCONSISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0885DATA DO PAGAMENTO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0886DATA E HORA (TIMESTAMP) INVALIDOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0887DATA INICIAL DE VENCIMENTO DE CRITERIO DE SELECAO INVALI
      -    'DA                     '.
               10  FILLER    PIC X(83) VALUE
           '0888VALOR DE ARGUMENTO DE CRITERIO DE SELECAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0889VALOR INICIAL DE CRITERIO DE SELECAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0890VALOR FINAL DE CRITERIO DE SELECAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0891DATA INICIAL DE CRITERIO DE SELECAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0892DATA FINAL DE CRITERIO DE SELECAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0893PRAZO INICIAL DE CRITERIO DE SELECAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0894PRAZO FINAL DE CRITERIO DE SELECAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0895TIPO DE VENCIMENTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0896CODIGO DE NEGOCIACAO NA COBRANCA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0897TIPO DE SOLICITACAO INVALIDA PARA OBTER TITULOS NO SISTE
      -    'MA DE COBRANCA (CBON)  '.
               10  FILLER    PIC X(83) VALUE
           '0898ERRO AO OBTER TITULOS NO SISTEMA DE COBRANCA (CBON). COD
      -    'IGO                    '.
               10  FILLER    PIC X(83) VALUE
           '0899NAO HA CARTEIRAS DE COBRANCA CADASTRADAS PARA A OPERACAO
      -    ' DE DESCONTO           '.
               10  FILLER    PIC X(83) VALUE
           '0900CODIGO MENSAGEM ALTERADO PARA 931-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0901CODIGO MENSAGEM ALTERADO PARA 932-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0902CODIGO MENSAGEM ALTERADO PARA 933-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0903CODIGO MENSAGEM ALTERADO PARA 934-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0904CODIGO MENSAGEM ALTERADO PARA 935-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0905CODIGO MENSAGEM ALTERADO PARA 936-COD RESER INTER CADU
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0906CODIGO MENSAGEM ALTERADO PARA 937-COD RESER INTER CBCO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0907CODIGO MENSAGEM ALTERADO PARA 938-COD RESER INTER CBON
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0908CODIGO MENSAGEM ALTERADO PARA 939-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0909CODIGO MENSAGEM ALTERADO PARA 940-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0910CODIGO MENSAGEM ALTERADO PARA 941-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0911CODIGO MENSAGEM ALTERADO PARA 942-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0912CODIGO MENSAGEM ALTERADO PARA 943-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0913CODIGO MENSAGEM ALTERADO PARA 944-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0914CODIGO MENSAGEM ALTERADO PARA 945-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0915CODIGO MENSAGEM ALTERADO PARA 946-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0916CODIGO MENSAGEM ALTERADO PARA 947-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0917CODIGO MENSAGEM ALTERADO PARA 948-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0918CODIGO MENSAGEM ALTERADO PARA 949-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0919CODIGO MENSAGEM ALTERADO PARA 950-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0920CODIGO MENSAGEM ALTERADO PARA 951-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0921CODIGO MENSAGEM ALTERADO PARA 952-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0922CODIGO MENSAGEM ALTERADO PARA 953-COD RESERVADO INTERF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0923TESTE 0923
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0924TESTE 0924
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0925TESTE 0925
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0926TESTE 0926
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0927TESTE 0927
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0928TESTE 0928
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0929TESTE 0929
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0930TESTE 0930
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0931EXISTE DOCUMENTO PARA FORMALIZACAO CADASTRADO PARA ESTE
      -    'TIPO DE DESCONTO       '.
               10  FILLER    PIC X(83) VALUE
           '0932EXISTE MEIO DE ENTRADA CADASTRADO PARA ESTE TIPO DE DESC
      -    'ONTO                   '.
               10  FILLER    PIC X(83) VALUE
           '0933EXISTE SUBPRODUTO CADASTRADO PARA ESTE TIPO DE DESCONTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0934EXISTE SUBPRODUTO CADASTRADO ASSOCIADO A ESTE MEIO DE EN
      -    'TRADA                  '.
               10  FILLER    PIC X(83) VALUE
           '0935EXISTE REGRA OPERACIONAL CADASTRADA PARA ESTA REGRA DE M
      -    'ORA LEGAL              '.
               10  FILLER    PIC X(83) VALUE
           '0936DESCRICAO DO MEIO DE ENTRADA EM BRANCO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0937VALOR DO CONTRATO LIMITE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0938VLR CONTR.LIMITE DEVE SER MENOR/IGUAL SALDO TOTAL DE AUT
      -    'ORIZACOES SELECIONADAS '.
               10  FILLER    PIC X(83) VALUE
           '0939VENCIMENTO DO CONTRATO EXCEDEU O PRAZO PERMITIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0940EXISTE ESPECIE DE TITULO CADASTRADA PARA ESTE TIPO DE DE
      -    'SCONTO                 '.
               10  FILLER    PIC X(83) VALUE
           '0941SITUACAO CONTRATO LIMITE IMPROPRIA PARA A SOLICITACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0942JA CADASTRADO(A) PARA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0943TESTEMUNHAS NAO PODEM SER AS MESMAS PARA O CONTRATO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0944FIADOR NAO PODE SER O CEDENTE DO CONTRATO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0945EXCEDEU NUMERO MAXIMO DE FIADORES DO CONTRATO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0946TESTEMUNHA JA CADASTRADA PARA O CONTRATO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0947EXCEDEU NUMERO MAXIMO DE TESTEMUNHAS DO CONTRATO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0948PRAZO PARA CANCELAMENTO DE LIBERACAO CONTRATO EXPIROU
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0949CANCELAMENTO DE LIBERACAO INVALIDO. CONTRATO JA AMPARA O
      -    'PERACOES DESCONTO      '.
               10  FILLER    PIC X(83) VALUE
           '0950TIPO DE TAC NAO PARAMETRIZADA PARA PRODUTO/CONTRATO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0951VALOR DO CONTRATO LIMITE EXCEDEU VALOR MAXIMO DE RENOVAC
      -    'ACAO AUTOMATICA        '.
               10  FILLER    PIC X(83) VALUE
           '0952*** MENSAGEM MONTADA NO PROGRAMA DCOM4115 ***
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0953CONTRATO LIMITE CANCELADO POR DECURSO DE PRAZO DE LIBERA
      -    'CAO                    '.
               10  FILLER    PIC X(83) VALUE
           '0954TIPO DE CONSULTA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0955SITUACAO DA ANALISE DE CREDITO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0956JUSTIFICATIVA DE BLOQUEIO DE PARCELA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0957JUSTIFICATIVA DE DESBLOQUEIO DE PACELA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0958TIPO DE AUTORIZACAO ESPECIAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0959DIRETORIA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0960GERENCIA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0961INDICADOR DE TIPO DE CONSULTA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0962DATA DE INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0963DATA ATE INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0964CONSOLIDADO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0965PRODUTO NAO POSSUI CONTRATO LIMITE PARAMETRIZADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0966SITUACAO DO SUBPRODUTO NAO PERMITE RENOVACAO DO CONTRAT
      -    'O LIMITE               '.
               10  FILLER    PIC X(83) VALUE
           '0967DATA DE VENCIMENTO DEVE SER MAIOR QUE DATA DE CONTRATO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0968DT.VENC NAO UTIL ALTERADA PROX. DIA UTIL TECLE CONFIRMAR
      -    ' OU ATUALIZE           '.
               10  FILLER    PIC X(83) VALUE
           '0969DATA FIM DESO DEVE SER MAIOR OU IGUAL A DATA ATUAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0970DATA INICIO DCOM DEVE SER MAIOR QUE DATA ATUAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0971*** MENSAGEM MONTADA NO PROGRAMA DCOM4108 ***
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0972CONJUGE NAO PODE SER O CEDENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0973AVALISTA NAO PODE SER O CEDENTE DA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0974SITUACAO DA OPERACAO IMPROPRIA PARA A SOLICITACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0975EXCEDEU O NUMERO DE AVALISTAS (MAX=4)
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0976EXCEDEU O NUMERO DE TESTEMUNHAS (MAX=2)
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0977NAO EXISTEM SUBPRODUTOS VALIDOS PARA O PRODUTO SELECIONA
      -    'DO                     '.
               10  FILLER    PIC X(83) VALUE
           '0978SUBPRODUTO GARANTIA ENTRADA NAO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0979NUMERO DE OPERACAO DO MEIO DE LIBERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0980INDICADOR DE SITUACAO DE TITULO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0981INDICADOR DE SITUACAO DE SACADOR INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0982INDICADOR DE TITULO CLIENTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0983DE ACEITE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0984DATA DE EMISSAO DO OFICIO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0985SIGLA DA ESPECIE DO TITULO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0986GARANTIA INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0987OPCAO DE CRITERIO NAO INFORMADa
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0988PARAMETROS INVALIDOS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0989FALTA ARGUMENTO OBRIGATORIO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0990NOSSO NUMERO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0991DATA DE INICIO DO VENCIMENTO DO TITULO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0992DATA DE TERMINO DO VENCIMENTO DO TITULO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0993CODIGO AGENCIA JA CADASTRADO PARA IMPLANTACAO IMPC
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0994DESO JA FINALIZADO, NAO PERMITE ALTERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0995DCOM JA INICIALIZADO, NAO PERMITE ALTERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0996CPF (999999999-99) JA UTILIZADO COMO XXXXXXXXXXXXX
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0997CPF DO CONJUGE NAO PODE SER IGUAL AO CPF DO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0998PRODUTO NAO POSSUI CONTRATO LIMITE PARAMETRIZADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '0999ERRO AO OBTER A DATA DE REFERENCIA DO SISTEMA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1000CHAVE DO ELEMENTO DESCONTO NAO INFORMADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1001DATA DE VENCIMENTO DO CONVENIO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1002DATA DE VENCIMENTO DO CONVENIO DEVE SER MAIOR/IGUAL A DA
      -    'TA INICIO              '.
               10  FILLER    PIC X(83) VALUE
           '1003VALOR LIMITE DE CONVENIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1004DEPENDENCIA EXCLUSIVA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1005DATA DE CADASTRO DEVE SER IGUAL A DATA ATUAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1006CODIGO CONTA BANCARIA CLIENTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1007CODIGO DEPENDENCIA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1008CODIGO DE IDENTIFICACAO FIXO TAC INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1009CODIGO INDICADOR ABRANGENCIA GEOGRAFICA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1010CODIGO INDICADOR PAGADOR JURO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1011CODIGO INDICADOR PRAZO DEBITO PARCELA VENCIDA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1012CODIGO INDICADOR TARIFA REGISTRO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1013CODIGO SITUACAO DESCONTO COMERCIAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1014CODIGO VERSAO CONVENIO CLIENTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1015DATA INCLUSAO CONVENIO GERAL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1016DATA INICIO VIGENCIA CONVENIO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1017NOME CONVENIO GERAL DESCONTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1018NOME RESUMO CONVENIO GERAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1019CALCULO DE COMISSAO DE CONVENIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1020INDICE ECONOMICO JURO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1021TAXA INDICE ECONOMICO JURO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1022TIPO DE PESQUISA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1023VALOR CALCULADO COMISSAO CONVENIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1024CONVENIO JA INICIADO NAO PERMITE ALTERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1025DATA FIM DESO DEVE SER MAIOR QUE A DATA INICIO DCOM
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1026PERCENTUAL DE LIBERACAO DE OPERACAO EXCEDEU TOTAL PERMIT
      -    'IDO                    '.
               10  FILLER    PIC X(83) VALUE
           '1027INDICADOR DE MEIO DE LIBERACAO DE CREDITO EM CONTA INVAL
      -    'IDO                    '.
               10  FILLER    PIC X(83) VALUE
           '1028ROTEIRO INCOMPLETO  SELECAO SIST ORIGEM NAO INCLUIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1029CONTA CORRENTE NAO CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1030ERRO AO ACESSAR A INTERFACE CLIB
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1031CODIGO DA ACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1032FLAG/FILTRO DA OPERACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1033ELEMENTO DESCONTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1034INDICADOR DE EVENTO WORKFLOW INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1035INDICADOR DE SITUACAO FINAL DE EVENTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1036ERRO AO ACESSAR LISTA DE UF
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1037CONVENIO NAO PODE SER VALIDADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1038CONVENIO NAO PODE SER DESBLOQUEADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1039CONVENIO NAO PODE SER ADITADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1040CONVENIO NAO PODE SER BLOQUEADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1041CONVENIO JA INICIADO NAO PERMITE EXCLUSAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1042ERRO AO ACESSAR LISTA AGENCIAS
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1043ERRO AO ACESSAR LISTA DE GERENCIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1044ERRO AO ACESSAR LISTA DE DIRETORIA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1045NUMERO SEQUENCIAL BANDA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1046PRAZO DEBITO PARCELA VENCIDA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1047PRAZO VENCIMENTO PARCELA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1048DATA DO CONVENIO NAO PODE SER ALTERADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1049EXISTE MEIO DE PAGAMENTO CADASTRADO PARA ESTE TIPO DE DE
      -    'SCONTO                 '.
               10  FILLER    PIC X(83) VALUE
           '1050EXISTE SUBPRODUTO CADASTRADO ASSOCIADO A ESTE MEIO DE PA
      -    'GAMENTO                '.
               10  FILLER    PIC X(83) VALUE
           '1051CODIGO DO PRODUTOR RURAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1052CODIGO DO PRODUTO RURAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1053UNIDADE DE MEDIDA RURAL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1054NOTA PROMISSORIA RUAL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1055INDICADOR DA COBRANCA DE DESPESA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1056DATA DE EMISSAO DA NPR INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1057DOCUMENTO FISCAL JA CADASTRADO PARA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1058VALOR DA OPERACAO NAO PERMITIDO PARA SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1059RENOVACAO NAO PERMITIDA, CONTRATO LIBERADO NESTA DATA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1060BLOQUEIO NAO PERMITIDO, CONTRATO LIBERADO NESTA DATA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1061TESTEMUNHA NAO PODE SER CEDENTE DO CONTRATO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1062GARANTIA COMPLEMENTAR NAO PODE SER FIANCA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1063SOLICITACAO EFETUADA. VALORES E CONDICOES SUJEITOS A ALT
      -    'ERACOES                '.
               10  FILLER    PIC X(83) VALUE
           '1064VALOR DISPONIVEL MENOR QUE O VALOR DA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1065A SITUACAO DO CONVENIO NAO PERMITE EXCLUSAO DE TODOS OS
      -    'AVALISTAS              '.
               10  FILLER    PIC X(83) VALUE
           '1066COD VALOR(NTAX)MAX NAO PODE SER INFERIOR COD VALOR(NTAX)
      -    'MIN TAC DO CONVENIO    '.
               10  FILLER    PIC X(83) VALUE
           '1067CONVENIO ESPECIAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1068DATA DE INICIO DE VIGENCIA NAO UTIL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1069DATA DE VENCIMENTO NAO UTIL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1070ERRO AO INCLUIR CONVENIO ESPECIAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1071ERRO AO INCLUIR MEIO DE PAGAMENTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1072PRAZO DEBITO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1073AGENCIA NEGOCIADORA INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1074BANCO NEGOCIADOR INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1075NUMERO CONVENIO CLIENTE INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1076CODIGO DO CONVENIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1077CODIGO DE CONTROLE CPF/CNPJ INVALIDO.
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1078CODIGO DE DDD DO FAX INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1079CODIGO DO RG INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1080CODIGO DO NUMERO DO TELEFONE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1081CODIGO INDICADOR CONVENIO ESPECIAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1082CODIGO INDICADOR CONVENIO ROTATIVO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1083CODIGO INDICADOR MEIO COMUNICACAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1084CODIGO INDICADOR PRAZO PARCELA INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1085CODIGO INDICADOR PRAZO RECEBIMENTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1086CODIGO INDICADOR RESPONSAVEL APOS INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1087CODIGO INDICADOR RESPONSAVEL ATE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1088CODIGO INDICADOR TARIFA REGIONAL INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1089CODIGO DE MEIO DE PAGAMENTO APOS INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1090CODIGO DE MEIO DE PAGAMENTO ATE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1091CODIGO DO RAMAL DE FAX  INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1092CODIGO OU VERSAO DO CONVENIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1093DATA PAGAMENTO DE COMISSAO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1094DATA INCLUSAO CONVENIO CLIENTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1095E-MAIL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1096IDENTIFICADOR DE PERCENTUAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1097PRAZO RETORNO ARQUIVO DEBITO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1098PRAZO RECEBIMENTO ARQUIVO CONVENIO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1099CODIGO FILIAL CPF/CNPJ INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1100OPERACAO ATIVA. ISENCAO NAO PODE SER EXCLUIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1101SUBPRODUTO NAO DISPONIVEL PARA OPERAR COM CLIENTE NAO CO
      -    'RRENTISTA              '.
               10  FILLER    PIC X(83) VALUE
           '1102MEIO DE ENTRADA NAO PERMITE MEIO DE LIBERACAO DIFERENTE
      -    ' DE CREDITO EM CONTA   '.
               10  FILLER    PIC X(83) VALUE
           '1103OBRIGATORIO O CADASTRAMENTO DE AVALISTA DA OPERACAO DE D
      -    'ESCONTO                '.
               10  FILLER    PIC X(83) VALUE
           '1104PRODUTO NAO POSSUI TIPO DE DESCONTO DE PAGAMENTO A FORNE
      -    'CEDORES                '.
               10  FILLER    PIC X(83) VALUE
           '1105CLIENTE NAO POSSUI CONVENIO CADASTRADO PARA O PRODUTO E
      -    'SUBPRODUTO INFORMADOS  '.
               10  FILLER    PIC X(83) VALUE
           '1106DATA DE VENCIMENTO DEVE SER MAIOR QUE A DATA DA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1107AGENCIA-CONTA DO CLIENTE NAO SAO AS MESMAS DO CONTRATO/
      -    'CONVENIO               '.
               10  FILLER    PIC X(83) VALUE
           '1108CONTRATO LIMITE NAO PERMITIDO PARA ESTE SUBPRODUTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1109NAO HA CONTRATO LIMITE PARA O CLIENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1110LOTE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1111CHEQUE INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1112NAO HA ESPECIE DE TIPO DE DESCONTO CADASTRADA PARA O PRO
      -    'DUTO                   '.
               10  FILLER    PIC X(83) VALUE
           '1113OPERACAO ATIVA, NAO PERMITE EXCLUSAO DE ISENCAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1114PRZ MINIMO NAO PERMITIDO, P/ LIBERAR OPERACAO SERA NECES
      -    'SARIO AUTORI. ESPECIAL '.
               10  FILLER    PIC X(83) VALUE
           '1115PRZ MAXIMO NAO PERMITIDO, P/ LIBERAR OPERACAO SERA NECES
      -    'SARIO AUTORI. ESPECIAL '.
               10  FILLER    PIC X(83) VALUE
           '1116VALOR DA NOTA FISCAL EXCEDE VALOR PERMITIDO PARA OPERACA
      -    'O                      '.
               10  FILLER    PIC X(83) VALUE
           '1117NOTA FISCAL JA CADASTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1118SUBPRODUTO/MEIO DE ENTRADA EXIGE CONVENIO ESPECIAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1119VALOR ORPAG INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1120VALOR DESCONTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1121VALOR DA ORPAG DEVE SER MAIOR OU IGUAL AO VALOR DESCONTO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1122INDIQUE DATA DE VENCIMENTO  OU  PRAZO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1123VALOR MINIMO MENOR QUE O VALOR MINIMO PARAMETRIZADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1124VALOR MAXIMO MAIOR QUE O VALOR MAXIMO PARAMETRIZADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1125NUMERO CONVENIO GERAL INEXISTENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1126QUANTIDADE DE TITULOS PARA NOTA PROMISSORIA DEVE SER IGU
      -    'AL A 1                 '.
               10  FILLER    PIC X(83) VALUE
           '1127QUANTIDADE DE TITULOS PARA NOTA PROMISSORIA RURAL RPL DE
      -    'VE SER IGUAL A 1       '.
               10  FILLER    PIC X(83) VALUE
           '1128QUANTIDADE DE TITULOS PARA NOTA PROMISSORIA RURAL RO DEV
      -    'E SER IGUAL A 1        '.
               10  FILLER    PIC X(83) VALUE
           '1129LIBERACAO DE OPERACAO INVALIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1130LIBERACAO DE OPERACAO JA CONCLUIDA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1131CODIGO DE EVENTO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1132SUBPRODUTO EXIGE SELECAO DE PELO MENOS UM TITULO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1133DADOS DO EMITENTE DEVEM SER DIFERENTE DOS DADOS DO CEDEN
      -    'TE                     '.
               10  FILLER    PIC X(83) VALUE
           '1134SITUACAO DA OPERACAO NAO PERMITE FORMALIZACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1135OPERACAO NAO PERMITE A OPCAO SELECIONADA, OPERACAO CADAS
      -    'TRADA                  '.
               10  FILLER    PIC X(83) VALUE
           '1136OPERACAO NAO PERMITE A OPCAO SELECIONADA, OPERACAO JA
      -    'FORMALIZADA            '.
               10  FILLER    PIC X(83) VALUE
           '1137VALOR INFORMADO INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1138CONTA CORRENTE INVALIDA PARA MEIO DE LIBERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1139MEIO DE ENTRADA INFORMADO NAO PERMITE MEIO DE LIBERACAO
      -    'SOLICITADO             '.
               10  FILLER    PIC X(83) VALUE
           '1140CONTA CORRENTE JA CADASTRADA COMO MEIO DE LIBERACAO DA O
      -    'PERACAO                '.
               10  FILLER    PIC X(83) VALUE
           '1141OPERACAO NAO PERMITE A OPCAO SELECIONADA, OPERACAO EM CA
      -    'DASTRAMENTO            '.
               10  FILLER    PIC X(83) VALUE
           '1142OPERACAO NAO PERMITE A OPCAO SELECIONADA, OPERACAO JA LI
      -    'BERADA                 '.
               10  FILLER    PIC X(83) VALUE
           '1143OPERACAO NAO PERMITE A OPCAO SELECIONADA, OPERACAO NAO L
      -    'IBERADA                '.
               10  FILLER    PIC X(83) VALUE
           '1144OPERACAO NAO PERMITE A OPCAO SELECIONADA, OPERACAO CANCE
      -    'LADA                   '.
               10  FILLER.
                   15  FILLER PIC X(67) VALUE
           '1145OPERACAO NAO PERMITE A OPCAO SELECIONADA, OPERACAO PENDE
      -    'NTE DE '.
                   15  WRK-1145-DESC-EVNTO
                                        PIC X(16) VALUE SPACES.
               10  FILLER    PIC X(83) VALUE
           '1146CONE INEXISTENTE PARA O EMITENTE INFORMADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1147CONE INEXISTENTE PARA O CONVENIO INFORMADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1148SUBPRODUTO DISPONIVEL APENAS PARA PESSOA FISICA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1149SUBPRODUTO DISPONIVEL APENAS PARA PESSOA JURIDICA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1150ERRO AO OBTER SEGMENTO DO CLIENTE
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1151CLIENTE NAO POSSUI SEGMENTO CADASTRADO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1152OPERACAO AMPARADA POR CONTRATO LIMITE NAO PERMITE ADITAM
      -    'ENTO DE GARANTIA       '.
               10  FILLER    PIC X(83) VALUE
           '1153OPERACAO AMPARADA POR CONTRATO LIMITE NAO PERMITE ADITAM
      -    'ENTO DE AVALISTA       '.
               10  FILLER    PIC X(83) VALUE
           '1154NAO PERMITIDA EXCLUSAO DE MEIO DE ENTRADA COM CRITERIOS
      -    'CADASTRADOS            '.
               10  FILLER    PIC X(83) VALUE
           '1155OPERACAO NAO POSSUI AUTORIZACAO ESPECIAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1156NAO EXISTE AUTORIZACAO PARA ESTA OPERACAO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1157NAO PERMITIDO CREDITO EM CONTA PARA NAO CORRENTISTA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
            '1158CONVENIO GERAL NAO PERMITE ESTE TIPO DE MEIO DE ENTRADA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1159CONVENIO CLIENTE NAO PERMITE ESTE TIPO DE MEIO DE ENTRAD
      -    'A                      '.
               10  FILLER    PIC X(83) VALUE
           '1160MEIO DE ENTRADA NAO PERMITIDO PARA CONVENIO ESPECIAL
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1161EMITENTE OBRIGATORIO SER CORRENTISTA
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1162CODIGO DO SUBPRODUTO RURAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1163NUMERO DE REFERENCIA BACEN INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1164CODIGO DO TERMINAL INVALIDO
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1165
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1166
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1167
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1168
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1169
      -    '                       '.
               10  FILLER    PIC X(83) VALUE
           '1170
      -    '                       '.
           05   FILLER  REDEFINES LITERAIS OCCURS  1170  TIMES.
                10   SG-COD-MENSAGEM    PIC 9(04).
                10   SG-MENSAGEM        PIC X(79).
