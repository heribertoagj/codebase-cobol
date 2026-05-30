      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMXF *
      * CRIACAO   : 01/2007                                   *
      * DESCRICAO : VALIDA DISPONIBILIDADE DO SISTEMA         *
      *                                                       *
      * APLICACAO : MODULO DCOM6438                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************
      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************
       01  ROTENT-6438-ENVIO.
           05 ROTENT-6438-COD-RETORNO    PIC  X(04).
           05 ROTENT-6438-MSG-RETORNO    PIC  X(79).
           05 ROTENT-6438-RESTART        PIC  9(05).
           05 ROTENT-6438-FLAG           PIC  X(01).
           05 ROTENT-6438-CFUNC-ENV      PIC  9(09).
           05 ROTENT-6438-CTERM          PIC  X(08).
           05 ROTENT-6438-AGEUSU-ENV     PIC  9(05).
           05 ROTENT-6438-PROD-ENV       PIC  9(03).
           05 ROTENT-6438-PROC-ENV       PIC  X(01).
      *       INFORMAR (R=RETROATIVO, M=MOVIMENTO)
           05 ROTENT-6438-FUNC-ENV       PIC  X(01).
      *       INFORMAR (I,A,C,E)
           05 ROTENT-6438-MOT-ENV        PIC  X(02).
      *       INFORMAR (OP=OPERACAO, SI=SIMULACAO, CL=CONTRATO,
      *       CV=CONVENIO, OU BRANCO)
           05 ROTENT-6438-OPCAO-ENV      PIC  9(03).
      *       INFORMAR:
      *       001 = CADASTRAMENTO DA OPERACAO
      *       002 = VALIDACAO DA OPERACAO
      *       003 = FORMALIZACAO DA OPERACAO
      *       004 = FORMALIZACAO DA ADITAMENTO DA OPERACAO
      *       005 = FORMALIZACAO DO ADITAMENTO DA PARCELA
      *       006 = FORMALIZACAO DA PRORROGACAO DE VENCIMENTO
      *       007 = FORMALIZACAO DO CONTRATO LIMITE
      *       008 = LIBERACAO DA OPERACAO
      *       009 = LIBERACAO DO CONTRATO LIMITE
      *       010 = ESTORNO DA OPERACAO
      *       011 = ESTORNO DO CONTRATO LIMITE
      *       012 = CONSULTA OPERACOES
      *       013 = DIGITACAO DE TITULOS
      *       014 = ADITAMENTO DA OPERACAO
      *       015 = ADITAMENTO DA PARCELA
      *       016 = SIMULACAO DE OPERACAO
      *       017 = CONSULTA SIMULACAO DE OPERACAO
      *       018 = CONSULTA CONVENIOS
      *       019 = CADASTRAMENTO CONTRATO LIMITE
      *       020 = MANUTENCAO DE CONTRATO LIMITE
      *       021 = BAIXAS COMANDADAS
      *       022 = VALIDACAO DE BAIXAS COMANDADAS
      *       023 = ESTORNO DE BAIXAS COMANDADAS
      *       024 = PRORROGACAO DE VENCIMENTOS
      *       025 = CONSULTA PRODUTOS/SUBPRODUTOS
      *       026 = CONSULTA MOVIMENTO CONSOLIDADO
      *       027 = CONSULTA LOG
      *       028 = SOLICITACAO DE RELATORIOS
      *       029 = AUTORIZACOES ESPECIAIS
      *       030 = REATIVACAO DE PARCELAS
      *       031 = CONSULTA CONTRATO LIMITE
      *       032 = CADASTRAMENTO DE CONVENIO CLIENTE
      *       033 = CADASTRAMENTO DE CONVENIO GERAL
      *       034 = VALIDACAO DE CONVENIO CLIENTE
      *       035 = VALIDACAO DE CONVENIO GERAL
      *       036 = ADITAR CONVENIO CLIENTE
      *       037 = ADITAR CONVENIO GERAL
      *       038 = BLOQUEAR CONVENIO CLIENTE
      *       039 = BLOQUEAR CONVENIO GERAL
      *       040 = DESBLOQUEAR CONVENIO GERAL
      *       041 = DESBLOQUEAR CONVENIO GERAL
      *       042 = BLOQUEAR CONTRATO LIMITE
      *       043 = DESBLOQUEAR CONTRATO LIMITE
      *       044 = CANCELAR CONTRATO LIMITE
      *       045 = RENOVAR CONTRATO LIMITE
      *       999 = INDIFERENTE

      *********************************************************
      * AREA DE RETORNO                                       *
      *********************************************************

       01  ROTSAI-6438-RETORNO.
           05 ROTSAI-6438-COD-RET       PIC  9(04).
           05 ROTSAI-6438-MENS-RET      PIC  X(79).
           05 ROTSAI-6438-RESTART       PIC  9(05).
           05 ROTSAI-6438-FLAG          PIC  X(01).
           05 ROTSAI-6438-RESUL-RET     PIC  9(02).
      *          01-DISPONIVEL
      *          02-INDISPONIVEL
           05 ROTSAI-6438-CONSISTENCIA.
             10 ROTSAI-6438-TAMANHO-ERRO   PIC  9(01).
             10 ROTSAI-6438-TABELA-ERROS OCCURS 5 TIMES.
                15 ROTSAI-6438-CAMPOS-ERRO PIC  9(01).
