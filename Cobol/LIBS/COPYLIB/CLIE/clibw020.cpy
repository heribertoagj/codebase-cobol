      *===============================================================*
      *                    COPIA DA BOOK I#CLIB20                     *
      *                    ======================                     *
      *             PARA TRAMANENTO DO CNPJ ALFANUMERICO              *
      * TODAS AS DESCRICOES SERAO MANTIDAS PARA HISTORICO DO PROGRAMA *
      *                                                               *
      * DATA CRIACAO :  09/09/2025                                    *
      * RESPONSAVEL  :  MARCELO MORINA MARQUES NOGUEIRA - GRP.464     *
      *                                                               *
      *===============================================================*
      *---------------------------------------------------------------*
      *      CADASTRO DE CLIENTES - MODULOS MULTIBANCO                *
      *                                                               *
      *      LAYOUT DE RETORNO MODULO CLIB1001 - INC I#CLIB20         *
      *                                                               *
      *      ACESSO AS TABELAS OBJEB000, OBJB010, OBJBB000, OBJBB010  *
      *                                                               *
      *      SE O CAMPO TIPO-CONTA = C = CONTA CORRENTE               *
      *      SE O CAMPO TIPO-CONTA = P = CONTA POUPANCA               *
      *---------------------------------------------------------------*
       01  ECLIB-AREA-CLIBW020.
           03  ECLIB-TRANSACAO         PIC 9(05) COMP-3.
           03  ECLIB-VERSAO            PIC X(06).
           03  ECLIB-PRODUTO           PIC 9(05) COMP-3.
           03  ECLIB-BANCO             PIC 9(05) COMP-3.
           03  ECLIB-AGENCIA           PIC 9(05) COMP-3.
           03  ECLIB-CONTA             PIC 9(13) COMP-3.
           03  FILLER                  PIC X(95).

       01  SCLIB-AREA-CLIBW020.
           03  SCLIB-RC                PIC 9(03) COMP-3.
           03  SCLIB-TIPO-CONTA-C-P    PIC X(01).
           03  SCLIB-MENSAGEM          PIC X(997).
           03  SCLIB-OBJBB000    REDEFINES SCLIB-MENSAGEM.
               05  BANCO               PIC S9(3)V USAGE COMP-3.
               05  AGENCIA             PIC S9(5)V USAGE COMP-3.
               05  CONTA-CORRENTE      PIC S9(13)V USAGE COMP-3.
               05  NOME                PIC X(40).
               05  ENDERECO            PIC X(40).
               05  CEP                 PIC S9(5)V USAGE COMP-3.
               05  FONE                PIC S9(7)V USAGE COMP-3.
               05  SEXO                PIC X(1).
               05  DT-NASCIMENTO       PIC S9(7)V USAGE COMP-3.
               05  RAZAO               PIC S9(5)V USAGE COMP-3.
               05  RAMO                PIC S9(5)V USAGE COMP-3.
               05  TIPO-CONTA          PIC X(2).
               05  ISENCAO             PIC X(1).
               05  GRUPO               PIC S9(5)V USAGE COMP-3.
               05  CHEQUE-ESPECIAL     PIC X(1).
               05  ULTIMA-FOLHA        PIC S9(7)V USAGE COMP-3.
               05  DT-EMISSAO          PIC S9(7)V USAGE COMP-3.
               05  SERIE               PIC X(1).
               05  BLOQUEIO            PIC X(1).
               05  QTDE-FOLHA          PIC X(1).
               05  TIPO-EXTRATO        PIC X(1).
               05  POSTO               PIC S9(3)V USAGE COMP-3.
               05  DT-ABERTURA         PIC S9(7)V USAGE COMP-3.
               05  DT-ULTIMO-ACERTO    PIC S9(7)V USAGE COMP-3.
               05  CEP-NOVO-NRO        PIC S9(5)V USAGE COMP-3.
               05  CEP-NOVO-COMPL      PIC S9(3)V USAGE COMP-3.
               05  FONE-DDD            PIC S9(5)V USAGE COMP-3.
               05  FAX-DDD             PIC S9(5)V USAGE COMP-3.
               05  FAX-FONE            PIC S9(7)V USAGE COMP-3.
               05  CTPO-PSSOA          PIC X(1).
               05  CDDD                PIC X(4).
               05  CFONE               PIC S9(8)V USAGE COMP-3.
               05  CDDD-FAX            PIC X(4).
               05  CFAX                PIC S9(8)V USAGE COMP-3.
               05  DNASC               PIC X(10).
               05  DABERT-CTA          PIC X(10).
               05  DULT-ACERT          PIC X(10).
               05  DRCADT              PIC X(10).
               05  CFUNC-BDSCO         PIC S9(9)V USAGE COMP-3.
               05  NLIN-TFONI          PIC S9(11)V USAGE COMP-3.
               05  NLIN-TFONI-FAX      PIC S9(11)V USAGE COMP-3.
               05  NUM-CGCCPF-ST       PIC X(9).
               05  FILIAL-CGCCPF-ST    PIC X(4).
               05  CONTR-CGCCPF-ST     PIC X(2).
               05  FILLER              PIC X(752).

           03  SCLIB-OBJBB010    REDEFINES SCLIB-MENSAGEM.
               05  POUPADOR-BANCO      PIC S9(3)V USAGE COMP-3.
               05  POUPADOR-AGENCIA    PIC S9(5)V USAGE COMP-3.
               05  POUPADOR-CONTA      PIC S9(13)V USAGE COMP-3.
               05  POUPADOR-NOME-CLIE  PIC X(39).
               05  POUPADOR-ALERTA     PIC X(1).
               05  POUPADOR-END-CLIE   PIC X(40).
               05  POUPADOR-CEP        PIC S9(5)V USAGE COMP-3.
               05  POUPADOR-TELEFONE   PIC S9(7)V USAGE COMP-3.
               05  POUPADOR-SEXO       PIC X(1).
               05  POUPADOR-DT-NASC    PIC S9(7)V USAGE COMP-3.
               05  POUPADOR-RAZAO      PIC S9(5)V USAGE COMP-3.
               05  POUPADOR-RAMO-AT    PIC S9(5)V USAGE COMP-3.
               05  POUPADOR-TIPO-CTA   PIC X(2).
               05  POUPADOR-PESSOA     PIC X(1).
               05  POUPADOR-COD-GRUP   PIC S9(5)V USAGE COMP-3.
               05  POUPADOR-TP-TRI     PIC X(1).
               05  POUPADOR-DT-QBMES   PIC S9(7)V USAGE COMP-3.
               05  POUPADOR-PSTO-SER   PIC S9(3)V USAGE COMP-3.
               05  POUPADOR-DT-ABERT   PIC S9(7)V USAGE COMP-3.
               05  CDDD                PIC X(4).
               05  CFONE               PIC S9(8)V USAGE COMP-3.
               05  CDDD-FAX            PIC X(4).
               05  CFAX                PIC S9(8)V USAGE COMP-3.
               05  DNASC               PIC X(10).
               05  DABERT-CTA          PIC X(10).
               05  DULT-ACERT          PIC X(10).
               05  DRCADT              PIC X(10).
               05  CFUNC-BDSCO         PIC S9(9)V USAGE COMP-3.
               05  CCEP-NOVO-COMPL     PIC S9(3)V USAGE COMP-3.
               05  NLIN-TFONI          PIC S9(11)V USAGE COMP-3.
               05  NLIN-TFONI-FAX      PIC S9(11)V USAGE COMP-3.
               05  POUPADOR-CGC-ST     PIC X(9).
               05  POUPADOR-FILIAL-ST  PIC X(4).
               05  POUPADOR-CONTR-ST   PIC X(2).
               05  FILLER              PIC X(778).
