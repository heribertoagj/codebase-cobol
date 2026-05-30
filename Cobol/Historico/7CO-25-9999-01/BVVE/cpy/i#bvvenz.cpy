      *===============================================================*
      *  ARQUIVO DE GARANTIAS PARA CONTABILIZACAO DE SALDO DEVEDOR E  *
      *  SOLICITACOES/CANCELAMENTOS DE GRAVAME                        *
      *                                                               *
      *  I#BVVENZ - 700 BYTES                                         *
      *---------------------------------------------------------------*
      * CONTRATO        : INFORMAR CONTRATO LEGADO OU CONTRATO NAS    *
      *   TP-CONTRATO   : 1-LEGADO / 2-NAS                            *
      *   CONTR-LEGADO  : CONTRATO LEGADO                             *
      *   CONTR-CPSSOA  : CONTRATO NAS CPSSOA-JURID-CONTR             *
      *   CONTR-CTPO    : CONTRATO NAS CTPO-CONTR-NEGOC               *
      *   CONTR-NSEQ    : CONTRATO NAS NSEQ-CONTR-NEGOC               *
      * CONTRATO-LOTE   : CONTRATO P/ ENVIO A CIP DAS OPER EM LOTE    *
      * COD-GARANTIA    : CODIGO DE GARANTIA GRAN / CATALOGO          *
      * BANDEIRA        : BANDEIRA CORPORATIVA BCPP VISAO 2           *
      * DT-INI-GRAV     : AAAAMMDD                                    *
      * DT-FIM-GRAV     : AAAAMMDD                                    *
      * DT-FIM-CONTR    : AAAAMMDD                                    *
      * IND-OPER        : INDICA OPERACAO DE INCLUSAO, ALTERACAO,     *
      *                   CANCELAMENTO OU LIQUIDACAO (I/A/C/L)        *
      * IND-RENEG       : INDICA SE TEM CONTRATO DE RENEGOCIACAO (S/N)*
      * AUT-CESSAO-GRAV : INDICA AUTORIZ DE CESSAO SOBRE GRAVAME (S/N)*
      * AUT-CNPJ-CPF    : SE CESSAO SOBRE GRAVAME, INFORMAR CNPJ/CPF  *
      * AUT-FILIAL      : SE CESSAO SOBRE GRAVAME, INFORMAR CNPJ/CPF  *
      * AUT-CONTROLE    : SE CESSAO SOBRE GRAVAME, INFORMAR CNPJ/CPF  *
      *
      * RETORNO SITUACAO: 0-BLOQUEIO COMPLETO                         *
      *                   1-BLOQUEIO PARCIAL                          *
      *                   2-CONTRATO ENCERRADO                        *
      *                   3-REGISTRO INCONSISTENTE                    *
      *===============================================================*
      *                                                               *
       01 BVVENZ-REGISTRO.
           05 BVVENZ-CCUSTO             PIC X(004).
FS2511     05 BVVENZ-CNPJ-CPF           PIC X(009).
FS2511     05 BVVENZ-FILIAL             PIC X(004).
           05 BVVENZ-DIGITO             PIC 9(002).
           05 BVVENZ-AGENCIA            PIC 9(005).
           05 BVVENZ-CONTA              PIC 9(013).
           05 BVVENZ-AG-CONTR           PIC 9(005).
           05 BVVENZ-CT-CONTR           PIC 9(013).
           05 BVVENZ-CARTEIRA           PIC X(003).
           05 BVVENZ-CONTRATO.
              10 BVVENZ-TP-CONTRATO     PIC X(001).
              10 BVVENZ-CONTR-LEGADO    PIC 9(017).
              10 BVVENZ-CONTR-CPSSOA    PIC 9(010).
              10 BVVENZ-CONTR-CTPO      PIC 9(003).
              10 BVVENZ-CONTR-NSEQ      PIC 9(010).
           05 BVVENZ-CONTRATO-LOTE      PIC X(019).
           05 BVVENZ-COD-GARANTIA       PIC 9(003).
           05 BVVENZ-BANDEIRA           PIC 9(003).
           05 BVVENZ-DT-INI-GRAV        PIC 9(008).
           05 BVVENZ-DT-FIM-GRAV        PIC 9(008).
           05 BVVENZ-DT-FIM-CONTR       PIC 9(008).
           05 BVVENZ-VLR-TOT-CONTR      PIC 9(015)V99.
           05 BVVENZ-VLR-EXIGIDO        PIC 9(015)V99.
           05 BVVENZ-IND-OPER           PIC X(001).
           05 BVVENZ-IND-RENEG          PIC X(001).
           05 BVVENZ-AUT-CESSAO-GRAV    PIC X(001).
FS2511     05 BVVENZ-AUT-CNPJ-CPF       PIC X(009).
FS2511     05 BVVENZ-AUT-FILIAL         PIC X(004).
           05 BVVENZ-AUT-CONTROLE       PIC 9(002).
           05 BVVENZ-RETORNO.
              10 BVVENZ-SITUACAO        PIC X(001).
              10 BVVENZ-VLR-BLOQUEADO   PIC 9(015)V99.
           05 FILLER                    PIC X(482).
      *                                                               *
      *===============================================================*
