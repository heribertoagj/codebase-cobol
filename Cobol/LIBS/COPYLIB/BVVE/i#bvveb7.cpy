      *----------------------------------------------------------------*
      *  I#BVVEB7 - LIQUIDACAO FINANCEIRA VSAM DE LOG DE OPERACOES     *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAAMMDDHHMMSS                             *
      *----------------------------------------------------------------*

       01  REG-ASLCPGTO.
           03  BVVEB7-CHAVE.
               05  BVVEB7-NUM-LIQ          PIC X(021).
               05  BVVEB7-FILLER           PIC X(029).
           03  BVVEB7-NOME-ARQ             PIC X(035).
           03  BVVEB7-CNPJ-BASE-CREDC-X.
               05  BVVEB7-CNPJ-BASE-CREDC  PIC 9(008).
           03  BVVEB7-PERFIL               PIC 9(008).
           03  BVVEB7-NOME-CREDC           PIC X(080).
           03  BVVEB7-TP-PSSOA-PT          PIC X(001).
           03  BVVEB7-CNPJ-CPF-PT          PIC 9(014).
           03  BVVEB7-AG-CENTRALZ          PIC 9(004).
           03  BVVEB7-CT-CENTRALZ          PIC 9(007).
           03  BVVEB7-PONTO-VENDA          PIC X(025).
           03  BVVEB7-NOME-PTO-VEN         PIC X(035).
           03  BVVEB7-COD-ARRAJ-PGTO       PIC X(003).
           03  BVVEB7-IDTF-TRANSF-X.
               05  BVVEB7-IDTF-TRANSF      PIC 9(001).
           03  BVVEB7-DT-PGTO              PIC 9(008).
           03  BVVEB7-VLR-LIQD-PGTO        PIC 9(015)V99.
           03  BVVEB7-SIT-BVVE             PIC X(001).
           03  BVVEB7-DH-ENV-MULTI         PIC 9(014).
           03  BVVEB7-DH-RET-MULTI         PIC 9(014).
           03  BVVEB7-COD-ERRO-MULTI       PIC 9(002).
           03  BVVEB7-COD-RET-CIP          PIC 9(002).
           03  BVVEB7-DH-ENV-CIP           PIC 9(014).
           03  BVVEB7-SEQ-CIP              PIC 9(005).
           03  FILLER                      PIC X(252).

      *    BVVEB7-SIT-BVVE = 'C' CONSISTENTE
      *                      'I' INCONSISTENTE
      *                      'P' SEM PERFIL
