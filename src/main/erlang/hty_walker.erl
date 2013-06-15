%@doc This is a take on a walker that works not on the file system but on http resources. It is meant to replace the mechanism 
%     using fs_cursor as the rule engine for mounting resource classes.
%     This would mean that a resource instance would not have access to an Fspath but other resources, some of which may be special fs 

%resource that DOES have fs access. The important consideration is sendfile. But some module would have to populate HTX with
% a file output. A resource that wants to stream a file to client would then synt a Htx2 and send it to the underlying resource.
% then a function like hty_tx:pipe(htx()) would ask the actual tx to copy output directives from the synted one. 
% In many cases the real tx could probably just be reused also. 
-module(hty_walker).
